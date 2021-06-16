#' @title Construct second degree polynomial of traits
#'
#' @description Construct second degree orthogonal polynomial of numerical 
#' traits while keeping the factors and binary traits as is.
#'
#' @param data An object of class \code{\link{alienData}}.
#' 
#' @details 
#' 
#' This function calculates the second degree polynomials of each variable 
#' independently. The second degree polynomial of each variable are orthogonal 
#' to one another.
#' 
#' In the process of calculating the second degree polynomials of each variable,
#' the resulting two variables were rescaled by multiplying each value by the 
#' square root of the number of species (observations). The calculation will 
#' result in the non-squared variable to present slightly different values from 
#' the values of the original variable (even after rescaling). This is expected 
#' and the non-squared variable still presents the same information.
#' 
#' The function will automatically transform all binary (0-1) variable to a 
#' factor.
#' 
#' @return 
#' 
#' An object of class \code{\link{alienData}} with all numerical traits squared. 
#' The squared variables all end with "_Sq".
#' 
#' @export
#' 
polyTrait <- function(data){
  
  # Build data object
  traits <- vector("list", length = 2)
  names(traits) <- c("from", "to")
  
  traits$from <- data$traitFrom
  traits$to <- data$traitTo
  
  for(j in 1:2){
    # Check NA
    if(any(is.na(traits[[j]]))){
      stop("There should not be any NAs in the traits")
    }
    
    # Basic object
    nRows <- nrow(traits[[j]])
    nCols <- ncol(traits[[j]])
    
    #====================
    # Check variable type
    #====================
    varType <- sapply(traits[[j]], class)
    
    #====================================
    # Convert binary variable to a factor
    #====================================
    varInteger <- which(varType == "integer")
    for(i in varInteger){
      # Find unique values
      varUniqueVal <- unique(traits[[j]][,i])
      if(all(varUniqueVal %in% c(0,1))){
        # Convert to factor
        traits[[j]][,i] <- as.factor(traits[[j]][,i])
        varType[i] <- "factor"
      }
    }
    
    #====================================
    # Convert integer to numeric for ease
    #====================================
    varType[which(varType == "integer")] <- "numeric"
    
    # Build result object
    res <- data.frame(NA)
    resName <- character()
    
    #================================================================
    # Calculate second order polynomial of each quantitative variable
    #================================================================
    for(i in 1:nCols){
      if(is.factor(traits[[j]][,i])){
        res <- cbind(res, traits[[j]][,i])
        resName <- c(resName, colnames(traits[[j]])[i])
      }else{
        res <- cbind(res,poly(traits[[j]][,i], degree = 2) * sqrt(nRows)) # sqrt(nRows) is for Rescaling
        resName <- c(resName, colnames(traits[[j]])[i], paste0(colnames(traits[[j]])[i],"_Sq"))
      }
    }
    
    # Add names and remove bogus NA variable
    res <- res[,-1]
    colnames(res) <- resName
    
    #===============================================
    # Convert all binary factors to a 1, -1 variable
    #===============================================
    facPointer <- which(varType == "factor")
    facLength <- lapply(lapply(res[,facPointer],levels),length)
    
    for(i in 1:length(facLength)){
      if(facLength[i] == 2){
        fac <- res[,facPointer[i]]
        res[,facPointer[i]] <- model.matrix(~fac,
                                                    contrasts = list(fac = "contr.sum"))[,-1]
      }
    }
    
    # Replace old traits with new ones
    if(j == 1){
      data$traitFrom <- res
    }
    if(j == 2){
      data$traitTo <- res
    }
  }
  
  # Return results
  return(data)
}

