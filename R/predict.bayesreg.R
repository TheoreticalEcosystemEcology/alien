#' @name predict.bayesreg
#' @title Predict species interaction probabilities based on a fitted hierarchical bayesian regression
#' @description A helper functional for generating the probability of interaction among hypothetical species based on their traits.
#' The trait-matching function is currently |Trait_i - Trait_j|
#' @param x A JAGS model file return from fit.bayesreg
#' @param newdata A data frame with columns 'I','J' and 'Traitmatch'
#' @details The newdata object gives the trait-matching between species I (eg. pollinator) and species J (eg. plant)
#' If fit.bayesreg was fit to an intercept model, newdata is ignored, not possible to create a prediction of a new species. In this case, it is assumed that the user wants the probability matrix for all input species.
#' @return A vector of probability of interaction values.
#' I: Species in the higher level (eg. pollinators)
#' J: Species in the lower level (eg. plants)
#' mean: Mean probability of interaction
#' lower: 0.05 quantile of the probability of interaction
#' upper: 0.95 quantile of the probability of interaction
#' @author 
#' Ben Weinstein
#' @rdname predict.bayesreg
#' @importFrom magrittr %>%
#' @export
predict.bayesreg <- function(x, newdata = NULL) {

    parsm1 <- extract.bayesreg(x)

    if (x$Algorithm == "Intercept") {
        # get intercepts
        alphas <- parsm1 %>% dplyr::filter(par %in% c("alpha")) %>% dplyr::group_by(Draw, 
            Chain)

        # label species
        alphas$I <- as.numeric(stringr::str_match(alphas$parameter, pattern = "\\[(\\d+),(\\d+)]")[,
            2])
        alphas$J <- as.numeric(stringr::str_match(alphas$parameter, pattern = "\\[(\\d+),(\\d+)]")[,
            3])

        # return matrix as probability
        df <- alphas %>% dplyr::group_by(I, J) %>% dplyr::mutate(value = boot::inv.logit(estimate)) %>%
            dplyr::select(Draw, Chain, I, J, value)

        # merge with input data
        df$I <- levels(factor(dat$I))[df$I]
        df$J <- levels(factor(dat$J))[df$J]
    }

    # matching function
    if (x$Algorithm == "Binomial") {
        # default is predicting the observed data frame
        if (is.null(newdata)) {
            newdata <- x$data %>% dplyr::select(I, J, Traitmatch) %>% dplyr::distinct()
        }

        # Trait-matching functions
        predfun <- function(alpha, beta, newdata) {
            data.frame(newdata, value = boot::inv.logit(alpha + beta * newdata[["Traitmatch"]]))
        }
        #
        df <- parsm1 %>% dplyr::filter(par %in% c("alpha_mu", "beta_mu")) %>% dplyr::select(Draw,
            Chain, par, estimate) %>% reshape2::dcast(., Draw + Chain ~ par, value.var = "estimate") %>%
            dplyr::group_by(Draw, Chain) %>% dplyr::do((predfun(alpha = .$alpha_mu,
            beta = .$beta_mu, newdata))) %>% dplyr::group_by(I, J) %>% dplyr::inner_join(newdata)
    }
    
    if (x$Algorithm == "Multinomial") {
      # get intercepts
      alphas <- parsm1 %>% dplyr::filter(par %in% c("p"))
      
      #label and merge pairwise interactions 
      classes<-x$classes
      
      #number the rows
      classes$Index<-1:nrow(classes)
      
      #merge with data
      df<-classes %>% select(I,J,Index) %>% inner_join(alphas) %>% select(-par) %>% mutate(value=estimate) %>% select(-estimate)
      }
    
    return(df)
}
