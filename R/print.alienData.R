#' @export
print.alienData <- function(x, ...){
  # Node information
  if(all(x$node$idInd == x$node$idSp)){
    print(paste(length(unique(x$node$idSp)),"Species"))
  }else{
    print(paste(length(x$node$idInd),"Individuals"))
    print(paste(length(unique(x$node$idSp)),"Species"))
  }

  print("-----------")
  
  # Edges information
  if(x$info$directed){
    print(paste(nrow(x$edge),"directed edges"))
  }else{
    print(paste(nrow(x$edge),"undirected edges"))
  }
  
  print("-----------")
  
  # Traits information
  if(is.null(x$trait)){
    print("No traits")
  }else{
    print(paste(length(unique(x$trait$trait)),"traits"))
  }
  
  print("-----------")
  
  # Phylogeny information
  if(is.null(x$phylo)){
    print("No phylogeny")
  }else{
    print("Phylogenies are available")
  }
}
