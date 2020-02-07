#' @export
print.alienData <- function(x, ...){
  # Node information
  if(all(x$node$idInd == x$node$idSp)){
    cat(length(unique(x$node$idSp)), "Species\n")
  }else{
    cat_info(length(x$node$idInd), "Individuals")
    cat_info(length(unique(x$node$idSp)), "Species")
  }

  cat_sep()
  
  # Edges information
  if(x$info$directed){
    cat_info(nrow(x$edge), "directed edges")
  }else{
    cat_info(nrow(x$edge), "undirected edges")
  }
  
  cat_sep()
  
  # Traits information
  if(is.null(x$trait)){
    cat_info("No traits")
  }else{
    cat_info(length(unique(x$trait$trait)), "traits")
  }
  
  cat_sep()
  
  # Phylogeny information
  if(is.null(x$phylo)){
    cat_info("No phylogeny")
  }else{
    cat_info("Phylogenies are available")
  }
}
