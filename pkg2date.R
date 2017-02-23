# pkg2date.R
# script to update packages
# last update: 2016-11-19
# Kevin Cazelles

args <- commandArgs(trailingOnly = TRUE)

## update README / documentation
ndc <- 60
decoreq <- function(nb) paste(rep("=",nb), collapse="")
decor <- function(){
  cat("DONE", decoreq(ndc), "\n")
}

##
cat(decoreq(.5*ndc-2), " START ", decoreq(.5*ndc-2), "\n\n")
devtools::load_all(".")

if (!as.numeric(args[1])) {
  ## README
  rmarkdown::render("README.Rmd", "all", quiet=TRUE)
  ##
  cat(decoreq(.5*ndc-7), " README UPDATED ", decoreq(.5*ndc-6), "\n\n")
} else {
  if (as.numeric(args[1])<2){
    ## format the code
    cat("##-- tidying ........\n")
    formatR::tidy_dir("./R",arrow = getOption("formatR.arrow",TRUE))
    formatR::tidy_dir("./tests/test_that", arrow = getOption("formatR.arrow",TRUE))
    decor()
    ## load the package
    cat("##-- loading ........\n")
    devtools::load_all(".")
    decor()
    ## document the package
    cat("##-- documenting ....\n")
    devtools::document(".")
    decor()
    ## testing the code
    cat("##-- testing ........\n")
    devtools::test()
    decor()
  } else{
    ## building the vgnettes
    if (dir.exists("vignettes")){
      cat("##-- vignettes ......\n")
      devtools::build_vignettes(".")
    }
  }
  ## recording update
  cat(date(), "   DONE \n", file = "record_updates.txt", append=TRUE)
}

decoreq <- function(nb) paste(rep("=",nb), collapse="")

cat(decoreq(.5*ndc-1), " END ", decoreq(.5*ndc-1), "\n\n")
