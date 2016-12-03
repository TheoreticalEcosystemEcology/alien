Status
------

**Early development** **Version**: 0.0.1

[![Travis](https://travis-ci.org/TheoreticalEcosystemEcology/alienR.svg?branch=master)](https://travis-ci.org/TheoreticalEcosystemEcology/alienR)
[![Build
status](https://ci.appveyor.com/api/projects/status/sk3sbvusvcyy0at0?svg=true)](https://ci.appveyor.com/project/TheoreticalEcosystemEcology/alienR/build/1.0.7)
[![codecov](https://codecov.io/gh/TheoreticalEcosystemEcology/alienR/branch/master/graphs/badge.svg)](https://codecov.io/gh/KevCaz/recruitR)

ALIEN: The aim of the package is to predict All Links In Ecological Networks
----------------------------------------------------------------------------

Incoming content
----------------

### Methods implemented

-   Direct matching centrality (DG)
    -   binomial
    -   poisson
    -   RF
-   Indirect matching centrality (latent \~ traits) (DG)
-   Niche binary (DG)
-   Niche probabilistic (DG)
-   iEat recommend (DB)
-   iEat gap-filling (DB)  
-   JSDM (GB)
-   Harris (KC)
-   Markov logic (A.T.?)
-   4th corner (GB)
-   Ben's magic (BW)
-   Ives' phylo (GB)

**Deadline:** Package ready by Christmas

Generate package metadata
-------------------------

All enhancements to the README has to be written in the Readme.Rmd file.
To build the metadata files (i.e. README, NAMESPACE, DESCRIPTION):

    ## Install packages
    pkgs = c('devtools','roxygen2','testthat','formatR','rmarkdown')
    for(pkg in pkgs) if(!require(pkg))

    ## Make sure you're working directory is set up correctly
    getwd()

    ## Create README file
    rmarkdown::render("README.Rmd", "all", quiet=TRUE)

    ## Format the R code (e.g. indentation, assignment, etc.)
    formatR::tidy_dir("./R",arrow = getOption("formatR.arrow",TRUE))
    formatR::tidy_dir("./tests/test_that", arrow = getOption("formatR.arrow",TRUE))

    ## Load your current version of the package
    devtools::load_all(".")

    ## Create man pages
    devtools::document(".")

    ## Testing the code with testthat package
    devtools::test()

### Only for the developers on Mac OSX and linux

All of the previous steps can be done with:

    cd path/to/alienR
    make

Contribute to this package
--------------------------

### Development pratices

-   the `master` branch is protected -- code can only arrive in it after
    a pull request, that has to be reviewed and approved by Steve
-   everyone work on branches in this repo
-   `git fetch` / `git pull` before you do anything else
-   new branches must be made from `master` -- if not, you are
    responsible for rebasing
-   good commit messages are 72 chars on the first line and explain what
    has been done in the imperative tone. A good rule of thumb is that
    if you say "If merged, this commit will" before the commit message,
    it should be a sentence. A good commit message is:
    `add informations about development practices`. A bad commit message
    is `CONTRIBUTING changes`
-   if the branching complexity of your function is \> 3 (number of
    nested for / if / while), rewrite

### Styleguide

-   function names are explicit and with `CamelCased` verbs, *e.g.*
    `SimulatesNicheModel`.
-   objects are declared with lowercase, underscores and small caps,
    *e.g.* `trophic_level`
-   `<-` is used for assignment
-   default values in function declarations have spaces around the `=`
    (*e.g.* `arg = var`)
-   namespaces with `::` (e.g `reshape2::acast`)
-   Packages dependancies are declared in the `DESCRIPTION` (import
    section)
-   all `roxygen2` flags (e.g. @param etc.) are to be set up.
-   R extension file has to be written in capital letter (e.g
    `fitBayesReg.R`)

Have a look at
[<http://adv-r.had.co.nz/Style.html>](http://adv-r.had.co.nz/Style.html)
For further details on namespaces:
[<http://r-pkgs.had.co.nz/namespace.html>](http://r-pkgs.had.co.nz/namespace.html)
