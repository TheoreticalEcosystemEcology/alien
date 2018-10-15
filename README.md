Status
------

**Version**: 0.1

[![Build Status](https://travis-ci.org/TheoreticalEcosystemEcology/alien.svg?branch=master)](https://travis-ci.org/TheoreticalEcosystemEcology/alien)
[![Build status](https://ci.appveyor.com/api/projects/status/al10lmlky7se4wa6/branch/master?svg=true)](https://ci.appveyor.com/project/SteveViss/alien/branch/master) [![codecov](https://codecov.io/gh/TheoreticalEcosystemEcology/alien/branch/master/graph/badge.svg)](https://codecov.io/gh/TheoreticalEcosystemEcology/alien)


ALIEN: The aim of the package is to predict All Links In Ecological Networks
----------------------------------------------------------------------------

Incoming content
----------------


### Methods

#### Predictions

-  [X] Direct matching centrality (DG)
-  [X] Indirect matching centrality (latent ~ traits)
-  [ ] 4th corner (Brown et al. MEE 2014) (GB)
-  [ ] JSDM (GB)
-  [ ] Niche binary (DG)
-  [ ] Niche probabilistic (DG)
-  [ ] KNN as a minimal iEat (KC)



##### To be implemented

-  [ ] Markov logic (A.T.? => guy in the UK)
-  [ ] iEat's similarity and data retrieving in a specific package (DB)
-  [ ] Ben's magic (BW)
-  [ ] Permutation methods (GB)
    -   C-scores
    -   V-ratio
-  [ ] Multinomial (somebody?)
- Simulate communities data
-  [ ] Ives' phylo (2006, Amnat see package `picante`) (GB)

Generate package metadata
-------------------------


To build the metadata files (i.e. README, NAMESPACE, DESCRIPTION):

``` r
ipak <- function(pkg){
    new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
    if (length(new.pkg))
        install.packages(new.pkg, dependencies = TRUE)
    sapply(pkg, require, character.only = TRUE)
}

## Install packages
pkgs = c('devtools','roxygen2','testthat','formatR','rmarkdown')
ipak(pkgs)

## Make sure you're working directory is set at path/to/alien/folder
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
```

### Only for the developers on Mac OSX and linux

All of the previous steps can be done with:

``` bash
cd path/to/alien
make
```

Contribute to this package
--------------------------

### Development pratices

-   the `master` branch is protected -- code can only arrive in it after a pull request, that has to be reviewed and approved by Steve
-   everyone work on branches in this repo
-   `git fetch` / `git pull` before you do anything else
-   new branches must be made from `master` -- if not, you are responsible for rebasing
-   good commit messages are 72 chars on the first line and explain what has been done in the imperative tone. A good rule of thumb is that if you say "If merged, this commit will" before the commit message, it should be a sentence. A good commit message is: `add informations about development practices`. A bad commit message is `CONTRIBUTING changes`
-   if the branching complexity of your function is &gt; 3 (number of nested for / if / while), rewrite

### Styleguide

-   function names are explicit and with `CamelCased` verbs, *e.g.* `SimulatesNicheModel`.
-   objects are declared with lowercase, underscores and small caps, *e.g.* `trophic_level`
-   namespaces with `::` (e.g `reshape2::acast`)
-   Packages dependancies are declared in the `DESCRIPTION` (import section)
-   all `roxygen2` flags (e.g. @param etc.) are to be set up.
-   R extension file has to be written in capital letter (e.g `fitBayesReg.R`)

Have a look at <http://adv-r.had.co.nz/Style.html>

For further details on namespaces: <http://r-pkgs.had.co.nz/namespace.html>

License
-------

Copyright (c) 2016 IELab research group

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
