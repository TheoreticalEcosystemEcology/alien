rfun := $(wildcard R/*.R)
rman := $(wildcard man/)
rcpp := $(wildcard src/*.cpp)
rtes := $(wildcard tests/testthat/*.R)
chk = record_updates.txt
rscr = pkg2date.R


all: $(md) $(chk)

$(chk): $(rfun) $(rtes) $(rman) $(rcpp)
	Rscript --no-site-file --no-init-file $(rscr) 1

check:
	Rscript --no-init-file -e "devtools::check('.')"

vignette:
	Rscript --no-init-file $(rscr) 2

clean:
	rm -rf man/* src/*.o src/*.so R/RcppExports.R src/RcppExports.cpp
