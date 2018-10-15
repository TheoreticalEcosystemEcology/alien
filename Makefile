rfun := $(wildcard R/*.R)
rman := $(wildcard man/)
rcpp := $(wildcard src/*.cpp)
rtes := $(wildcard tests/testthat/*.R)
chk = record_updates.txt
rscr = ../pkg2date.R


all: $(md) $(chk)

$(chk): $(rfun) $(rtes) $(rman) $(rcpp)
	Rscript --no-init-file $(rscr) 1

readme:
	Rscript --no-init-file $(rscr) 0

check:
	Rscript --no-init-file -e "devtools::check('.')"

vignet:
	Rscript --no-init-file -e "devtools::load_all(); devtools::build_vignettes('.')"

formatR:
	Rscript -e 'formatR::tidy_dir("./R", width.cutoff = 70)'
	Rscript -e 'formatR::tidy_dir("./tests/test_that", width.cutoff = 70)'

paper:
	 Rscript -e "render('paper/paper.md', 'pdf_document')"

winbuild:
	Rscript --no-init-file -e "devtools::build_win()"

goodpractice:
	Rscript --no-init-file -e "goodpractice::gp()"

website:
	Rscript --no-init-file -e "pkgdown::build_site()"

everything:
	Rscript --no-init-file $(rscr) 2

clean:
	rm -rf man/* src/*.o src/*.so R/RcppExports.R src/RcppExports.cpp
