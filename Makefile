rfun := $(wildcard R/*.R)
rman := $(wildcard man/)
rtes := $(wildcard tests/testthat/*.R)
chk = record_updates.txt
rscr = pkg2date.R


all: $(chk)

$(chk): $(rfun) $(rtes) $(rman)
	Rscript --no-site-file  --no-init-file $(rscr) 1

clean:
	rm -rf man/* NAMESPACE
