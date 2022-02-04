.PHONY: all clean tag
.SUFFIXES: .html .Rmd
package: /usr/local/lib/R/library/largerscale/
/usr/local/lib/R/library/largerscale/: R/* NAMESPACE DESCRIPTION
	R CMD INSTALL .
rmd:
.Rmd.html:
	Rscript -e "rmarkdown::render('$<', 'html_document')"
	cp "$@" ~/Downloads
all: package rmd
clean:
	rm demo/*.html
tag:
	uctags R/*
