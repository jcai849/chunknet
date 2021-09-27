.SUFFIXES: .html .Rmd
package: /usr/local/lib/R/library/largerscale/
/usr/local/lib/R/library/largerscale/: R/* NAMESPACE
	R CMD INSTALL .
rmd: demo/general.html demo/recover.html
.Rmd.html:
	Rscript -e "rmarkdown::render('$<', 'html_document')"
	cp "$@" ~/Downloads
all: package rmd
clean:
	rm demo/*.html
