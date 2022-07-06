.PHONY: tags install test
all: install test
install: /usr/local/lib/R/library/largerscale/
/usr/local/lib/R/library/largerscale/: R/* NAMESPACE DESCRIPTION
	R CMD INSTALL .
tags:
	uctags -R 
test:
	cd tests && tmux new-session \; source-file test.tmux
