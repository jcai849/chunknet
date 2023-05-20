.PHONY: tags install test
all: install
install: /usr/local/lib/R/library/chunknet/
/usr/local/lib/R/library/chunknet/: R/* NAMESPACE DESCRIPTION
	R CMD INSTALL .
tags:
	uctags -R 
test:
	cd inst/dev-tests && tmux new-session \; source-file test.tmux
