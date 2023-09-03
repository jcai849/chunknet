.PHONY: tags install test
all: install
install: /usr/local/lib/R/library/largescalechunks/
/usr/local/lib/R/library/largescalechunks/: R/* NAMESPACE DESCRIPTION
	R CMD INSTALL .
tags:
	uctags -R 
test:
	cd inst/dev-tests && tmux new-session \; source-file test.tmux
