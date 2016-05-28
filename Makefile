# This is to make the stupid expansion in clean work.
SHELL=/bin/bash

DOC=type_algebra

#######################################################################
#                               Targets                               #
#######################################################################
.PHONY: clean

pdf: $(DOC).pdf

$(DOC).pdf: $(DOC).tex
	pdflatex $(DOC).tex

$(DOC).tex: $(DOC).lhs
	lhs2TeX --poly -o $(DOC).tex $(DOC).lhs

clean:
	-rm -r $(DOC).{tex,aux,log,nav,out,ptb,snm,toc}
