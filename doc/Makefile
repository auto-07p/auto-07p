#
#   Makefile		11/1996
#
RM     = rm -f
#
all:  manual
#
manual: auto.ps
	gv auto.ps &
#
auto.ps:auto.tex
	latex auto.tex
	bibtex auto
	latex auto.tex
	latex auto.tex
	latex auto.tex
	dvips auto -o auto.ps
#
clean:
	@echo "Cleaning auto.doc ..."
	@$(RM) *.dvi *.aux *.log *.bbl *.blg *.toc
	@echo "Cleaning ... done"
#
superclean: clean
	@$(RM) auto.ps
#
manualclean: auto.ps clean
