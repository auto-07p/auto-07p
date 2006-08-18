#
#   Makefile		11/1996
#
RM     = rm -f
#
all:  manual
#
manual: auto.ps
#
auto.ps:auto.tex
	latex auto.tex
	bibtex auto
	latex auto.tex
	dvips auto -o auto.ps
	gv auto.ps &
#
clean:
	@echo "Cleaning auto.doc ..."
	@$(RM) *.dvi *.ps 
	@echo "Cleaning ... done"
