#
#   Makefile		11/1996
#
RM     = rm -f
#
all:  manual
#
manual: auto.tex
	latex auto.tex
#	bibtex auto
	latex auto.tex
	dvips auto -o auto.ps
	ghostview auto.ps &
#
clean:
	@echo "Cleaning auto.doc ..."
	@$(RM) *.dvi *.ps 
	@echo "Cleaning ... done"
