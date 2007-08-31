#
#   Makefile		11/1996
#
.SUFFIXES: .fig .eps
RM     = rm -f
PLAUT04_FIGS = include/2component.eps include/centerMenu.eps \
include/clrLBMu0.eps include/clrTimeMu0.eps include/clrTyMu0.eps \
include/drawCoordMenu.eps include/emL1Sol.eps include/exit.eps \
include/floquet.eps include/helpMenu.eps include/menubar.eps \
include/optionMenu.eps include/styleMenuNew.eps include/typeMenu.eps
#
all:  manual
#
manual: auto.pdf ../plaut04/doc/userguide.pdf
	-rm -f $(PLAUT04_FIGS)
	gv auto.pdf &
#
auto.pdf:auto.tex $(PLAUT04_FIGS)
	latex auto.tex
	bibtex auto
	latex auto.tex
	latex auto.tex
	latex auto.tex
	dvips -Ppdf -G0 -o auto.ps auto.dvi
	ps2pdf auto.ps auto.pdf
#

.fig.eps:
	fig2dev -Leps $< $@

../plaut04/doc/userguide.pdf: user_guide.tex $(PLAUT04_FIGS)
	latex user_guide.tex
	latex user_guide.tex
	dvipdfm -o $@ user_guide.dvi
#
clean:
	@echo "Cleaning auto.doc ..."
	@$(RM) *.dvi *.aux *.log *.bbl *.blg *.toc
	@echo "Cleaning ... done"
#
superclean: clean
	@$(RM) auto.pdf auto.ps

#
manualclean: auto.pdf clean
