#
#   Makefile		11/1996
#
.SUFFIXES: .fig .ps .eps .pdf
RM     = rm -f
I = include/

FIGS = $(I)ab1.pdf $(I)ab2.pdf $(I)ab3.pdf $(I)cir1.pdf $(I)cir2.pdf \
$(I)kpr10.pdf $(I)kpr1.pdf $(I)kpr2.pdf $(I)kpr4.pdf $(I)kpr5.pdf $(I)kpr6.pdf \
$(I)kpr7.pdf $(I)kpr8.pdf $(I)mtn1.pdf $(I)mtn2.pdf $(I)mtn3.pdf $(I)rev1.pdf \
$(I)rev2.pdf $(I)rev3.pdf $(I)rev4.pdf $(I)san1.pdf $(I)san2.pdf $(I)she1.pdf \
$(I)broken.pdf $(I)cb_snh.pdf $(I)cb_snh_par.pdf $(I)closegap_pcl.pdf \
$(I)closegap_pcl_par.pdf $(I)closegap_snh.pdf $(I)closegap_snh_par.pdf \
$(I)cont_pcl.pdf $(I)cont_pcl_par.pdf $(I)cont_snh_all.pdf \
$(I)cont_snh_all_par.pdf $(I)cusp1.pdf $(I)cusp2.pdf $(I)fnc_attrrep1.pdf \
$(I)fnc_attrrep2.pdf $(I)fnc_canards1.pdf $(I)fnc_canards2.pdf \
$(I)fnc_canards3.pdf $(I)fnc_canards4.pdf $(I)fnc_canards5.pdf $(I)hom2.pdf \
$(I)hom3.pdf $(I)hopfbif.pdf $(I)kdv3hom.pdf $(I)loop.pdf $(I)notshifted.pdf \
$(I)parspace.pdf $(I)pp21.pdf $(I)pp22.pdf $(I)shilgap.pdf

PLAUT04_FIGS = $(I)2component.pdf $(I)centerMenu.pdf \
$(I)clrLBMu0.pdf $(I)clrTimeMu0.pdf $(I)clrTyMu0.pdf \
$(I)drawCoordMenu.pdf $(I)emL1Sol.pdf $(I)exit.pdf \
$(I)floquet.pdf $(I)helpMenu.pdf $(I)menubar.pdf \
$(I)optionMenu.pdf $(I)styleMenuNew.pdf $(I)typeMenu.pdf

all:  manual

manual: auto.pdf ../plaut04/doc/userguide.pdf
	../cmds/@mn

auto.pdf:auto.tex $(FIGS) $(PLAUT04_FIGS)
	pdflatex auto.tex
	bibtex auto
	pdflatex auto.tex
	pdflatex auto.tex
	pdflatex auto.tex

.fig.pdf:
	fig2dev -Lpdf $< $@

.ps.pdf:
	perl ./epstopdf.pl $<

.eps.pdf:
	perl ./epstopdf.pl $<

../plaut04/doc/userguide.pdf: user_guide.tex $(PLAUT04_FIGS)
	pdflatex user_guide.tex
	pdflatex user_guide.tex
	mv user_guide.pdf $@

clean:
	@echo "Cleaning auto.doc ..."
	@$(RM) *.aux *.log *.bbl *.blg *.toc *.out
	@$(RM) $(FIGS) $(PLAUT04_FIGS)
	@for i in include/*.eps; do $(RM) `echo $$i | sed -e "s/\.eps/.pdf/"`; done
	@for i in include/*.ps; do $(RM) `echo $$i | sed -e "s/\.ps/.pdf/"`; done
	@echo "Cleaning ... done"

superclean: clean
	@$(RM) auto.pdf

manualclean: auto.pdf clean
