GENERATED = gauche-refe.texi gauche-refj.texi
GOSH = gosh

all:

install:

texi : gauche-refe.texi gauche-refj.texi

html : gauche-refe.html gauche-refj.html

htmls : gauche-refe_toc.html gauche-refj_toc.html

dvi : gauche-refe.dvi

pdf : gauche-refe.pdf

gauche-refe.html : gauche-refe.texi
	texi2html -number gauche-refe.texi

gauche-refe_toc.html : gauche-refe.texi
	texi2html -split section -number gauche-refe.texi

gauche-refe.dvi : gauche-refe.texi
	texi2dvi gauche-refe.texi

gauche-refe.pdf : gauche-refe.texi
	texi2pdf gauche-refe.texi

gauche-refe.texi : gauche-ref.texi extract
	$(GOSH) extract -en -o gauche-refe.texi gauche-ref.texi

gauche-refj.html : gauche-refj.texi
	texi2html -number gauche-refj.texi

gauche-refj_toc.html : gauche-refj.texi
	texi2html -split section -number gauche-refj.texi

gauche-refj.dvi : gauche-refj.texi
	texi2dvi gauche-refj.texi

gauche-refj.pdf : gauche-refj.texi
	texi2pdf gauche-refj.texi

gauche-refj.texi : gauche-ref.texi extract
	$(GOSH) extract -jp -o gauche-refj.texi gauche-ref.texi

clean:
	rm -rf core *~ *.aux *.cl *.cls *.cp *.fn *.fns *.ky *.log *.md *.mds \
	       *.pg *.toc *.tp *.tps *.vr *.vrs *.pdf *.dvi

distclean : clean
	rm -rf $(GENERATED)

realclean : clean
	rm -f gauche-ref*.html gauche-ref.dvi $(GENERATED)


