all:

install:

html : gauche-ref.html

dvi : gauche-ref.dvi

gauche-ref.html : gauche-ref.texi
	texi2html -monolithic -number gauche-ref.texi

gauche-ref.dvi : gauche-ref.texi
	texi2dvi gauche-ref.texi

clean:
	rm -rf core *~ *.aux *.cl *.cls *.cp *.fn *.fns *.ky *.log *.mo *.pg \
	       *.toc *.tp *.tps *.vr *.vrs

distclean : clean


realclean : clean
