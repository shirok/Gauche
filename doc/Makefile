all:

install:

html : gauche-ref.html

dvi : gauche-ref.dvi

pdf : gauche-ref.pdf

gauche-ref.html : gauche-ref.texi
	texi2html -number gauche-ref.texi

gauche-ref.dvi : gauche-ref.texi
	texi2dvi gauche-ref.texi

gauche-ref.pdf : gauche-ref.texi
	texi2pdf gauche-ref.texi

clean:
	rm -rf core *~ *.aux *.cl *.cls *.cp *.fn *.fns *.ky *.log *.md *.mds \
	       *.pg *.toc *.tp *.tps *.vr *.vrs

distclean : clean


realclean : clean
	rm -f gauche-ref*.html gauche-ref.dvi

