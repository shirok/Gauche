MORE_CONTENT = Gauche/COPYING.rtf \
	       Gauche/gauche-logo.ico

all: stamp.0

stamp.0 : genwxs.scm Gauche $(MORE_CONTENT)
	rm -f gauche.wxs *.msi
	../../src/gosh -ftest ./genwxs.scm gauche.wxs
	candle gauche.wxs
	light -ext WixUIExtension -sice:ICE61 gauche.wixobj
	mv gauche.msi Gauche-mingw-`Gauche/bin/gauche-config -V`.msi
	touch stamp.0

Gauche/COPYING.rtf : txt2rtf.scm
	../../src/gosh -ftest ./txt2rtf.scm ../../COPYING Gauche/COPYING.rtf

Gauche/gauche-logo.ico : gauche-logo.ico
	cp gauche-logo.ico Gauche/

clean:
	rm -f stamp.0 *.wxs *.msi *.wixobj *.wixpdb *.log *~
