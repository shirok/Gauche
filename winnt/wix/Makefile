MORE_CONTENT = Gauche/COPYING.rtf \
	       Gauche/gauche-logo.ico

ifeq ($(MSYSTEM),MINGW64)
  ARCH_SUFFIX=-64bit
  GENWXS_OPTION=--arch=x64
  CANDLE_OPTION=-arch x64
else ifeq ($(MSYSTEM),MINGW32)
  ARCH_SUFFIX=-32bit
  GENWXS_OPTION=
  CANDLE_OPTION=
else
  ARCH_SUFFIX=
  GENWXS_OPTION=
  CANDLE_OPTION=
endif

INSTALLER_FILENAME=Gauche-mingw-`Gauche/bin/gauche-config -V`$(ARCH_SUFFIX).msi

all: stamp$(ARCH_SUFFIX).0

stamp$(ARCH_SUFFIX).0 : genwxs.scm Gauche $(MORE_CONTENT)
	rm -f gauche$(ARCH_SUFFIX).wxs $(INSTALLER_FILENAME)
	../../src/gosh -ftest ./genwxs.scm $(GENWXS_OPTION) gauche$(ARCH_SUFFIX).wxs
	candle $(CANDLE_OPTION) gauche$(ARCH_SUFFIX).wxs
	light -ext WixUIExtension -sice:ICE61 gauche$(ARCH_SUFFIX).wixobj
	mv gauche$(ARCH_SUFFIX).msi $(INSTALLER_FILENAME)
	touch stamp$(ARCH_SUFFIX).0

Gauche/COPYING.rtf : txt2rtf.scm
	../../src/gosh -ftest ./txt2rtf.scm ../../COPYING Gauche/COPYING.rtf

Gauche/gauche-logo.ico : gauche-logo.ico
	cp gauche-logo.ico Gauche/

clean:
	rm -f stamp*.0 *.wxs *.msi *.wixobj *.wixpdb *.log *~
