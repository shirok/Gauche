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

COPYING=../../COPYING

all: stamp$(ARCH_SUFFIX).0

stamp$(ARCH_SUFFIX).0 : genwxs.scm Gauche $(MORE_CONTENT)
	rm -f gauche$(ARCH_SUFFIX).wxs $(INSTALLER_FILENAME)
	../../src/gosh -ftest ./genwxs.scm $(GENWXS_OPTION) gauche$(ARCH_SUFFIX).wxs
	candle $(CANDLE_OPTION) gauche$(ARCH_SUFFIX).wxs
	light -ext WixUIExtension -sice:ICE61 gauche$(ARCH_SUFFIX).wixobj
	mv gauche$(ARCH_SUFFIX).msi $(INSTALLER_FILENAME)
	touch stamp$(ARCH_SUFFIX).0

# We must include LGPL for libiconv.dll, so some tweaks...

Gauche/COPYING.rtf : txt2rtf.scm $(COPYING)
	cat intro-COPYING.txt > copying.tmp
	sed '1,/^$$/ d' $(COPYING) >> copying.tmp
	cat libiconv-COPYING.LIB >> copying.tmp
	cat libwinpthreads-COPYING.txt >> copying.tmp
	cat mbedtls-COPYING.txt >> copying.tmp
	../../src/gosh -ftest ./txt2rtf.scm copying.tmp Gauche/COPYING.rtf

Gauche/gauche-logo.ico : gauche-logo.ico
	cp gauche-logo.ico Gauche/

clean:
	rm -f stamp*.0 *.tmp *.wxs *.msi *.wixobj *.wixpdb *.log *~
	rm -rf Gauche/
