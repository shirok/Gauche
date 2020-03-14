#

VERSION = 0.9.9
MSI64   = Gauche-mingw-0.9.9-64bit.msi
MSI32   = Gauche-mingw-0.9.9-32bit.msi

TARGET = gauche.json

all: $(TARGET)

.PHONY: clean

$(TARGET): make-scoop-manifesto.scm
	gosh $< --version=$(VERSION) --msi64=$(MSI64) --msi32=$(MSI32) > $@
	emacs --batch --file=$@ --load=json --funcall=json-pretty-print-buffer --funcall=save-buffer

clean:
	rm -f $(TARGET)
