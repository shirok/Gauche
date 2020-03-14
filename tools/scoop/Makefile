#

TARGET = gauche.json

all: $(TARGET)

.PHONY: clean

$(TARGET): make-scoop-manifesto.scm
	gosh $< --version=0.9.9 --msi64=Gauche-mingw-0.9.9-64bit.msi --msi32=Gauche-mingw-0.9.9-32bit.msi > $@
	emacs --batch --file=$@ --load=json --funcall=json-pretty-print-buffer --funcall=save-buffer

clean:
	rm -f $(TARGET)
