#

TARGET = gauche.json

all: $(TARGET)

.PHONY: clean

$(TARGET): make-scoop-manifesto.scm
	gosh $< > $@
	emacs --batch --file=$@ --load=json --funcall=json-pretty-print-buffer --funcall=save-buffer

clean:
	rm -f $(TARGET)
