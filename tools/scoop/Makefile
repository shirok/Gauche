#

TARGET = gauche.json

all: $(TARGET)

.PHONY: clean

$(TARGET): make-scoop-manifesto.scm
	gosh $< > $@
	emacs --batch $@ -l json -f json-pretty-print-buffer -f save-buffer

clean:
	rm -f $(TARGET)
