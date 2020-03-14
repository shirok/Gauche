#

TARGET = gauche.json

all: $(TARGET)

$(TARGET): make-scoop-manifesto.scm
	gosh $< > $@
	emacs --batch $@ -l json -f json-pretty-print-buffer -f save-buffer
