#

TARGET = gauche.json

all: $(TARGET)

$(TARGET): make-scoop-manifesto.scm
	gosh $< > $@
