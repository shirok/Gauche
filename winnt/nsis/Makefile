MAKENSIS = "$(shell which makensis.exe 2> /dev/null)"
GEN_TARGET = setup.nsi
GAUCHE_VERSION = $(shell cat ../../VERSION 2> /dev/null)
TARGET = Gauche-win32-$(GAUCHE_VERSION).exe

all: gen build

gen: $(GEN_TARGET)

$(GEN_TARGET): $(GEN_TARGET).in
	gosh ./file-list.scm ../../../Gauche-mingw-dist/Gauche $(GEN_TARGET).in $(GAUCHE_VERSION)

build: $(TARGET)

$(TARGET): $(GEN_TARGET)
ifneq ($(MAKENSIS), "")
	$(MAKENSIS) $(GEN_TARGET)
endif

clean:
	@if test -f $(GEN_TARGET); then \
		rm $(GEN_TARGET); \
	fi
	@if test -f $(TARGET); then \
		rm $(TARGET); \
	fi

