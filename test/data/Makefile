# This makefile is to prepare packaging data.
# The user doesn't need this.

GETTEXT_DATA = gettext.data.euc-jp.scm \
               gettext.data.sjis.scm \
               gettext.data.none.scm

GENERATED = $(GETTEXT_DATA)


all : $(GENERATED)

check :

clean :
	rm -f core *~

distclean : clean

maintainer-clean : clean
	rm -f $(GENERATED)

gettext.data.euc-jp.scm : gettext.data.utf-8.scm
	gosh -I../../src gauche-cesconv -f utf-8 -t eucjp -o gettext.data.euc-jp.scm gettext.data.utf-8.scm

gettext.data.sjis.scm : gettext.data.utf-8.scm
	gosh -I../../src gauche-cesconv -f utf-8 -t sjis -o gettext.data.sjis.scm gettext.data.utf-8.scm

# note: 'none' must be in eucjp encoding, since test .po file is
# in eucjp encoding and 'none' compares them bytewise.
gettext.data.none.scm : gettext.data.utf-8.scm
	gosh -I../../src gauche-cesconv -f utf-8 -t eucjp -o gettext.data.none.scm gettext.data.utf-8.scm

