# automake doesn't like things in subdirectories, so I was forced to
# make this Makefile.am only for proper installation of headers. --SK

pkggauchehdrsubdir = $(pkglibdir)/$(VERSION)/include/gauche

pkggauchehdrsub_HEADERS = \
      vm.h vminsn.h  char_euc_jp.h char_utf_8.h char_sjis.h char_none.h \
      predef-syms.h extend.h class.h exception.h pthread.h uthread.h

MAINTAINERCLEANFILES += Makefile.in
