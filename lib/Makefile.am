
pkgdataversiondir = $(pkgdatadir)/$(VERSION)/lib

pkgdataversion_DATA = srfi-0.scm \
       srfi-1.scm srfi-1/cat.scm srfi-1/filter.scm srfi-1/finder.scm \
       srfi-1/folder.scm srfi-1/generator.scm srfi-1/mapper.scm \
       srfi-1/nary.scm srfi-1/nth.scm srfi-1/pred.scm srfi-1/selector.scm \
       srfi-1/set.scm srfi-1/zipper.scm \
       srfi-2.scm srfi-4.scm srfi-9.scm srfi-11.scm \
       srfi-13.scm srfi-13/casemap.scm srfi-13/comparer.scm \
       srfi-13/filter.scm srfi-13/folder.scm srfi-13/generator.scm \
       srfi-13/internal.scm srfi-13/kmp.scm srfi-13/misc.scm srfi-13/hash.scm \
       srfi-13/pred.scm srfi-13/prefix.scm srfi-13/revapp.scm \
       srfi-13/rotator.scm srfi-13/searcher.scm srfi-13/selector.scm \
       srfi-14.scm srfi-14/query.scm srfi-14/set.scm \
       srfi-19.scm srfi-19/format.scm srfi-19/read-tai.scm \
       srfi-26.scm srfi-27.scm srfi-31.scm srfi-37.scm \
       slib.scm	 \
       gauche/object.scm gauche/test.scm gauche/macroutil.scm \
       gauche/listutil.scm gauche/stringutil.scm gauche/with.scm \
       gauche/regexp.scm gauche/process.scm gauche/signal.scm \
       gauche/numerical.scm gauche/let-opt.scm gauche/logical.scm \
       gauche/vm/disasm.scm gauche/vm/debugger.scm gauche/procedure.scm \
       gauche/serializer.scm gauche/serializer/aserializer.scm \
       gauche/parseopt.scm gauche/interactive.scm gauche/interactive/info.scm \
       gauche/selector.scm gauche/logger.scm gauche/portutil.scm \
       gauche/time.scm gauche/hook.scm gauche/hashutil.scm \
       gauche/common-macros.scm gauche/singleton.scm gauche/validator.scm \
       gauche/version.scm gauche/collection.scm gauche/sequence.scm \
       gauche/interpolate.scm gauche/parameter.scm gauche/defvalues.scm \
       gauche/listener.scm gauche/config.scm gauche/reload.scm \
       gauche/mop/instance-pool.scm gauche/mop/validator.scm \
       gauche/mop/propagate.scm gauche/mop/singleton.scm \
       math/const.scm \
       util/isomorph.scm util/toposort.scm util/queue.scm util/tree.scm \
       util/digest.scm util/combinations.scm util/list.scm \
       compat/jfilter.scm compat/stk.scm \
       file/filter.scm file/util.scm \
       rfc/822.scm rfc/base64.scm rfc/uri.scm rfc/cookie.scm \
       rfc/quoted-printable.scm rfc/http.scm rfc/hmac.scm \
       text/csv.scm text/tr.scm text/parse.scm text/tree.scm \
       text/html-lite.scm text/info.scm text/gettext.scm \
       www/cgi.scm

install-pkgdataversionDATA:
	for i in $(DATA) ; \
	do \
		d=`echo $${i} | sed -e 's,/[^/]*$$,,'` ; \
		if test -d $${d}; \
		then \
			$(mkinstalldirs) "$(DESTDIR)$(pkgdataversiondir)/$${d}" ; \
		fi ; \
		\
		if test -f "$(srcdir)/$${i}" ; \
		then \
			p=$(srcdir)/$${i} ; \
		else \
			p=$${i} ; \
		fi ; \
		\
		$(INSTALL_DATA) $${p} "$(DESTDIR)$(pkgdataversiondir)/$${i}" ; \
	done

CLEANFILES += core *~
DISTCLEANFILES += 
MAINTAINERCLEANFILES += Makefile.in
