
.SUFFIXES: .EUCJP .SJIS .UTF-8 .ISO2022JP .EUCKR .ISO2022KR

GENERATED = jp1.SJIS jp1.UTF-8 jp1.ISO2022JP \
	    jp2.SJIS jp2.ISO2022JP \
            jp3.SJIS jp3.UTF-8 jp3.ISO2022JP \
            jp4.SJIS jp4.UTF-8 jp4.ISO2022JP \
	    kr1.UTF-8 kr1.ISO2022KR

.EUCJP.SJIS:
	nkf -E -s -x $< > $@

.EUCJP.UTF-8:
	iconv -f EUC-JP -t UTF-8 $< > $@

.EUCJP.ISO2022JP:
	nkf -E -j -x $< > $@

.EUCJP.ASCII:
	iconv -f EUC-JP -t ASCII//TRANSLIT $< > $@

.EUCKR.UTF-8:
	iconv -f EUC-KR -t UTF-8 $< > $@

.EUCKR.ISO2022KR:
	iconv -f EUC-KR -t ISO2022KR $< > $@

.EUCKR.ASCII:
	iconv -f EUC-KR -t ASCII//TRANSLIT $< > $@

all: $(GENERATED)

clean :
	rm -f core *~

distclean : clean

realclean : clean
