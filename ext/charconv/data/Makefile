
.SUFFIXES: .EUCJP .SJIS .UTF-8 .CSISO2022JP .EUCKR .CSISO2022KR

GENERATED = jp1.SJIS jp1.UTF-8 jp1.CSISO2022JP \
            jp2.SJIS jp2.UTF-8 jp2.CSISO2022JP \
	    kr1.UTF-8 kr1.CSISO2022KR

.EUCJP.SJIS:
	iconv -f EUC-JP -t SJIS $< > $@

.EUCJP.UTF-8:
	iconv -f EUC-JP -t UTF-8 $< > $@

.EUCJP.CSISO2022JP:
	iconv -f EUC-JP -t CSISO2022JP $< > $@

.EUCKR.UTF-8:
	iconv -f EUC-KR -t UTF-8 $< > $@

.EUCKR.CSISO2022KR:
	iconv -f EUC-KR -t CSISO2022KR $< > $@

all: $(GENERATED)

clean :
	rm -f core *~

distclean : clean

realclean : clean
	rm -f $(GENERATED)
