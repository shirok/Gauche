
.SUFFIXES: .EUCJP .SJIS .UTF8 .CSISO2022JP .EUCKR .CSISO2022KR

GENERATED = jp1.SJIS jp1.UTF8 jp1.CSISO2022JP \
            jp2.SJIS jp2.UTF8 jp2.CSISO2022JP \
	    kr1.UTF8 kr1.CSISO2022KR

.EUCJP.SJIS:
	iconv -f EUC-JP -t SJIS -o $@ $<

.EUCJP.UTF8:
	iconv -f EUC-JP -t UTF8 -o $@ $<

.EUCJP.CSISO2022JP:
	iconv -f EUC-JP -t CSISO2022JP -o $@ $<

.EUCKR.UTF8:
	iconv -f EUC-KR -t UTF8 -o $@ $<

.EUCKR.CSISO2022KR:
	iconv -f EUC-KR -t CSISO2022KR -o $@ $<

all: $(GENERATED)

clean :
	rm -f core *~

distclean : clean

realclean : clean
	rm -f $(GENERATED)
