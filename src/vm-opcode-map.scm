;; VM instruction name to opcode map
;; To regenerate this, run 'make generate-opcode-map'

(NOP . 0)
(CONST . 1)
(CONSTI . 2)
(CONSTN . 3)
(CONSTF . 4)
(CONSTU . 5)
(CONST-PUSH . 6)
(CONSTI-PUSH . 7)
(CONSTN-PUSH . 8)
(CONSTF-PUSH . 9)
(CONST-RET . 10)
(CONSTF-RET . 11)
(CONSTU-RET . 12)
(PUSH . 13)
(PRE-CALL . 14)
(PUSH-PRE-CALL . 15)
(CHECK-STACK . 16)
(CALL . 17)
(TAIL-CALL . 18)
(JUMP . 19)
(RET . 20)
(DEFINE . 21)
(CLOSURE . 22)
(LOCAL-ENV . 23)
(PUSH-LOCAL-ENV . 24)
(LOCAL-ENV-CLOSURES . 25)
(POP-LOCAL-ENV . 26)
(LOCAL-ENV-JUMP . 27)
(LOCAL-ENV-CALL . 28)
(LOCAL-ENV-TAIL-CALL . 29)
(BF . 30)
(BT . 31)
(BNEQ . 32)
(BNEQV . 33)
(BNNULL . 34)
(BNUMNE . 35)
(BNLT . 36)
(BNLE . 37)
(BNGT . 38)
(BNGE . 39)
(LREF-VAL0-BNUMNE . 40)
(LREF-VAL0-BNLT . 41)
(LREF-VAL0-BNLE . 42)
(LREF-VAL0-BNGT . 43)
(LREF-VAL0-BNGE . 44)
(BNUMNEI . 45)
(BNEQC . 46)
(BNEQVC . 47)
(RF . 48)
(RT . 49)
(RNEQ . 50)
(RNEQV . 51)
(RNNULL . 52)
(RECEIVE . 53)
(TAIL-RECEIVE . 54)
(RECEIVE-ALL . 55)
(TAIL-RECEIVE-ALL . 56)
(VALUES-N . 57)
(LSET . 58)
(GSET . 59)
(LREF . 60)
(LREF0 . 61)
(LREF1 . 62)
(LREF2 . 63)
(LREF3 . 64)
(LREF10 . 65)
(LREF11 . 66)
(LREF12 . 67)
(LREF20 . 68)
(LREF21 . 69)
(LREF30 . 70)
(LREF-PUSH . 71)
(LREF0-PUSH . 72)
(LREF1-PUSH . 73)
(LREF2-PUSH . 74)
(LREF3-PUSH . 75)
(LREF10-PUSH . 76)
(LREF11-PUSH . 77)
(LREF12-PUSH . 78)
(LREF20-PUSH . 79)
(LREF21-PUSH . 80)
(LREF30-PUSH . 81)
(LREF-RET . 82)
(LREF0-RET . 83)
(LREF1-RET . 84)
(LREF2-RET . 85)
(LREF3-RET . 86)
(LREF10-RET . 87)
(LREF11-RET . 88)
(LREF12-RET . 89)
(LREF20-RET . 90)
(LREF21-RET . 91)
(LREF30-RET . 92)
(GREF . 93)
(GREF-PUSH . 94)
(GREF-CALL . 95)
(GREF-TAIL-CALL . 96)
(PUSH-GREF . 97)
(PUSH-GREF-CALL . 98)
(PUSH-GREF-TAIL-CALL . 99)
(PROMISE . 100)
(VALUES-APPLY . 101)
(CONS . 102)
(CONS-PUSH . 103)
(CAR . 104)
(CAR-PUSH . 105)
(LREF0-CAR . 106)
(LREF1-CAR . 107)
(LREF2-CAR . 108)
(LREF3-CAR . 109)
(LREF10-CAR . 110)
(LREF11-CAR . 111)
(LREF12-CAR . 112)
(LREF20-CAR . 113)
(LREF21-CAR . 114)
(LREF30-CAR . 115)
(CDR . 116)
(CDR-PUSH . 117)
(LREF0-CDR . 118)
(LREF1-CDR . 119)
(LREF2-CDR . 120)
(LREF3-CDR . 121)
(LREF10-CDR . 122)
(LREF11-CDR . 123)
(LREF12-CDR . 124)
(LREF20-CDR . 125)
(LREF21-CDR . 126)
(LREF30-CDR . 127)
(CAAR . 128)
(CAAR-PUSH . 129)
(CADR . 130)
(CADR-PUSH . 131)
(CDAR . 132)
(CDAR-PUSH . 133)
(CDDR . 134)
(CDDR-PUSH . 135)
(LIST . 136)
(LIST-STAR . 137)
(LENGTH . 138)
(MEMQ . 139)
(MEMV . 140)
(ASSQ . 141)
(ASSV . 142)
(EQ . 143)
(EQV . 144)
(APPEND . 145)
(NOT . 146)
(REVERSE . 147)
(APPLY . 148)
(TAIL-APPLY . 149)
(IS-A . 150)
(NULLP . 151)
(PAIRP . 152)
(CHARP . 153)
(EOFP . 154)
(STRINGP . 155)
(SYMBOLP . 156)
(VECTORP . 157)
(NUMBERP . 158)
(REALP . 159)
(IDENTIFIERP . 160)
(SETTER . 161)
(VALUES . 162)
(VALUES-RET . 163)
(VEC . 164)
(LIST2VEC . 165)
(APP-VEC . 166)
(VEC-LEN . 167)
(VEC-REF . 168)
(VEC-SET . 169)
(VEC-REFI . 170)
(VEC-SETI . 171)
(UVEC-REF . 172)
(NUMEQ2 . 173)
(NUMLT2 . 174)
(NUMLE2 . 175)
(NUMGT2 . 176)
(NUMGE2 . 177)
(NUMADD2 . 178)
(NUMSUB2 . 179)
(NUMMUL2 . 180)
(NUMDIV2 . 181)
(LREF-VAL0-NUMADD2 . 182)
(NEGATE . 183)
(NUMIADD2 . 184)
(NUMISUB2 . 185)
(NUMIMUL2 . 186)
(NUMIDIV2 . 187)
(NUMADDI . 188)
(LREF0-NUMADDI . 189)
(LREF1-NUMADDI . 190)
(LREF2-NUMADDI . 191)
(LREF3-NUMADDI . 192)
(LREF10-NUMADDI . 193)
(LREF11-NUMADDI . 194)
(LREF12-NUMADDI . 195)
(LREF20-NUMADDI . 196)
(LREF21-NUMADDI . 197)
(LREF30-NUMADDI . 198)
(LREF0-NUMADDI-PUSH . 199)
(LREF1-NUMADDI-PUSH . 200)
(LREF2-NUMADDI-PUSH . 201)
(LREF3-NUMADDI-PUSH . 202)
(LREF10-NUMADDI-PUSH . 203)
(LREF11-NUMADDI-PUSH . 204)
(LREF12-NUMADDI-PUSH . 205)
(LREF20-NUMADDI-PUSH . 206)
(LREF21-NUMADDI-PUSH . 207)
(LREF30-NUMADDI-PUSH . 208)
(NUMSUBI . 209)
(NUMMODI . 210)
(NUMREMI . 211)
(ASHI . 212)
(LOGAND . 213)
(LOGIOR . 214)
(LOGXOR . 215)
(LOGANDC . 216)
(LOGIORC . 217)
(LOGXORC . 218)
(READ-CHAR . 219)
(PEEK-CHAR . 220)
(WRITE-CHAR . 221)
(CURIN . 222)
(CUROUT . 223)
(CURERR . 224)
(SLOT-REF . 225)
(SLOT-SET . 226)
(SLOT-REFC . 227)
(SLOT-SETC . 228)
(PUSH-HANDLERS . 229)
(POP-HANDLERS . 230)
(BOX . 231)
(ENV-SET . 232)
(UNBOX . 233)
(LREF-UNBOX . 234)
(LOCAL-ENV-SHIFT . 235)
(XLREF . 236)
(XLSET . 237)
(EXTEND-DENV . 238)
(TAIL-EXTEND-DENV . 239)
