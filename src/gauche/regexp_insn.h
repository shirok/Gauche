/*
 * Regexp engine instructions
 *
 * This file is included multiple times from src/regexp.c
 */

/* DEF_RE_INSN(name, operand-type)

   Operand-type can be:

   none        - no operand
   octet       - one octet (character)
   string      - one byte length + bytestring
   cset        - code set number
   group       - 1-byte group number
   offset2     - 2-byte offset
   offset1_2   - 1-byte ofset & 2-byte offset
   offset2_2   - 2-1byte ofset & 2-byte offset
*/

/* 'RL' suffix indicates that the instructions moves the current position
   pointer right to left.   They are used within lookbehind assertions. */

/* Followed by 1 byte to match */
DEF_RE_INSN(MATCH1, OP_octet)
DEF_RE_INSN(MATCH1_RL, OP_octet)

/* Followed by length, and bytes to match */
DEF_RE_INSN(MATCH, OP_string)
DEF_RE_INSN(MATCH_RL, OP_string)

/* Followed by 1 byte to match, case insensitive */
DEF_RE_INSN(MATCH1_CI, OP_octet)
DEF_RE_INSN(MATCH1_CI_RL, OP_octet)

/* Followed by length, and bytes to match, case insensitive */
DEF_RE_INSN(MATCH_CI, OP_string)
DEF_RE_INSN(MATCH_CI_RL, OP_string)

/* Match any char */
DEF_RE_INSN(ANY, OP_none)
DEF_RE_INSN(ANY_RL, OP_none)

/* Followed by offset (2 bytes).  Try matching the following sequence,
   and if filas, jump to offset.  This handles backtracking. */
DEF_RE_INSN(TRY, OP_offset2)

/* Followed by charset #.  Match any char in the charset. */
DEF_RE_INSN(SET, OP_cset)
DEF_RE_INSN(SET_RL, OP_cset)

/* Followed by charset #.  Match any char in the charset.  */
DEF_RE_INSN(NSET, OP_cset)
DEF_RE_INSN(NSET_RL, OP_cset)

/* Followed by charset #.  Match any char in the charset.  Guaranteed
   that the charset holds only range 0-127.  */
DEF_RE_INSN(SET1, OP_cset)
DEF_RE_INSN(SET1_RL, OP_cset)

/* Followed by charset #.  Match any char in the charset.  Guaranteed
   that the charset holds only range 0-127.  */
DEF_RE_INSN(NSET1, OP_cset)
DEF_RE_INSN(NSET1_RL, OP_cset)

/* Followed by offset (2 bytes).  Jump to that bytecode. */
DEF_RE_INSN(JUMP, OP_offset2)

/* Fail and success */
DEF_RE_INSN(FAIL, OP_none)
DEF_RE_INSN(SUCCESS, OP_none)

/* Follwed by a group number.  Start the group. */
DEF_RE_INSN(BEGIN, OP_group)
DEF_RE_INSN(BEGIN_RL, OP_group)

/* Followed by a group number.  End the group. */
DEF_RE_INSN(END, OP_group)
DEF_RE_INSN(END_RL, OP_group)

/* Beginning and end of string assertions */
DEF_RE_INSN(BOS, OP_none)
DEF_RE_INSN(EOS, OP_none)

/* Beginning and end of line assertions */
DEF_RE_INSN(BOL, OP_none)
DEF_RE_INSN(EOL, OP_none)

/* Beginning and end of word boundary assertions */
DEF_RE_INSN(BOW, OP_none)
DEF_RE_INSN(EOW, OP_none)

/* RE_BOW + RE_EOW */
DEF_RE_INSN(WB, OP_none)

/* Negative word boundary assertion */
DEF_RE_INSN(NWB, OP_none)

/* Beginning and end of grapheme cluster */
DEF_RE_INSN(BOG, OP_none)
DEF_RE_INSN(EOG, OP_none)

/* Backreference.  Followed by group # */
DEF_RE_INSN(BACKREF, OP_group)
DEF_RE_INSN(BACKREF_RL, OP_group)
DEF_RE_INSN(BACKREF_CI, OP_group)
DEF_RE_INSN(BACKREF_CI_RL, OP_group)

/* Conditional pattern */
DEF_RE_INSN(CPAT, OP_offset1_2)
DEF_RE_INSN(CPATA, OP_offset2_2)

/* Standalone pattern */
DEF_RE_INSN(ONCE, OP_offset2)

/* Positive and negative lookahead assertion.  Followed by 2 byte offset */
DEF_RE_INSN(ASSERT, OP_offset2)
DEF_RE_INSN(NASSERT, OP_offset2)

/* The following instructions are not necessary to implement the basic
   engine, but used in the optimized code.
   The *R instructions (and *R_RL counterparts) consumes all input that
   matches, without backtracking.  */

/* 1-byte set match repeat, followed by charset #. */
DEF_RE_INSN(SET1R, OP_cset)
DEF_RE_INSN(SET1R_RL, OP_cset)

/* 1-byte negative set match repeat,  followed by charset #. */
DEF_RE_INSN(NSET1R, OP_cset)
DEF_RE_INSN(NSET1R_RL, OP_cset)

/* set match repeat, followed by charset #. */
DEF_RE_INSN(SETR, OP_cset)
DEF_RE_INSN(SETR_RL, OP_cset)

/* negative set match repeat, followed by charset #. */
DEF_RE_INSN(NSETR, OP_cset)
DEF_RE_INSN(NSETR_RL, OP_cset)

/* 1-byte exact match repeat, followed by a byte */
DEF_RE_INSN(MATCH1R, OP_octet)

/* multiple byte exact match repeat, followed by length, and bytes to match. */
DEF_RE_INSN(MATCHR, OP_string)

/* any char match repeat */
DEF_RE_INSN(ANYR, OP_none)
