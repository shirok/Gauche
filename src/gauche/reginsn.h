/*
 * reginsn.h - regular expression interpreter instructions
 *
 *  Copyright(C) 2000-2001 by Shiro Kawai (shiro@acm.org)
 *
 *  Permission to use, copy, modify, distribute this software and
 *  accompanying documentation for any purpose is hereby granted,
 *  provided that existing copyright notices are retained in all
 *  copies and that this notice is included verbatim in all
 *  distributions.
 *  This software is provided as is, without express or implied
 *  warranty.  In no circumstances the author(s) shall be liable
 *  for any damages arising out of the use of this software.
 *
 *  $Id: reginsn.h,v 1.1 2001-04-11 06:19:44 shiro Exp $
 */

DEF_REG_INSN(RX_MATCH)
DEF_REG_INSN(RX_ASSERT)
DEF_REG_INSN(RX_CHARSET)
DEF_REG_INSN(RX_JUMP)
DEF_REG_INSN(RX_TRY)
DEF_REG_INSN(RX_BEGIN)
DEF_REG_INSN(RX_END)

