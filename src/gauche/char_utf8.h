/*
 * char_utf8.h - UTF8 encoding interface
 *
 *  Copyright(C) 2000 by Shiro Kawai (shiro@acm.org)
 *
 *  Permission to use, copy, modify, ditribute this software and
 *  accompanying documentation for any purpose is hereby granted,
 *  provided that existing copyright notices are retained in all
 *  copies and that this notice is included verbatim in all
 *  distributions.
 *  This software is provided as is, without express or implied
 *  warranty.  In no circumstances the author(s) shall be liable
 *  for any damages arising out of the use of this software.
 *
 *  $Id: char_utf8.h,v 1.2 2001-04-20 08:44:47 shiro Exp $
 */

#define SCM_CHAR_ENCODING_NAME "utf8"

#define SCM_CHAR_NFOLLOWS(ch) \
    ((unsigned char)(ch) < 0xc0) ? 0:
       (unsigned char)(ch) < 0xe0) ? 1:
