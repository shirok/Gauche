/*
 * port.h - port implementation
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
 *  $Id: port.h,v 1.1.1.1 2001-01-11 19:26:03 shiro Exp $
 */

#ifndef PICCOLO_PORT_H
#define PICCOLO_PORT_H

/*
 * PORT
 */

/* using Quad */
typedef struct ScmPortStr {
    ScmObj tagword;
    ScmObj ungotten;
    union {
        struct {
            FILE *fp;
            
        } file;
    } body;
} ScmPort;


    
#endif /* PICCOLO_PORT_H */
