/*
 * gauche/priv/vectorP.h - Vector private API
 *
 *   Copyright (c) 2016-2018  Shiro Kawai  <shiro@acm.org>
 *
 *   Redistribution and use in source and binary forms, with or without
 *   modification, are permitted provided that the following conditions
 *   are met:
 *
 *   1. Redistributions of source code must retain the above copyright
 *      notice, this list of conditions and the following disclaimer.
 *
 *   2. Redistributions in binary form must reproduce the above copyright
 *      notice, this list of conditions and the following disclaimer in the
 *      documentation and/or other materials provided with the distribution.
 *
 *   3. Neither the name of the authors nor the names of its contributors
 *      may be used to endorse or promote products derived from this
 *      software without specific prior written permission.
 *
 *   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 *   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 *   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 *   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 *   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 *   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
 *   TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 *   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 *   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 *   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 *   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

#ifndef GAUCHE_PRIV_VECTORP_H
#define GAUCHE_PRIV_VECTORP_H

/*
 * Flat vector API
 *   Some utilities that directly accesses vector of C values (as opposed
 *   to ScmObjs).  They are useful in other parts of Gauche, so we
 *   share it.
 */

/* Binary search KEY in VEC.  LEN is the total length of VEC.
   Each "entry" may consists of more than one element; the first
   element of entry is the key, and the rest are the payload.

   If ESIZE = 2:

      +----------+
      |   key0   |
      | payload0 |
      |   key1   |
      | payload1 |
      :          :

   If ESIZE = 3

      +----------+
      |   key0   |
      | payload0 |
      | payload0 |
      |   key1   |
      | payload1 |
      | payload1 |
      :          :
 */

size_t Scm_BinarySearchS8(const int8_t vec[], size_t len, int8_t key,
                          u_int skip, size_t *floor, size_t *ceil);
size_t Scm_BinarySearchU8(const uint8_t vec[], size_t len, uint8_t key,
                          u_int skip, size_t *floor, size_t *ceil);
size_t Scm_BinarySearchS16(const int16_t vec[], size_t len, int16_t key,
                           u_int skip, size_t *floor, size_t *ceil);
size_t Scm_BinarySearchU16(const uint16_t vec[], size_t len, uint16_t key,
                           u_int skip, size_t *floor, size_t *ceil);
size_t Scm_BinarySearchS32(const int32_t vec[], size_t len, int32_t key,
                           u_int skip, size_t *floor, size_t *ceil);
size_t Scm_BinarySearchU32(const uint32_t vec[], size_t len, uint32_t key,
                           u_int skip, size_t *floor, size_t *ceil);
size_t Scm_BinarySearchS64(const ScmInt64 vec[], size_t len, int64_t key,
                           u_int skip, size_t *floor, size_t *ceil);
size_t Scm_BinarySearchU64(const ScmUInt64 vec[], size_t len, uint64_t key,
                           u_int skip, size_t *floor, size_t *ceil);
size_t Scm_BinarySearchF16(const ScmHalfFloat vec[], size_t len, ScmHalfFloat key,
                           u_int skip, size_t *floor, size_t *ceil);
size_t Scm_BinarySearchF32(const float vec[], size_t len, float key,
                           u_int skip, size_t *floor, size_t *ceil);
size_t Scm_BinarySearchF64(const double vec[], size_t len, double key,
                           u_int skip, size_t *floor, size_t *ceil);

#endif /*GAUCHE_PRIV_VECTORP_H*/

