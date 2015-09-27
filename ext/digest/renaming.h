/*
 * Rename symbols to avoid conflicts
 *
 * The symbols will be a part of Gauche library when compiled as static,
 * and it may conflict with other libraries.  (Compiling as a dynamic
 * library, which is the default settings, won't cause a problem, since
 * these digest modules will be dlopen()-ed.)
 */

#define MD5_Init        Scm_MD5_Init
#define MD5_Update      Scm_MD5_Update
#define MD5_Final       Scm_MD5_Final
#define MD5_Transform   Scm_MD5_Transform
#define MD5_End         Scm_MD5_End
#define MD5_File        Scm_MD5_File
#define MD5_Data        Scm_MD5_Data

#define SHA1_Init       Scm_SHA1_Init
#define SHA1_Update     Scm_SHA1_Update
#define SHA1_Final      Scm_SHA1_Final
#define SHA1_End        Scm_SHA1_End
#define SHA1_Data       Scm_SHA1_Data

#define SHA224_Init     Scm_SHA224_Init
#define SHA224_Update   Scm_SHA224_Update
#define SHA224_Final    Scm_SHA224_Final
#define SHA224_End      Scm_SHA224_End
#define SHA224_Data     Scm_SHA224_Data

#define SHA256_Init     Scm_SHA256_Init
#define SHA256_Update   Scm_SHA256_Update
#define SHA256_Final    Scm_SHA256_Final
#define SHA256_End      Scm_SHA256_End
#define SHA256_Data     Scm_SHA256_Data

#define SHA384_Init     Scm_SHA384_Init
#define SHA384_Update   Scm_SHA384_Update
#define SHA384_Final    Scm_SHA384_Final
#define SHA384_End      Scm_SHA384_End
#define SHA384_Data     Scm_SHA384_Data

#define SHA512_Init     Scm_SHA512_Init
#define SHA512_Update   Scm_SHA512_Update
#define SHA512_Final    Scm_SHA512_Final
#define SHA512_End      Scm_SHA512_End
#define SHA512_Data     Scm_SHA512_Data




