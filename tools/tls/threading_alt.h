/*
 * Use Windows primitives for mbedtls threading support
 */

#ifndef MBEDTLS_THREADING_ALT_H
#define MBEDTLS_THREADING_ALT_H

typedef struct mbedtls_threading_mutex_t
{
    void *mutex;                /* opaque */
} mbedtls_threading_mutex_t;

#endif //MBEDTLS_THREADING_ALT_H
