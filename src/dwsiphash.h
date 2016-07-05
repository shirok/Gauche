/* Public domain 2013 Sam Trenholme */

#include <stdint.h>

/* Size of a single SipHash word */
#define DwSH_WORD uint64_t
#define DwSH_BWIDTH 64
#define DwSH_OWIDTH 8
#define DwSH_WIDTHM 7

/* Rotation constants.  The i constants are here to make the C rotation
 * code simpler (since a circular rotate is (x << a) | (x >> 64 - a) ) */
#define DwSH_R1  13
#define DwSH_R1i 51
#define DwSH_R2  16
#define DwSH_R2i 48
#define DwSH_R3  32
#define DwSH_R3i 32
#define DwSH_R4  17
#define DwSH_R4i 47
#define DwSH_R5  21
#define DwSH_R5i 43
#define DwSH_R6  32
#define DwSH_R6i 32

/* Key setup constants */
#define DwSH_kc1 0x736f6d6570736575ULL
#define DwSH_kc2 0x646f72616e646f6dULL
#define DwSH_kc3 0x6c7967656e657261ULL
#define DwSH_kc4 0x7465646279746573ULL
#define DwSH_fx  0xff

/* Number of rounds */
#define DwSH_CROUNDS 2
#define DwSH_FROUNDS 4

