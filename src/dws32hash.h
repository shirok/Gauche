/* Public domain 2013 Sam Trenholme */

#include <stdint.h>

/* Size of a single SipHash word */
#define DwSH_WORD uint32_t
#define DwSH_BWIDTH 32
#define DwSH_OWIDTH 4
#define DwSH_WIDTHM 3

/* Rotation constants.  The i constants are here to make the C rotation
 * code simpler (since a circular rotate is (x << a) | (x >> 32 - a) ) */
#define DwSH_R1  7
#define DwSH_R1i 25
#define DwSH_R2  8
#define DwSH_R2i 24
#define DwSH_R3  16
#define DwSH_R3i 16
#define DwSH_R4  9
#define DwSH_R4i 23
#define DwSH_R5  11
#define DwSH_R5i 21
#define DwSH_R6  16
#define DwSH_R6i 16

/* Key setup constants */
#define DwSH_kc1 0x736f6d65UL
#define DwSH_kc2 0x646f7261UL
#define DwSH_kc3 0x6c796765UL
#define DwSH_kc4 0x74656462UL
#define DwSH_fx  0xff

/* Number of rounds */
#define DwSH_CROUNDS 2
#define DwSH_FROUNDS 4

