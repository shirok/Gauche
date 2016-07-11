/* Public domain 2013 Sam Trenholme */

#include "dws32hash.h"
#include "gauche/priv/dws_adapter.h"

/* Core sipHash round function, as per spec */
void DwSip_round(DwSH_WORD *v0, DwSH_WORD *v1, DwSH_WORD *v2, DwSH_WORD *v3) {
	*v0 += *v1;
	*v2 += *v3;
	*v1 = (*v1 << DwSH_R1) | (*v1 >> DwSH_R1i);
	*v3 = (*v3 << DwSH_R2) | (*v3 >> DwSH_R2i);
	*v1 ^= *v0;
	*v3 ^= *v2;
	*v0 = (*v0 << DwSH_R3) | (*v0 >> DwSH_R3i);
	*v2 += *v1;
	*v0 += *v3; 
	*v1 = (*v1 << DwSH_R4) | (*v1 >> DwSH_R4i);
	*v3 = (*v3 << DwSH_R5) | (*v3 >> DwSH_R5i);
	*v1 ^= *v2;
	*v3 ^= *v0;
	*v2 = (*v2 << DwSH_R6) | (*v2 >> DwSH_R6i);
}

/* Key setup.  Given two keys, set up the first state and final XOR constant */
void DwSip_ksetup(DwSH_WORD *k0, DwSH_WORD *k1, DwSH_WORD *v0, DwSH_WORD *v1,
		DwSH_WORD *v2, DwSH_WORD *v3, DwSH_WORD *fx) {
	*v0 = *k0 ^ DwSH_kc1;
	*v1 = *k1 ^ DwSH_kc2;
	*v2 = *k0 ^ DwSH_kc3;
	*v3 = *k1 ^ DwSH_kc4;
	*fx = DwSH_fx;
}

/* Convert part of a word in to a message block SipHash can understand */
DwSH_WORD DwSip_getword(uint32_t *offset, uint8_t *str, uint32_t len) {
	uint32_t toffset = *offset;
	int shift = 0;
	DwSH_WORD out = 0;
	do {	
		if(toffset >= len) {
			out |= ((DwSH_WORD)len & 0xff) << (DwSH_BWIDTH - 8);
			*offset = len + 1;
			return out;
		}
		out |= (DwSH_WORD)*(str + toffset) << shift;
		shift += 8;
		toffset++;
	} while(toffset < *offset + DwSH_OWIDTH);
	*offset = toffset;
	return out;
}

/* Calculate the hash for a given string */
DwSH_WORD DwSip_hash(uint8_t *str, uint32_t len, DwSH_WORD k1, DwSH_WORD k2) {
	uint32_t offset = 0;
	int a = 0;
	DwSH_WORD v0 = 0, v1 = 0, v2 = 0, v3 = 0, fx = 0, m = 0;
	DwSip_ksetup(&k1, &k2, &v0, &v1, &v2, &v3, &fx);
	while(offset <= len) {
		m = DwSip_getword(&offset, str, len);
		v3 ^= m;
		for(a = 0; a < DwSH_CROUNDS; a++) {
			DwSip_round(&v0,&v1,&v2,&v3);
		}
		v0 ^= m;
	} 
	v2 ^= fx;
	for(a = 0; a < DwSH_FROUNDS; a++) {
		DwSip_round(&v0,&v1,&v2,&v3);
	}
	return v0 ^ v1 ^ v2 ^ v3;
}
	
#ifdef MAIN
#include <stdio.h>
int main() {
	uint8_t str[64];
	uint32_t len = 15;
	DwSH_WORD o = 0;
	for(len = 0; len < 64; len++) {
		str[len] = len;
		o = DwSip_hash(str, len, 0x03020100UL, 0x07060504UL);
		printf("%08x\n",o);
	}
	return 0;
}
#endif /* MAIN */
