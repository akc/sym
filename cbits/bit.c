/* (c) Anders Claesson 2013 */

#include <strings.h>

/* Lexicographically, the next bitmask with the same Hamming weight */
long
next(const long v)
{
	long t = v | (v - 1);
	return ((t + 1) | (((~t & -~t) - 1) >> (__builtin_ctz(v) + 1)));
}

/* Positions of bits set */
void
ones(long *u, const long a)
{
	long b;

	for (b = a; b; b &= b-1)
		*u++ = ffs(b) - 1;
}
