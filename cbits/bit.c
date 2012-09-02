 #include <strings.h>

/* Lexicographically, the next bitmask with the same Hamming weight */
unsigned int
next(const unsigned int v)
{
	unsigned int t = v | (v - 1);
	return ((t + 1) | (((~t & -~t) - 1) >> (__builtin_ctz(v) + 1)));
}

/* Positions of bits set */
void
ones(unsigned int *u, const unsigned int a)
{
	unsigned int b;

	for (b = a; b; b &= b-1)
		*u++ = ffs(b) - 1;
}
