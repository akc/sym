/* (c) Anders Claesson 2013 */

#include <stdlib.h>

/* Determines whether the subword in v specified by m is order
 * isomorphic to u; len is the length of u.
 */
int
ordiso(const long *u, const long *v, const long *m, long len)
{
	register long i;
	long *w = alloca(len*sizeof(*w));

	/* Let w = v.m.u^{-1} */
	for (i = 0; i < len; i++, u++, m++)
		w[*u] = v[*m];

	/* Return 1 if w is increasing, 0 otherwise */
	for (; len > 1; len--, w++) {
		if (*w > *(w+1))
			return 0;
	}
	return 1;
}
