#include <stdlib.h>

/*
 * Determines whether the subword in v specified by m is order
 * isomorphic to u; len is the length of u.
 */
int
ordiso(const long *u, const long *v, const long *m, long len)
{
	register int i;
	long *w = malloc(len*sizeof(*w));
	long *w0 = w;

        /* Let w = v.m.u^{-1} */
	for (i = 0; i < len; i++, u++, m++)
		w[(int)*u] = v[(int)*m];

        /* Return 1 if w is increasing, 0 otherwise */
        for (; len > 1; len--, w++) {
                if (*w > *(w+1)) {
			free(w0);
			return 0;
		}
        }
	free(w0);
	return 1;
}
