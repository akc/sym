/* (c) Anders Claesson 2013 */

#include <stdlib.h>
#include <string.h>

/* The image of w under Simion-Schmidt is assigned to u */
void
simion_schmidt(long *u, const long *w, long len)
{
	register long i, j, x;
	long  size = len * sizeof(*w);
	long *used = alloca(size);

	memset(used, 0, size);
	x = len;

	for (i = 0; i < len; i++, w++, u++) {
		if (*w < x) {
			x = *u = *w;
		} else {
			j = x+1;
			while (j < len && used[j])
				j++;
			*u = j;
		}
		used[*u] = 1;
	}
}


/* The image of w under the inverse of Simion-Schmidt is assigned to u */
void
simion_schmidt_inverse(long *u, const long *w, long len)
{
	register long i, j, x;
	long  size = len * sizeof(*w);
	long *used = alloca(size);

	memset(used, 0, size);
	x = len;

	for (i = 0; i < len; i++, w++, u++) {
		if (*w < x) {
			x = *u = *w;
		} else {
			j = len-1;
			while (j >= 0 && used[j])
				j--;
			*u = j;
		}
		used[*u] = 1;
	}

}
