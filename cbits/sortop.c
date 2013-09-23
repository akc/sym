/* (c) Anders Claesson 2013 */

#include <string.h>

/* One pass of stack-sort implemented a la Petter Br\"and\'en [Actions
 * on permutations and unimodality of descent polynomials, European
 * J. Combin. 29 (2008)]
 */
void
stacksort(long *u, long *w, long len) {
        long i = 0;
        long j = 0;
        long y;
	long size = len * sizeof(*w);

	memcpy(u, w, size);

        while (i < len) {
                j = i;
                y = u[j];
                while (y > u[j+1] && j+1 < len) {
                        u[j] = u[j+1];
                        j++;
                }
                u[j] = y;
                if (j == i)
			i++;
        }
}

/* On pass of bubble-sort */
void
bubblesort(long *u, long *w, long len) {
        long tmp;
	long size = len * sizeof(*w);

	memcpy(u, w, size);

	for (; len > 1; len--, u++) {
		if (*u > *(u+1)) {
			tmp    = *u;
			*u     = *(u+1);
			*(u+1) = tmp;
		}
	}
}
