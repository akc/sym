#include <stdlib.h>
#include <string.h>

#define MIN(a,b) (((a)<(b))?(a):(b))
#define MAX(a,b) (((a)>(b))?(a):(b))

/*
 * Determines whether a permutation is simple.
 * Based on Michael Albert's java implementation in PermLab.
 */
int
simple(const long *w, long len)
{
	register int i, j;
	int size  = len * sizeof(*w);
	long *mins = malloc(size);
	long *maxs = malloc(size);
	
	memcpy(mins, w, size);
	memcpy(maxs, w, size);
	
	for (i = 1; i < len-1; i++) {
		for (j = len-1; j >= i; j--) {
			mins[j] = MIN(mins[j-1], w[j]);
			maxs[j] = MAX(maxs[j-1], w[j]);
			if (maxs[j] - mins[j] == i) {
				free(mins);
				free(maxs);
				return 0;
			}
		}
	}
	free(mins);
	free(maxs);
	return 1;
}
