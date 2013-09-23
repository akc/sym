/* (c) Anders Claesson 2013 */

#include <stdlib.h>
#include <string.h>
#include <math.h>


static inline void
swap(long *p1, long *p2)
{
	long tmp;

	tmp = *p1;
	*p1 = *p2;
	*p2 = tmp;
}


/* Returns the rank-th (Myrvold & Ruskey) permutation of {0..len-1}.
 * E.g., len = 3 and rank = 0..5 gives 120 201 102 210 021 012
 */
void
unrank(long *w, long len, double rank)
{
	register long i;

	for (i = 0; i < len; i++)
		w[i] = i;

	for (i = len; i > 0; i--) {
		swap(w + i - 1, w + (long)fmod(rank, i));
		rank /= i;
	}
}


/* Returns the (Myrvold & Ruskey)-rank of the length len permutation w.
 */
double
rank(long *w, long len)
{
	long s;
	long r = 0;
	long a = 1;
	long size = len * sizeof(*w);
	long *u = alloca(size);
	long *v = alloca(size);
	register long i;

	/* Let v = w */
	memcpy(v, w, size);

	/* Let u = w^{-1} */
	for (i = 0; i < len; i++, w++)
		u[*w] = i;

	while (--len > 0) {
		s = v[len];
		swap(v + len, v + u[len]);
		swap(u + s, u + len);
		r += s*a;
		a *= len+1;
	};

	return r;
}
