/* (c) Anders Claesson 2013 */

#include <stdlib.h>
#include <string.h>


/* The number of ascents */
long
asc(const long *w, long len)
{
	long acc = 0;

	for (; len > 1; len--, w++) {
		if (*w < *(w+1))
			acc++;
	}
	return acc;
}


/* The number of descents */
long
des(const long *w, long len)
{
	long acc = 0;

	for (; len > 1; len--, w++) {
		if (*w > *(w+1))
			acc++;
	}
	return acc;
}


/* The number of excedances */
long
exc(const long *w, long len)
{
	long i, acc = 0;

	for (i = 0; i < len; i++, w++) {
		if (*w > i)
			acc++;
	}
	return acc;
}


/* The number of fixed points */
long
fp(const long *w, long len)
{
	long i, acc = 0;

	for (i = 0; i < len; i++, w++) {
		if (*w == i)
			acc++;
	}
	return acc;
}


/* The number of strong fixed points */
long
sfp(const long *w, long len)
{
	long m = *w - 1;
	long i, acc = 0;

	for (i = 0; i < len; i++, w++) {
		if (*w > m) {
			m = *w;
			if (m == i)
				acc++;
		}
	}
	return acc;
}


/* The number of cycles */
long
cyc(const long *w, long len)
{
	long i = 0;
	long j = 0;
	long acc = 0;
	int *called = malloc(len*sizeof(*called));

	memset(called, 0, len*sizeof(*called));

	while (j < len) {
		j = w[i];
		called[i] = 1;
		if (called[j]) {
			acc++;
			while (j < len && called[j])
				j++;
		}
		i = j;
	}
	free(called);
	return acc;
}


/* The number of inversions */
long
inv(const long *w, long len)
{
	long i, j;
	long acc = 0;
	long *v;

	for (i = 0; i < len; i++, w++) {
		for (j = i+1, v = (long*)w+1; j < len; j++, v++) {
			if (*w > *v)
				acc++;
		}
	}
	return acc;
}


/* The major index */
long
maj(const long *w, long len)
{
	long i, sum = 0;

	for (i = 1; i < len; i++, w++) {
		if (*w > *(w+1))
			sum += i;
	}
	return sum;
}

/* The co-major index */
long
comaj(const long *w, long len)
{
	long i, sum = 0;

	for (i = 1; i < len; i++, w++) {
		if (*w > *(w+1))
			sum += len - i;
	}
	return sum;
}


/* The number of peaks */
long
peak(const long *w, long len)
{
	long acc = 0;

	for (; len > 2; len--, w++) {
		if (*w < *(w+1) && *(w+1) > *(w+2))
			acc++;
	}
	return acc;
}


/* The number of valleys */
long
vall(const long *w, long len)
{
	long acc = 0;

	for (; len > 2; len--, w++) {
		if (*w > *(w+1) && *(w+1) < *(w+2))
			acc++;
	}
	return acc;
}


/* The number of double ascents */
long
dasc(const long *w, long len)
{
	long acc = 0;

	for (; len > 2; len--, w++) {
		if (*w < *(w+1) && *(w+1) < *(w+2))
			acc++;
	}
	return acc;
}


/* The number of double descents */
long
ddes(const long *w, long len)
{
	long acc = 0;

	for (; len > 2; len--, w++) {
		if (*w > *(w+1) && *(w+1) > *(w+2))
			acc++;
	}
	return acc;
}


/* The number of left-to-right minima */
long
lmin(const long *w, long len)
{
	long m = *w + 1;
	long acc = 0;

	for (; len > 0; len--, w++) {
		if (*w < m) {
			m = *w;
			acc++;
		}
	}
	return acc;
}


/* The number of left-to-right maxima */
long
lmax(const long *w, long len)
{
	long m = *w - 1;
	long acc = 0;

	for (; len > 0; len--, w++) {
		if (*w > m) {
			m = *w;
			acc++;
		}
	}
	return acc;
}

/* The left-most increasing run */
long
lir(const long *w, long len)
{
	long acc;

	if (len == 0)
		return 0;

	for (acc = 1; len > 1 && *w < *(w+1); len--, w++)
		acc++;

	return acc;
}

/* The left-most decreasing run */
long
ldr(const long *w, long len)
{
	long acc;

	if (len == 0)
		return 0;

	for (acc = 1; len > 1 && *w > *(w+1); len--, w++)
		acc++;

	return acc;
}

/* The number of components; O(len) */
long
comp(const long *w, long len)
{
	long i;
	long m = *w - 1;
	long acc = 0;

	for (i = 0; i < len; i++, w++) {
		if (*w > m)
			m = *w;
		if (m == i)
			acc++;
	}
	return acc;
}

/* rank as defined by Elizalde & Pak */
long
ep(const long *w, long len)
{
	long i;
	long m = *w;

	if (len == 0)
		return 0;

	for (i = 0; i < len; i++, w++) {
		if (*w <= m)
			m = *w;
		if (m <= i)
			return i;
	}
	return len;
}

/* The dimension is the largest i such that w[i] != i */
long
dim(const long *w, long len)
{
	long i, j = 0;

	for (i = 0; i < len; i++, w++) {
		if (*w != i)
			j = i;
	}
	return j;
}

/* The number of small ascents: i such that w[i] + 1 == w[i+1] */
long
asc0(const long *w, long len)
{
	long acc = 0;

	for (; len > 1; len--, w++) {
		if (*w + 1 == *(w+1))
			acc++;
	}
	return acc;
}


/* The number of small descents: i such that w[i] == w[i+1] + 1*/
long
des0(const long *w, long len)
{
	long acc = 0;

	for (; len > 1; len--, w++) {
		if (*w == *(w+1) + 1)
			acc++;
	}
	return acc;
}
