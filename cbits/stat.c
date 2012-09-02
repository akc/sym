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
