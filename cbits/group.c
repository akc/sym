/* (c) Anders Claesson 2013 */

/* The permutation w is defined by w[i] = u[v[i]]. */
void
compose(long *w, const long *u, long k, const long *v, long n)
{
	register long i = 0;

	if (k < n) {
		for ( ; i < n; i++, w++, v++)
			*w = (*v < k) ? u[*v] : *v;
	} else {
		for ( ; i < n; i++, w++, v++)
			*w = u[*v];

		for ( ; i < k; i++, w++)
			*w = u[i];
	}
}

/* The permutation w is defined by w[u[i]] = v[i]. */
void
act(long *w, const long *u, long k, const long *v, long n)
{
	register long i = 0;

	if (k < n) {
		for ( ; i < k; i++, u++, v++)
			w[*u] = *v;

		for ( ; i < n; i++, v++)
			w[i] = *v;
	} else {
		for ( ; i < n; i++, u++, v++)
			w[*u] = *v;

		for ( ; i < k; i++, u++)
			w[*u] = i; 
	}
}
