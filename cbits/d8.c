/* (c) Anders Claesson 2013 */

/* The inverse of w is assigned to u */
void
inverse(long *u, const long *w, long len)
{
	register long i;

	for (i = 0; i < len; i++, w++)
		u[*w] = i;
}


/* The reverse of w is assigned to u */
void
reverse(long *u, const long *w, long len)
{
	register long i;

	for (i = 0; i < len; i++, u++)
		*u = w[len - i - 1];
}


/* The complement of w is assigned to u */
void
complement(long *u, const long *w, long len)
{
	register long i;

	for (i = 0; i < len; i++, u++, w++)
		*u = len - 1 - *w;
}
