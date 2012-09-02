
/* One pass of stack-sort implemented a la Petter Br\"and\'en [Actions
 * on permutations and unimodality of descent polynomials, European
 * J. Combin. 29 (2008)]
 */
void
stacksort(long *w, long len) {
        int i = 0;
        int j = 0;
        int y;
        while (i < len) {
                j = i;
                y = w[j];
                while (y > w[j+1] && j+1 < len) {
                        w[j] = w[j+1];
                        j++;
                }
                w[j] = y;
                if (j == i)
			i++;
        }
}

/* On pass of bubble-sort */
void
bubblesort(long *w, long len) {
        int tmp;
	for (; len > 1; len--, w++) {
		if (*w > *(w+1)) {
			tmp    = *w;
			*w     = *(w+1);
			*(w+1) = tmp;
		}
	}
}
