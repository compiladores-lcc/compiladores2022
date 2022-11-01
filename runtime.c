#include <stdio.h>
#include <stdint.h>
#include <stdarg.h>
#include <stdlib.h>
#include <inttypes.h>
#include <gc.h>
#include <wchar.h>

/*
  Runtime para el compilador a C.
  Compilar los programas generados con:
	gcc runtime.c -lgc programa.c
*/

uint64_t fd4_printn(uint64_t x)
{
	wprintf(L"%" PRIu64 "\n", x);
	return x;
}

uint64_t fd4_sub(uint64_t x, uint64_t y)
{
	if (x < y)
		return 0;

	return x - y;
}

void *fd4_mkclosure(void *fun, int amt, ...)
{
	int i;
	va_list valist;
	uint64_t** res = GC_malloc(sizeof (uint64_t*) * (amt + 1));

	if (!res) {
		fprintf(stderr, "OOM\n");
		abort();
	}

	va_start(valist, amt);

	res[0] = fun;
	i = 0;
	while (i++ < amt) {
		uint64_t *a = va_arg(valist, uint64_t*);
		res[i] = a;
	}
	va_end(valist);

	return res;
}

extern uint64_t* fd4main(void);

int main(int argc, char **argv)
{
	GC_INIT();
	uint64_t* rp = fd4main();
	uint64_t r = (uint64_t)rp;
	return r;
}
