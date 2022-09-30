/* La Verdadera Macchina */

#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <stdint.h>
#include <inttypes.h>
#include <gc.h>
#include <wchar.h>
#include <locale.h>

#define STATIC_ASSERT(p)			\
	void _ass_ ## __FILE__ ## __LINE__ (char v[(p) ? 1 : -1])

/* Necesitamos que un uint32_t (i.e. una instrucción) entre en un int. */
STATIC_ASSERT(sizeof (int) >= sizeof (uint32_t));

/* Habilitar impresión de traza? */
#define TRACE 0

enum {
	RETURN   = 1,
	CONST    = 2,
	ACCESS   = 3,
	FUNCTION = 4,
	CALL     = 5,
	ADD      = 6,
	SUB      = 7,
	JUMP     = 8,
	FIX      = 9,
	STOP     = 10,
	SHIFT    = 11,
	DROP     = 12,
	PRINT    = 13,
	PRINTN   = 14,
	CJUMP    = 15,
	TAILCALL = 16,
};

#define quit(...)							\
	do {								\
		fprintf(stderr, __VA_ARGS__);				\
		fprintf(stderr, "\n");					\
		if (errno)						\
			fprintf(stderr, "errno = %d\n", errno);		\
		exit(EXIT_FAILURE);					\
	} while (0)

/*
 * Un bytecode es un array de enteros, incluyendo tanto opcodes como
 * datos (e.g. constantes enteras). Para las operaciones simples se
 * recorre opcode a opcode operando en la stack. Las más interesantes
 * involucran saltos y la construcción de clausuras.
 */
typedef uint32_t *code;

/*
 * Un entorno es una lista enlazada de valores. Representan los valores
 * para cada variable de de Bruijn en el "término" que evaluamos.
 */
typedef struct env *env;

/*
 * Una clausura: un par compuesto de un entorno y un cuerpo, que es
 * simplemente un puntero a código, es decir simplemente una etiqueta.
 */
struct clo {
	env  clo_env;
	code clo_body;
};

/*
 * Los valores son o un entero, o una clausura. Notar que no hay una
 * etiqueta para distinguirlos: la máquina asume que el bytecode estaba
 * bien tipado, y usa el dato que espera tener. No se hace ningún tipo
 * de chequeo en runtime.
 */
union value {
	uint32_t i;
	struct clo clo;
};
typedef union value value;

/*
 * Entornos: listas enlazadas de valores. Notar la recursión mutua
 * entre las clausuras y los entornos: un entorno es una lista de
 * valores, que pueden ser clausuras; y cada clausura tiene un entorno.
 */
struct env {
	value v;
	struct env *next;
};

/*
 * Empuja un valor al entorno `e` y devuelve el entorno extendido.
 */
static inline env env_push(env e, value v)
{
	env new = GC_malloc(sizeof *new);
	new->v = v;
	new->next = e;
	return new;
}

/*
 * Sólo para debugging: devuelve la longitud de un entorno.
 */
static int env_len(env e)
{
	int rc = 0;
	while (e) {
		e = e->next;
		rc++;
	}
	return rc;
}

void run(code init_c)
{
	/*
	 * La pila de valores de la máquina, alocada en memoria dinámica.
	 * La misma se agranda si está cerca de llenarse.
	 */
	int stack_size = 4096;
	value *stack = GC_malloc(stack_size * sizeof stack[0]);
	if (!stack)
		quit("OOM stack");

	/*
	 * El estado de la máquina. Son 3 punteros, empezando con
	 * el programa inicial, y stack y entornos vacíos.
	 */
	code c = init_c;
	env e = NULL;
	value *s = stack;

	/*
	 * Usando la pila como un verdadero C Hacker
	 * ==========================================
	 *
	 * El puntero `s` apunta siempre una (1) dirección más adelante del
	 * último elemento de la pila, i.e. a la primera dirección libre.
	 * Esto significa que podemos acceder al elemento en la cima de la pila
	 * con s[-1]. El anteúltimo elemento está en s[-2], etc.
	 *
	 * Para agregar un valor v a la pila hacemos:
	 *
	 *   *s++ = v;
	 *
	 * Esto es igual a *s = v; s = s + 1. Simétricamente,
	 * para sacar un valor de la pila, hacemos:
	 *
	 *   v = *--s;
	 *
	 * Que es igual a s = s - 1; v = *s
	 */

	while (1) {
		/* Si se llenó la pila, duplicamos su tamaño. */
		if (s == stack + stack_size) {
			int offset = s - stack;

			stack_size *= 2;
			value *new = GC_realloc(stack, stack_size * sizeof stack[0]);
			if (!new)
				quit("OOM stack grow");
			stack = new;

			s = stack + offset;
		}

		/* Tracing: sólo activado condicionalmente */
		if (TRACE) {
			int n = 0;
			code cc;
			fprintf(stderr, "code offset = %li\n", c - init_c);
			fprintf(stderr, "code -> [");
			for (cc = c; n < 20 && *cc != STOP; n++, cc++) {
				fprintf(stderr, "%i ", *cc);
			}
			if (n == 20)
				fprintf(stderr, "...]\n");
			else
				fprintf(stderr, "%i]\n", STOP);

			fprintf(stderr, "*c = %d\n", *c);
			fprintf(stderr, "|s| = %ld\n", s - stack);
			fprintf(stderr, "|e| = %d\n", env_len(e));
		}

		/* Consumimos un opcode y lo inspeccionamos. */
		switch(*c++) {
		case ACCESS: {
			/* implementame */
			abort();
		}

		case CONST: {
			/* Una constante: la leemos y la ponemos en la pila */
			(*s++).i = *c++;
			break;
		}

		case ADD: {
			/*
			 * Suma: desapilamos los dos operandos, sumamos,
			 * y apilamos el resultado.
			 */
			uint32_t y = (*--s).i;
			uint32_t x = (*--s).i;
			(*s++).i = x+y;
			break;
		}

		case SUB: {
			/*
			 * Resta: ya tenemos los valores en el tope de la pila,
			 * hacemos la resta solo si x > y, sino es 0.
			 */
			uint32_t y = (*--s).i;
			uint32_t x = (*--s).i;
			(*s++).i = x > y ? x-y : 0;
			break;
		}

		case RETURN: {
			/*
			 * Return: tenemos en la pila un valor y una dirección,
			 * de retorno (junto a su entorno). Saltamos a la
			 * dirección de retorno y a su entorno, pero dejamos el
			 * valor de retorno en la pila.
			 */
			value rv = *--s;

			struct clo ret_addr = (*--s).clo;

			e = ret_addr.clo_env;
			c = ret_addr.clo_body;

			*s++ = rv;
			break;
		}

		case CALL: {
			/*
			 * Aplicación: tenemos en la pila un argumento
			 * y una función. La función debe ser una clausura.
			 * La idea es saltar a la clausura extendiendo su
			 * entorno con el valor de la aplicación, pero
			 * tenemos que guardar nuestra dirección de retorno.
			 */
			value arg = *--s;
			value fun = *--s;

			struct clo ret_addr = { .clo_env = e, .clo_body = c };
			(*s++).clo = ret_addr;

			/* Cambiamos al entorno de la clausura, agregando arg */
			e = env_push(fun.clo.clo_env, arg);

			/* Saltamos! */
			c = fun.clo.clo_body;

			break;
		}

		case TAILCALL: {
			/* implementame */
			abort();
		}

		case FUNCTION: {
			/*
			 * Un lambda, es un valor! Armamos una clausura
			 * la ponemos en la pila, y listo!
			 *
			 * La parte tramposa es que el cuerpo del lambda
			 * puede tener cualquier longitud y tenemos que saber
			 * dónde seguir evaluando. Nuestro bytecode
			 * incluye la longitud del cuerpo del lambda en
			 * el entero siguiente, así que lo consumimos.
			 */
			int leng = *c++;

			/* Ahora sí, armamos la clausura */
			struct clo clo = {
				.clo_env = e,
				.clo_body = c,
			};

			/* La ponemos en la pila */
			(*s++).clo = clo;

			/* Y saltamos de largo el cuerpo del lambda */
			c += leng;

			break;
		}

		case FIX: {
			/*
			 * Fixpoint: algo de magia. Tenemos una clausura en
			 * la pila, donde su primer variable libre es el
			 * binding recursivo. La modificamos para que el
			 * entorno se apunte a sí mismo.
			 */
			value clo = *--s;
			env env_fix;

			/* Atar el nudo! */
			env_fix = env_push(e, clo);
			(clo.clo).clo_env = env_fix;
			env_fix->v = clo;

			(*s++) = clo;

			break;
		}

		case STOP: {
			return;
		}

		case SHIFT: {
			/* implementame */
			abort();
		}

		case DROP: {
			/* implementame */
			abort();
		}

		case PRINTN: {
			uint32_t i = s[-1].i;
			wprintf(L"%" PRIu32 "\n", i);
			break;
		}

		case PRINT: {
			wchar_t wc;
			while ((wc = *c++))
				putwchar(wc);

			break;
		}

		default:
			quit("FATAL: opcode no reconocido: %d", *(c-1));
		}
	}
}

/*
 * main simplemente llama al intérprete sobre el código que hay en el
 * archivo argv[1]. Para ser más piolas, en vez de hacer malloc, leer
 * el código del archivo a ese buffer y saltar ahí, usamos memory
 * mapping. Le decimos al kernel que nos dé un puntero a los contenidos
 * del archivo, y se leen automáticamente a medida que se necesitan.
 * Si el bytecode fuera realmente grande, esta laziness puede ser
 * conveniente.
 */
int main(int argc, char **argv)
{
	code codeptr;
	struct stat sb;
	int fd;

	GC_INIT();

	setlocale(LC_ALL, "");

	if (argc != 2) {
		fprintf(stderr, "Uso: %s archivo.bc\n", argv[0]);
		exit(EXIT_FAILURE);
	}

	fd = open(argv[1], O_RDONLY);
	if (fd < 0) {
		perror(argv[1]);
		exit(EXIT_FAILURE);
	}

	/* Obtenemos el tamaño del archivo. */
	if (fstat(fd, &sb) < 0)
		quit("fstat");

	/* Mapeamos el archivo en memoria */
	codeptr = mmap(NULL, sb.st_size, PROT_READ, MAP_PRIVATE, fd, 0);
	if (!codeptr)
		quit("mmap");

	/* Llamamos a la máquina */
	run(codeptr);

	return 0;
}
