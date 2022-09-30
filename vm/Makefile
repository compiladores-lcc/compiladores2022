all: macc

%.o: CFLAGS+= -Wall -pedantic
macc: LDLIBS +=-lgc

clean:
	rm -f *.o macc

.PHONY: clean all
