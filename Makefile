CC=gcc
CFLAGS=-g -O0

SRCS=supllib.c suvm.c

all: suplc suvm

depend: .depend

.depend: $(SRCS)
	rm -f ./.depend
	$(CC) $(CFLAGS) -MM $^ -MF  ./.depend;

include .depend

%.tab.c %.tab.h: %.y
	bison -d $<

lex.yy.c: supl.lex supl.tab.h
	flex $<

suplc: lex.yy.o supl.tab.o supllib.o
	$(CC) $(CFLAGS) -o $@ $^ -lfl

suvm: suvm.o supllib.o
	$(CC) $(CFLAGS) -o $@ $^ -lm

%.sux: %.su suplc
	./suplc $<

%.run: %.sux suvm
	./suvm $< | tee $@

clean:
	rm -f supl.tab.* lex.yy.c *.o suplc suvm *.sux *.run
