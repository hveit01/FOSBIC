CC=gcc
FC=gfortran
FOPTS=-fdefault-real-8 -fdefault-integer-8 -ffixed-form -std=legacy
DEL=rm

all: fosbic asa


%o: %for
	$(FC) $(FOPTS) -c $<

IGNORE=overt.o
OBJS=check.o clear.o comerr.o exerr.o findfi.o main.o mattra.o prilin.o string.o\
	 subinv.o zalph.o zconvn.o zdigit.o zeval.o zexec.o zexfil.o zexmat.o zfile.o\
	 zhoppr.o zimage.o zinitl.o zinsno.o zklam.o zliste.o znumb.o ztranx.o


clean:
	-$(DEL) fosbic$(EXE)
	-$(DEL) asa$(EXE)
	-$(DEL) $(OBJS)

fosbic: $(OBJS)
	$(FC) $(FOPTS) $(OBJS) -o fosbic

asa: asa.c
	$(CC) asa.c -o asa