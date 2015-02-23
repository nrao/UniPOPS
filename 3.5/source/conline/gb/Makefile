# @(#)Makefile	5.3 11/02/94
#
ARCH = `uname -m`
INCLUDES = $(popsdir)includes/gb/
INCS = $(popsdir)includes/
CCMP = cc
CC = $(CCMP)
CDEBOROPT = -g
FDEBOROPT = -g
FLOATOPT =
FILLIB = /usr/lang/SC1.0/libm.il
CILLIB = /usr/lib/libm.il
FFLOAT =  $(FDEBOROPT) $(FLOATOPT) $(FILLIB)
CFLOAT =  $(CDEBOROPT) $(FLOATOPT) $(CILLIB)
FFLAGS = -Nn1500 -i2 -DI2ON -u $(FFLOAT) -D$(ARCH)
#	remove the -DI2ON if your remove the -i2 flag
CFLAGS = $(CFLOAT) -I$(INCLUDES) -I$(INCS) -D$(ARCH) -DUSEPAD

BINLIST = data_client.o

all:	$(BINLIST)

clean:
	rm -f *.o
	-sccs clean

data_client.o: $(INCLUDES)access.h

