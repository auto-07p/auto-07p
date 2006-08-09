#===> solaris.makefile
#
#   Makefile written by Xianjun WANG in March 1994
#   (Modified for Solaris by E. Doedel, following instructions
#    of Stan Swierz; March 1998)
#
#   Type "make -f solaris.makefile" to compile
#
.SUFFIXES: .c .o
#
CC   = cc -O
#
# the following line defines the path where X11 and Motif header files
# were installed. If X11 and Motif was installed in a different path
# this following path should be modified to locate the them (X11 and Motif 
# header files )
#
INC_DIR=$(MOTIFHOME)/include

# the following line defines the path where X11 and Motif library files
# were installed. If the library files for X11 and Motif was installed in 
# a different path, this following path should be modified to locate the them 
# as before
#
LIB_DIR=$(MOTIFHOME)/lib
#
CFLAGS = -I../include -c
#SRC = auto97.c
OBJ  = auto97.o
PGM  = ../bin/AUTO97
RM   = rm -f
LIBS = -L/enmd/IXImd12x/lib -lXm -lXt -lXext -lX11 -lsocket -lnsl -lgen
#
.c.o:
	$(CC) $(CFLAGS) -I$(INC_DIR) $*.c -o $@
#
$(PGM): $(OBJ)
	$(CC) $(OBJ) -o $@ -L$(LIB_DIR) -R$(LIB_DIR) $(LIBS)
#
clean:  
	$(RM) *.o *~

