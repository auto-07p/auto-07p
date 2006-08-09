#===> sgi.makefile
#
#   Makefile written by Xianjun WANG in March 1994
#
#   This Makefile is used to compile the Graphic User Interface(GUI)
#   source code for Silicon Graphics machines.
#   
#   Type "make -f sgi.makefile" to compile
#
.SUFFIXES: .c .o
#
#  if using gnu gcc, uncomment the following line
#CC    = gcc -O
#
CC     = cc -Wf,-XNl10240 -O
#
# the following line defines the path where X11 and Motif header files
# were installed. If X11 and Motif was installed in a different path
# this following path should be modified to locate the them (X11 and Motif 
# header files )
#
INC_DIR=/usr/include

# the following line defines the path where X11 and Motif library files
# were installed. If the library files for X11 and Motif was installed in 
# a different path, this following path should be modified to locate the them 
# as before
#
LIB_DIR=/usr/lib
#
#CFLAGS = -DSGI -cckr -I/usr/include/sun -I/usr/include/bsd  -c
CFLAGS = -DSGI -cckr -I../include $(INC_DIR) -c
#SRC    = auto97.c 
OBJ    = auto97.o
PGM    = ../bin/AUTO97
RM     = rm -f
#LIBS   = -lsun -lXm -lXt -lX11 -lPW
LIBS   = -lXm -lXt -lX11 -lPW
#
.c.o:
	$(CC) $(CFLAGS) $*.c -o $@
#
$(PGM): $(OBJ)
	$(CC) $(OBJ) -o $@ -L$(LIB_DIR) $(LIBS)
#
clean:  
	$(RM) *.o *~

