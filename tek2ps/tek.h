/* t2ps header file @(#)tek.h	1.10 (Copyright) Michael Fischbein */
/* Copyright 1987 Michael Fischbein.  Commercial reproduction prohibited; */
/* non-profit reproduction and distribution encouraged. */
#define XDIM 4096
#define YDIM 3120

#define FALSE 0
#define TRUE (~FALSE)

/* 4014 modes */
#define ALPHA 0
#define GRAPH 1
#define INCRE 2
#define LCEMD 3
#define PTPLT 4

/* type sizes (char/line, or lines/page)  (for 12bit) */
#define CHUGEX 55
#define CLARGEX 51
#define CMEDX 34
#define CSMALLX 31
#define CHUGEY 89
#define CLARGEY 82
#define CMEDY 54
#define CSMALLY 49

/* ASCII */
#define NUL 0
#define SOH 1
#define STX 2
#define ETX 3
#define EOT 4
#define ENQ 5
#define ACK 6
#define BEL 7
#define BS  8
#define HT  9
#define LF  10
#define VT  11
#define FF  12
#define NL  13
#define CR  13
#define SO  14
#define SI  15
#define DLE 16
#define DC1 17
#define DC2 18
#define DC3 19
#define DC4 20
#define NAK 21
#define SYN 22
#define ETB 23
#define CAN 24
#define EM  25
#define SUB 26
#define ESC 27
#define FS  28
#define GS  29
#define RS  30
#define US  31
#define SPACE 32
#define DEL 127

