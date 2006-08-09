%!PS-Adobe-1.0 EPSF-2.0
%%DocumentFonts: Courier
%%Title: pstek prolog file, version 1.0 @(#)pstek.pro	1.10
%%Creator: Michael Fischbein
%% Copyright 1987 Michael Fischbein.  Commercial reproduction prohibited;
%% non-profit reproduction and distribution encouraged.
%%CreationDate: %?% 5 June 1987
%%For: tektronics-to-PS converter
%%BoundingBox:  0 395 560 740
%%EndComments
gsave
-30 800 translate -90 rotate
0.75 0.75 scale
4 setlinewidth

% Font definitions (make 3/4 functions to avoid scaling if not needed)
/FntH /Courier findfont 100 scalefont def
/DFntL { /FntL /Courier findfont 100 scalefont def } def
/DFntM { /FntM /Courier findfont 70 scalefont def } def
/DFntS { /FntS /Courier findfont 60 scalefont def } def

% tektronix line styles
/NV { [] 0 setdash } def	% normal vectors
/DV { [8] 0 setdash } def	% dotted vectors
/DDV { [8 8 32 8] 0 setdash } def	% dot-dash vectors
/SDV { [32 8] 0 setdash } def	% short-dash vectors
/LDV { [64 8] 0 setdash } def	% long-dash vectors

% Defocussed Z axis and Focussed Z axis
/DZ { .5 setgray } def
/FZ {  0 setgray } def

/PR	% char x y -> -  prints character
{	moveto show } def

/NP	% - -> - new page
% change default scale and orientation to match tek's
{	572 40 translate	% leave a border
	90 rotate
	% .71707 .692308 scale	% 0-1023X, 0-780Y
	.1730769 .17626953 scale	%0-4096X, 0-3120Y
} def

/DP	% tsizey -> - erase and home
{	clippath 1 setgray fill
	0 setgray
	0 exch moveto
} def

FntH  setfont

NP

