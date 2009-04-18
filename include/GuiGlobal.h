#ifndef	_GUIGLOBAL_H
#define	_GUIGLOBAL_H

#ifndef	_NO_PROTO
#if	!(defined(__STDC__) && __STDC__) \
     && !defined(__cplusplus)		\
     && !defined(c_plusplus)		\
     && !defined(FUNCPROTO)		\
     && !defined(XTFUNCPROTO)		\
     && !defined(XMFUNCPROTO)	
#define	_NO_PROTO
#endif	/* __STDC__ */
#endif	/* _NO_PROTO */

#ifdef	__cplusplus
extern	"C"	{	/* Begin scope of extern "C" */
#endif

/* define global varibales */

/* GP<name> : Global Parameter */

GlobalPar   GP;

/* GW<widgetName> : Global Widget */

GlobalWidget GW;


enum {
  PAR_NDIM,
  PAR_NBC,                          
  PAR_NINT,
  PAR_JAC,
  PAR_NTST,     
  PAR_NCOL,                         
  PAR_IAD,
  PAR_EPSL,
  PAR_EPSU,
  PAR_EPSS,             
  PAR_ITMX,
  PAR_NWTN,
  PAR_ITNW,
  PAR_DS,
  PAR_DSMIN,
  PAR_DSMAX,
  PAR_IADS,                   
  PAR_NMX,
  PAR_RL0,
  PAR_RL1,
  PAR_A0,
  PAR_A1,
  PAR_NICP,                      
  PAR_ICP,
  PAR_ILP,
  PAR_ISP,
  PAR_ISW,               
  PAR_MXBF,
  PAR_IRS,
  PAR_IPS,
  PAR_NPR,
  PAR_IID,
  PAR_IPLT,                     
  PAR_NUZR,
  PAR_NTHL,
  PAR_NTHU
};


char *parLabel[] = {
  "NDIM",
  "NBC",                           /* problem */
  "NINT",
  "JAC",

  "NTST",     
  "NCOL",                          /* discretization */       
  "IAD",

  "EPSL",
  "EPSU",
  "EPSS",                          /* tolerance */
  "ITMX",
  "NWTN",
  "ITNW",

  "DS",
  "DSMIN",
  "DSMAX",
  "IADS",                          /* step size */

  "NMX",
  "RL0",
  "RL1",
  "A0",
  "A1",

  "NICP",                          /* continuation */
  "ICP",


  "ILP",
  "ISP",
  "ISW",                           /* run   */
  "MXBF",
  "IRS",
  "IPS",

  "NPR",
  "IID",
  "IPLT",                          /*  output  */
  "NUZR",
  "NTHL",
  "NTHU"

};

char *strHelp[] = {

"NDIM\n\
\n\
 Dimension of the system of equations as specified in the user-supplied\n\
 subroutine FUNC.",

"NBC\n\
\n\
 The number of boundary conditions as specified in the user-supplied\n\
 subroutine BCND.",

"NINT\n\
\n\
 The number of integral conditions as specified in the user-supplied\n\
 subroutine ICND.",

"JAC\n\
\n\
 Used to indicate whether derivatives are supplied by the user or to\n\
 be obtained by differencing :\n\
\n\
 JAC=0 :\n\
\n\
 No derivatives are given by the user. (Most demos use JAC=0.)\n\
\n\
 JAC=1 :\n\
\n\
 Derivatives with respect to state- and problem-parameters are given\n\
 in the user-supplied subroutines FUNC, BCND, ICND and FOPT.",

"NTST\n\
\n\
 The number of mesh intervals used for discretization. NTST remains\n\
 fixed during any particular run, but can be changed when restarting.\n\
 Recommended value of NTST : As small as possible to maintain\n\
 convergence.",

"NCOL\n\
\n\
 The number of Gauss collocation points per mesh interval (2<=NCOL<=7).\n\
 NCOL remains fixed during any given run, but can be changed when\n\
 restarting at a previously computed solution. The choice NCOL=4,\n\
 used in most demos, is recommended.",

"IAD \n\
\n\
 This constant controls the mesh adaption : \n\
 \n\
 IAD=0 :Fixed mesh. This choice should never be used, as it may result \n\
 in spurious solutions. \n\
\n\
 IAD>0 : Adapt the mesh every IAD steps along the branch. Most demos\n\
 use IAD=3, which is the strongly recommended value.",

"EPSL\n\
\n\
 Relative convergence criterion for equation parameters in the\n\
 Newton/Chord method. Most demos use EPSL=10^{-6} or EPSL=10^{-7}, \n\
 which is the recommended value range.", 

"EPSU\n\
\n\
 Relative convergence criterion for solution components in the\n\
 Newton/Chord method. Most demos use EPSU=10^{-6} or EPSU=10^{-7}, \n\
 which is the recommended value range.",

"EPSS\n\
\n\
 Relative arclength convergence criterion for detecting special \n\
 solutions. Most demos use EPSS=10^{-4} or EPSS=10^{-5}, which is\n\
 the recommended value range. Generally, EPSS should be approximately\n\
 100 to 1000 times the value of EPSL, EPSU.",
 
"ITMX\n\
\n\
 The maximum number of iterations allowed in the accurate location of\n\
 special solutions, such as bifurcations, folds, and user output points, \n\
 by Muller's method with bracketing. The recommended value is ITMX=8, \n\
 used in most demos.",

"NWTN\n\
\n\
 After NWTN Newton iterations the Jacobian is frozen, i.e., AUTO uses\n\
 full Newton for the first NWTN iterations and the Chord method for\n\
 iterations NWTN+1 to ITNW. The choice NWTN=3 is strongly recommended\n\
 and used in most demos. Note that this constant is only effective for\n\
 ODEs, i.e., for solving the piecewise polynomial collocation equations.\n\
 For algebraic systems AUTO always uses full Newton.",

"ITNW\n\
\n\
 The maximum number of combined Newton-Chord iterations. When this\n\
 maximum is reached, the step will be retried with half the stepsize.\n\
 This is repeated until convergence, or until the minimum stepsize is\n\
 reached. In the latter case the computation of the branch is\n\
 discontinued and a message printed in fort.9. The recommended value\n\
 is ITNW=5, but ITNW=7 may be used for difficult problems.",

"DS\n\
\n\
 The constant DS defines the pseudo-arclength stepsize to be used for\n\
 the first attempted step along any branch. (Note that if IADS>0 then\n\
 DS will automatically be adapted for subsequent steps and for failed\n\
 steps.) DS may be chosen positive or negative; changing its sign\n\
 reverses the direction of computation. The relation\n\
\n\
                  DSMIN < |DS| <= DSMAX\n\
\n\
 must be satisfied. The precise choice of DS is problem-dependent; the demos\n\
 use a value that was found appropriate after some experimentation.",
 
"DSMIN\n\
\n\
 This is minimum allowable absolute value of the pseudo-arclength\n\
 stepsize. DSMIN must be positive. It is only effective if the \n\
 pseudo-arclength step is adaptive, i.e., if IADS>0. The choice of\n\
 DSMIN is highly problem-dependent; most demos use a value that was\n\
 found appropriate after some experimentation.",

"DSMAX\n\
\n\
 The maximum allowable absolute value of the pseudo-arclength stepsize. \n\
 DSMAX must be positive. It is only effective if the pseudo-arclength \n\
 step is adaptive, i.e., if IADS>0. The choice of DSMAX is highly \n\
 problem-dependent; most demos use a value that was found appropriate\n\
 after some experimentation.",

"IADS\n\
\n\
 This constant controls the frequency of adaption of the\n\
 pseudo-arclength stepsize.\n\
\n\
 IADS=0 :\n\
 \n\
 Use fixed pseudo-arclength stepsize, i.e., the stepsize will be equal\n\
 to the specified value of DS for every step. The computation of a\n\
 branch will be discontinued as soon as the maximum number of\n\
 iterations ITNW is reached. This choice is not recommended.\n\
\n\
 IADS>0 :\n\
\n\
 Adapt the pseudo-arclength stepsize after every IADS steps. The \n\
 recommended value is IADS=1, which is used in almost all demos.",

"NMX\n\
\n\
 The maximum number of steps to be taken along any branch.",

"RL0\n\
\n\
 The lower bound on the principal continuation parameter. i.e., on the\n\
 parameter that appears first in the ICP list.",

"RL1\n\
\n\
 The upper bound on the principal continuation parameter.",

"A0\n\
\n\
 The lower bound on the principal solution measure. (By default, if\n\
 IPLT=0, the principal solution measure is the L_2-norm of the state \n\
 vector or state vector function.)",

"A1\n\
\n\
 The upper bound on the principal solution measure.",

"ICP\n\
\n\
 For each equation type and for each continuation calculation there is \n\
 a typical (generic) number of problem parameters that must be\n\
 allowed to vary, in order for the calculations to be properly posed. \n\
 The constant NICP indicates how many free parameters have been\n\
 specified, while the array ICP designates these free parameters. \n\
 The parameter that appears first in the ICP list is called the\n\
 principal continuation parameter. See the AUTO manual for more details.",
 
"NICP \n\
\n\
 For each equation type and for each continuation calculation there is \n\
 a typical (generic) number of problem parameters that must be\n\
 allowed to vary, in order for the calculations to be properly posed. \n\
 The constant NICP indicates how many free parameters have been\n\
 specified, while the array ICP designates these free parameters. \n\
 The parameter that appears first in the ICP list is called the\n\
 principal continuation parameter. See the AUTO manual for more details.",

"ILP\n\
\n\
 ILP=0 : No detection of folds. This choice is recommended. \n\
 \n\
 ILP=1 : Detection of folds. This setting should be used if subsequent \n\
 fold continuation is intended.",

 
"ISP\n\
\n\
 This constant controls the detection of branch points, period-doubling\n\
 bifurcations, and torus bifurcations.\n\
\n\
 ISP=0 :\n\
 \n\
 This setting disables the detection of branch points, period-doubling \n\
 bifurcations, and torus bifurcations and the computation of Floquet\n\
 multipliers. \n\
\n\
 ISP=1 :\n\
 \n\
 Branch points are detected for algebraic equations, but not for \n\
 periodic solutions and boundary value problems. Period-doubling \n\
 bifurcations and torus bifurcations are not located either. However, \n\
 Floquet multipliers are computed. \n\
\n\
 ISP=2 :\n\
\n\
 This setting enables the detection of all special solutions. \n\
\n\
 ISP=3 :\n\
\n\
 Branch points will be detected, but AUTO will not monitor the \n\
 Floquet multipliers. Period-doubling and torus bifurcations will \n\
 go undetected. This option is useful for certain problems with \n\
 non-generic Floquet behavior.",


"ISW\n\
\n\
 This constant controls branch switching at branch points for the case \n\
 of differential equations. Note that branch switching is automatic for \n\
 algebraic equations. \n\
\n\
 ISW=1 : This is the normal value of ISW. \n\
\n\
 ISW=-1 : \n\
\n\
 If IRS is the label of a branch point or a period-doubling bifurcation \n\
 then branch switching will be done. \n\
\n\
 ISW=2 : \n\
\n\
 If IRS is the label of a fold, a Hopf bifurcation point, or a\n\
 a period-doubling or torus bifurcation then a locus of such points \n\
 will be computed.",

"MXBF\n\
\n\
 This constant, which is effective for algebraic problems only, sets\n\
 the maximum number of bifurcations to be treated. Additional branch\n\
 points will be noted, but the corresponding bifurcating branches will \n\
 not be computed. If MXBF is positive then the bifurcating branches of \n\
 the first MXBF branch points will be traced out in both directions. \n\
 If MXBF is negative then the bifurcating branches of the first |MXBF| \n\
 branch points will be traced out in only one direction.",

"IRS\n\
\n\
 This constant sets the label of the solution where the computation is\n\
 to be restarted. \n\
\n\
 IRS=0 :\n\
\n\
 This setting is typically used in the first run of a new problem. \n\
\n\
 IRS>0 : \n\
\n\
 Restart the computation at the previously computed solution with label \n\
 IRS. This solution is normally expected to be in the current\n\
 data-file. Various AUTO-constants can be modified when restarting.",

"IPS\n\
\n\
 This constant defines the problem type : \n\
\n\
 IPS=0 : An algebraic bifurcation problem. \n\
\n\
 IPS=1 : Stationary solutions of ODEs with detection of Hopf bifurcations. \n\
\n\
 IPS=-1 : Fixed points of a discrete dynamical system. \n\
\n\
 IPS=-2 : Time integration using implicit Euler.\n\
\n\
 IPS=2 : Computation of periodic solutions.\n\
\n\
 IPS=4 : A boundary value problem.\n\
\n\
 IPS=5 : Algebraic optimization problems.\n\
\n\
 IPS=7 : A boundary value problem with computation of Floquet multipliers.\n\
\n\
 IPS=9 : This option is used in connection with the HomCont algorithms\n\
 for the detection and continuation of homoclinic bifurcations.\n\
\n\
 IPS=11 : Spatially uniform solutions of a system of parabolic PDEs,\n\
 with detection of bifurcations to traveling waves.\n\
\n\
 IPS=12 : Continuation of traveling wave solutions to a system of \n\
 parabolic PDEs.\n\
\n\
 IPS=14 : Time evolution for a system of parabolic PDEs subject to\n\
 periodic boundary conditions.\n\
\n\
 IPS=15 : Optimization of periodic solutions.\n\
\n\
 IPS=16 : This option is similar to IPS=14, except that user supplies\n\
 the boundary conditions.\n\
\n\
 IPS=17 : This option can be used to continue stationary solutions of\n\
 parabolic systems obtained from an evolution run with IPS=16.",

"NPR\n\
\n\
 This constant can be used to regularly write fort.8 plotting and\n\
  restart data.\n\
\n\
 IF NPR>0 then such output is written every NPR steps. \n\
\n\
 IF NPR=0 or if NPR >= NMX then no such output is written.",

"IID \n\
\n\
 This constant controls the amount of diagnostic output printed in\n\
 fort.9 : the greater IID the more detailed the diagnostic output. \n\
\n\
 IID=0 : Minimal diagnostic output. This setting is not recommended. \n\
\n\
 IID=2 : Regular diagnostic output. This value is recommended. \n\
\n\
 IID=3 : This gives additional diagnostic output for algebraic equations, \n\
  namely the Jacobian and the residual vector at the starting point. \n\
\n\
 IID=4 : This gives additional diagnostic output for differential\n\
 equations, namely the reduced system and the associated residual\n\
 vector. \n\
\n\
 IID=5 : This setting gives very extensive diagnostic output for \n\
 differential equations, namely, debug output from the linear  \n\
 equation solver. This setting should not normally be used.",

"IPLT\n\
\n\
 This constant allows redefinition of the principal solution measure,\n\
 which is printed as the second (real) column in the screen output and\n\
 in the fort.7 output-file : \n\
\n\
 If IPLT = 0 then the L_2-norm is printed. Most demos use this setting. \n\
\n\
 If 0 < IPLT <= NDIM then the maximum of the IPLT'th solution component \n\
 is printed. \n\
\n\
 If - NDIM <= IPLT <0 then the minimum of the IPLT'th solution component \n\
 is printed. \n\
\n\
 If NDIM < IPLT <= 2* NDIM then the integral of the ( IPLT- NDIM)'th\n\
 solution component is printed.\n\
\n\
 If 2* NDIM < IPLT <= 3* NDIM then the L_2-norm of the ( IPLT- NDIM)'th\n\
 solution component is printed.",

"NUZR\n\
\n\
 This constant allows the setting of parameter values at which labeled \n\
 plotting and restart information is to be written in the fort.8\n\
 output-file. Optionally, it also allows the computation to terminate \n\
 at such a point. \n\
\n\
 Set NUZR=0 if no such output is needed. Many demos use this setting. \n\
\n\
 If NUZR>0 then one must enter NUZR pairs, \n\
\n\
             <Parameter-Index> <Parameter-Value>  \n\
\n\
 If the <Parameter-Index> is preceded by a minus sign then the\n\
 computation will terminate at such a solution point.",
 
"NTHL\n\
\n\
 By default, the pseudo-arclength stepsize includes all state variables \n\
 (or state functions) and all free parameters. Under certain\n\
 circumstances one may want to modify the weight accorded to individual \n\
 parameters in the definition of stepsize. \n\
\n\
 If NTHL=0 then all weights will have default value 1.0 . \n\
\n\
 If NTHL>0 then one must enter NTHL pairs, \n\
\n\
               <Parameter-Index>    <Weight>",

"NTHU\n\
\n\
 Under certain circumstances one may want to modify the weight accorded\n\
 to individual state variables (or state functions) in the definition\n\
 of stepsize.\n\
\n\
 If NTHU=0 then all weights will have default value 1.0 . \n\
\n\
 If NTHU>0 then one must enter NTHU pairs, \n\
\n\
               <State-Index>       <Weight>"
};

char *demoItems[] = {
  "ab.f90",
  "abc.f90",
  "brc.f90",
  "brf.f90",
  "bru.f90",
  "bvp.f90",
  "chu.f90",
  "cir.f90",
  "dd2.f90",
  "enz.f90",
  "exp.f90",
  "ext.f90",
  "ezp.f90",
  "frc.f90",
  "fsh.f90",
  "int.f90",
  "ivp.f90",
  "kar.f90",
  "kpr.f90",
  "lin.f90",
  "lor.f90",
  "lrz.f90",
  "mtn.f90",
  "nag.f90",
  "non.f90",
  "obv.f90",
  "ops.f90",
  "opt.f90",
  "pd1.f90",
  "pd2.f90",
  "pen.f90",
  "phs.f90",
  "plp.f90",
  "pp2.f90",
  "pp3.f90",
  "ppp.f90",
  "pvl.f90",
  "san.f90",
  "she.f90",
  "spb.f90",
  "stw.f90",
  "tim.f90",
  "tor.f90",
  "wav.f90"
};

char *demoHelp[] = {
  "See AUTO manual for help on ab",
  "See AUTO manual for help on abc",
  "See AUTO manual for help on brc",
  "See AUTO manual for help on brf",
  "See AUTO manual for help on bru",
  "See AUTO manual for help on bvp",
  "See AUTO manual for help on chu",
  "See AUTO manual for help on cir",
  "See AUTO manual for help on dd2",
  "See AUTO manual for help on enz",
  "See AUTO manual for help on exp",
  "See AUTO manual for help on ext",
  "See AUTO manual for help on ezp",
  "See AUTO manual for help on frc",
  "See AUTO manual for help on fsh",
  "See AUTO manual for help on int",
  "See AUTO manual for help on ivp",
  "See AUTO manual for help on kar",
  "See AUTO manual for help on kpr",
  "See AUTO manual for help on lin",
  "See AUTO manual for help on lor",
  "See AUTO manual for help on lrz",
  "See AUTO manual for help on mtn",
  "See AUTO manual for help on nag",
  "See AUTO manual for help on non",
  "See AUTO manual for help on obv",
  "See AUTO manual for help on ops",
  "See AUTO manual for help on opt",
  "See AUTO manual for help on pd1",
  "See AUTO manual for help on pd2",
  "See AUTO manual for help on pen",
  "See AUTO manual for help on phs",
  "See AUTO manual for help on plp",
  "See AUTO manual for help on pp2",
  "See AUTO manual for help on pp3",
  "See AUTO manual for help on ppp",
  "See AUTO manual for help on pvl",
  "See AUTO manual for help on san",
  "See AUTO manual for help on she",
  "See AUTO manual for help on spb",
  "See AUTO manual for help on stw",
  "See AUTO manual for help on tim",
  "See AUTO manual for help on tor",
  "See AUTO manual for help on wav"
};


/* bits for exclamation point in dialog */

unsigned char warningBits[] = {

   0x00, 0x00, 0x00, 0x00, 0x00, 0xe0, 0x07, 0x00, 0x00, 0xe0, 0x07, 0x00,
   0x00, 0xe0, 0x07, 0x00, 0x00, 0xe0, 0x07, 0x00, 0x00, 0xe0, 0x07, 0x00,
   0x00, 0xe0, 0x07, 0x00, 0x00, 0xe0, 0x07, 0x00, 0x00, 0xe0, 0x07, 0x00,
   0x00, 0xe0, 0x07, 0x00, 0x00, 0xe0, 0x07, 0x00, 0x00, 0xe0, 0x07, 0x00,
   0x00, 0xe0, 0x07, 0x00, 0x00, 0xe0, 0x07, 0x00, 0x00, 0xe0, 0x07, 0x00,
   0x00, 0xe0, 0x07, 0x00, 0x00, 0xe0, 0x07, 0x00, 0x00, 0xe0, 0x07, 0x00,
   0x00, 0xe0, 0x07, 0x00, 0x00, 0xe0, 0x07, 0x00, 0x00, 0x00, 0x00, 0x00,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xc0, 0x03, 0x00,
   0x00, 0xe0, 0x07, 0x00, 0x00, 0xf0, 0x0f, 0x00, 0x00, 0xf0, 0x0f, 0x00,
   0x00, 0xf0, 0x0f, 0x00, 0x00, 0xf0, 0x0f, 0x00, 0x00, 0xe0, 0x07, 0x00,
   0x00, 0xc0, 0x03, 0x00, 0x00, 0x00, 0x00, 0x00

};

static char transTable[] = 
"Shift<Key>P:	ArmAndActivate() \n\
 Shift<Key>D:	ArmAndActivate() \n\
 Shift<Key>T:	ArmAndActivate() \n\
 Shift<Key>S:	ArmAndActivate() \n\
 Shift<Key>L:	ArmAndActivate() \n\
 Shift<Key>C:	ArmAndActivate() \n\
 Shift<Key>R:	ArmAndActivate() \n\
 Shift<Key>O:	ArmAndActivate() \n\
 Shift<Key>A:	ArmAndActivate()";


#ifdef	__cplusplus
}	/* Close scope of extern "C" */
#endif 

#endif /* _GUIGLOBAL_H */

/***	DON'T ADD ANYTHING AFTER THIS #endif	***/

