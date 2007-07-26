C---------------------------------------------------------------------- 
C----------------------------------------------------------------------
      PARAMETER (NE=2, NX=6)
C---------------------------------------------------------------------- 
C---------------------------------------------------------------------- 
C     brf  :  A parabolic PDE (the Brusselator)
C---------------------------------------------------------------------- 
C---------------------------------------------------------------------- 
C     (Discretized in space by fourth order finite differences)
C---------------------------------------------------------------------- 
C----------------------------------------------------------------------
C NOTE: The values of the constants NE and NX are defined above.
C       If they are changed then the equations-file brf.f must 
C       be rewritten with an editor or with the GUI Write button.
C
C      NE  :  the dimension of the PDE system
C      NX  :  the number of space intervals for the discretization 
C
C The AUTO-constant NDIM must be set equal to the value of NE*(NX-1)
C---------------------------------------------------------------------- 
C---------------------------------------------------------------------- 

