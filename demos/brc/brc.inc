C---------------------------------------------------------------------- 
C----------------------------------------------------------------------
      PARAMETER (NE=2, NN=6)
C---------------------------------------------------------------------- 
C---------------------------------------------------------------------- 
C     pde  :  A parabolic PDE (the Brusselator)
C---------------------------------------------------------------------- 
C---------------------------------------------------------------------- 
C (Discretized in space by polynomial collocation at Chebyshev points)
C---------------------------------------------------------------------- 
C----------------------------------------------------------------------
C NOTE: The values of the constants NE and NN are defined above.
C       If they are changed then the equations file brc.f must 
C       be rewritten with an editor or with the GUI Write button.
C
C      NE  :  the dimension of the PDE system
C      NN  :  the number of Chebyshev collocation points in space 
C
C The AUTO-constant NDIM must be set equal to the value of NE*NN
C---------------------------------------------------------------------- 
C---------------------------------------------------------------------- 

