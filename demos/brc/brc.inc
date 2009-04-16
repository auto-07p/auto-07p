!---------------------------------------------------------------------- 
!----------------------------------------------------------------------
      PARAMETER (NE=2, NN=6)
!---------------------------------------------------------------------- 
!---------------------------------------------------------------------- 
!     pde  :  A parabolic PDE (the Brusselator)
!---------------------------------------------------------------------- 
!---------------------------------------------------------------------- 
! (Discretized in space by polynomial collocation at Chebyshev points)
!---------------------------------------------------------------------- 
!----------------------------------------------------------------------
! NOTE: The values of the constants NE and NN are defined above.
!       If they are changed then the equations file brc.f must 
!       be rewritten with an editor or with the GUI Write button.
!
!      NE  :  the dimension of the PDE system
!      NN  :  the number of Chebyshev collocation points in space 
!
! The AUTO-constant NDIM must be set equal to the value of NE*NN
!---------------------------------------------------------------------- 
!---------------------------------------------------------------------- 

