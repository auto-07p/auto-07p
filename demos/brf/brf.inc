!---------------------------------------------------------------------- 
!----------------------------------------------------------------------
      INTEGER, PARAMETER :: NE=2, NX=6
!---------------------------------------------------------------------- 
!---------------------------------------------------------------------- 
!     brf  :  A parabolic PDE (the Brusselator)
!---------------------------------------------------------------------- 
!---------------------------------------------------------------------- 
!     (Discretized in space by fourth order finite differences)
!---------------------------------------------------------------------- 
!----------------------------------------------------------------------
! NOTE: The values of the constants NE and NX are defined above.
!       If they are changed then the equations-file brf.f must 
!       be rewritten with an editor or with the GUI Write button.
!
!      NE  :  the dimension of the PDE system
!      NX  :  the number of space intervals for the discretization 
!
! The AUTO-constant NDIM must be set equal to the value of NE*(NX-1)
!---------------------------------------------------------------------- 
!---------------------------------------------------------------------- 

