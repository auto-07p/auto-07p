! wrapper functions for user files written in C

module user_c

  use, intrinsic :: iso_c_binding, only: c_int, c_double, c_funptr, c_char
  use support

  implicit none
  private
  public :: user, func_c, bcnd_c, icnd_c, stpnt_c, fopt_c, pvls_c
  public :: auto_set_user, auto_reset_user

  type, bind(c) :: user_function_list
     type(c_funptr) :: func, stpnt, bcnd, icnd, fopt, pvls
     integer(c_int) :: uses_fortran
  end type user_function_list

  type(user_function_list), bind(c) :: user

  abstract interface
     subroutine func_c(ndim, u, icp, par, ijac, f, dfdu, dfdp) bind(c)
       import c_int, c_double
       implicit none
       integer(c_int), value :: ndim, ijac
       integer(c_int), intent(in) :: icp(*)
       real(c_double), intent(in) :: u(ndim), par(*)
       real(c_double), intent(out) :: f(ndim)
       real(c_double), intent(inout) :: dfdu(ndim,ndim), dfdp(ndim,*)
     end subroutine func_c

     subroutine stpnt_c(ndim, t, u, par) bind(c)
       import c_int, c_double
       implicit none
       integer(c_int), value :: ndim
       real(c_double), value :: t
       real(c_double), intent(inout) :: u(ndim), par(*)
     end subroutine stpnt_c

     subroutine bcnd_c(ndim, par, icp, nbc, u0, u1, ijac, fb, dbc) bind(c)
       import c_int, c_double
       implicit none
       integer(c_int), value :: ndim, nbc, ijac
       integer(c_int), intent(in) :: icp(*)
       real(c_double), intent(in) :: par(*), u0(ndim), u1(ndim)
       real(c_double), intent(out) :: fb(nbc)
       real(c_double), intent(inout) :: dbc(nbc, *)
     end subroutine bcnd_c

     subroutine icnd_c(ndim, par, icp, nint, u, uold, udot, upold, ijac, fi, dint) bind(c)
       import c_int, c_double
       implicit none
       integer(c_int), value :: ndim, nint, ijac
       integer(c_int), intent(in) :: icp(*)
       real(c_double), intent(in) :: par(*), u(ndim), uold(ndim)
       real(c_double), intent(in) :: udot(ndim), upold(ndim)
       real(c_double), intent(out) :: fi(nint)
       real(c_double), intent(inout) :: dint(nint, *)
     end subroutine icnd_c

     subroutine fopt_c(ndim, u, icp, par, ijac, fs, dfdu, dfdp) bind(c)
       import c_int, c_double
       implicit none
       integer(c_int), value :: ndim, ijac
       integer(c_int), intent(in) :: icp(*)
       real(c_double), intent(in) :: u(ndim), par(*)
       real(c_double), intent(out) :: fs
       real(c_double), intent(inout) :: dfdu(ndim), dfdp(*)
     end subroutine fopt_c

     subroutine pvls_c(ndim, u, par) bind(c)
       import c_int, c_double
       implicit none
       integer(c_int), value :: ndim
       real(c_double), intent(in) :: u(ndim)
       real(c_double), intent(inout) :: par(*)
     end subroutine pvls_c
  end interface

  interface
     double precision function getp(code,ic,u)
       character(3), intent(in) :: code
       integer, intent(in) :: ic
       double precision, intent(in) :: u(*)
     end function getp
  end interface

contains

  real(c_double) function getp_c(code,ic,u) bind(c)
     character(c_char) :: code(3)
     integer(c_int), value :: ic
     real(c_double) :: u(*)

     character(3) :: fcode
     fcode(1:1) = code(1)
     fcode(2:2) = code(2)
     fcode(3:3) = code(3)
     getp_c = getp(fcode, ic, u)
  end function getp_c

  ! ---- Runtime user-function registration (W4) --------------------------
  ! Fill or clear the module-level `user` table from C/Python at runtime, so
  ! the Python build needs no compiled equation object.  Compiled-C users are
  ! unaffected: their own `const user` definition still wins at link time and
  ! never calls these setters.
  subroutine auto_set_user(ul) bind(c, name="auto_set_user")
     type(user_function_list), intent(in) :: ul
     user = ul                     ! copies the c_funptr fields + uses_fortran
  end subroutine auto_set_user

  subroutine auto_reset_user() bind(c, name="auto_reset_user")
     use, intrinsic :: iso_c_binding, only: c_null_funptr
     user%func  = c_null_funptr
     user%stpnt = c_null_funptr
     user%bcnd  = c_null_funptr
     user%icnd  = c_null_funptr
     user%fopt  = c_null_funptr
     user%pvls  = c_null_funptr
     user%uses_fortran = 0
  end subroutine auto_reset_user

end module user_c

subroutine func(ndim,u,icp,par,ijac,f,dfdu,dfdp)
  use user_c, only: user, func_c
  use, intrinsic :: iso_c_binding, only: c_f_procpointer
  integer, intent(in) :: ndim, ijac, icp(*)
  double precision, intent(in) :: u(ndim), par(*)
  double precision, intent(out) :: f(ndim)
  double precision, intent(inout) :: dfdu(ndim,ndim),dfdp(ndim,*)

  procedure(func_c), pointer :: func_f
  call c_f_procpointer(user%func, func_f)
  call func_f(ndim,u,icp,par,ijac,f,dfdu,dfdp)
end subroutine func

subroutine stpnt(ndim,u,par,t)
  use user_c, only: user, stpnt_c
  use, intrinsic :: iso_c_binding, only: c_f_procpointer
  integer, intent(in) :: ndim
  double precision, intent(inout) :: u(ndim),par(*)
  double precision, intent(in) :: t

  procedure(stpnt_c), pointer :: stpnt_f
  call c_f_procpointer(user%stpnt, stpnt_f)
  call stpnt_f(ndim,t,u,par)
end subroutine stpnt

subroutine bcnd(ndim,par,icp,nbc,u0,u1,fb,ijac,dbc)
  use user_c, only: user, bcnd_c
  use, intrinsic :: iso_c_binding, only: c_f_procpointer
  integer, intent(in) :: ndim, icp(*), nbc, ijac
  double precision, intent(in) :: par(*), u0(ndim), u1(ndim)
  double precision, intent(out) :: fb(nbc)
  double precision, intent(inout) :: dbc(nbc,*)

  procedure(bcnd_c), pointer :: bcnd_f
  call c_f_procpointer(user%bcnd, bcnd_f)
  call bcnd_f(ndim,par,icp,nbc,u0,u1,ijac,fb,dbc)
end subroutine bcnd

subroutine icnd(ndim,par,icp,nint,u,uold,udot,upold,fi,ijac,dint)
  use user_c, only: user, icnd_c
  use, intrinsic :: iso_c_binding, only: c_f_procpointer
  integer, intent(in) :: ndim, icp(*), nint, ijac
  double precision, intent(in) :: par(*),u(ndim),uold(ndim)
  double precision, intent(in) :: udot(ndim),upold(ndim)
  double precision, intent(out) :: fi(nint)
  double precision, intent(inout) :: dint(nint,*)

  procedure(icnd_c), pointer :: icnd_f
  call c_f_procpointer(user%icnd, icnd_f)
  call icnd_f(ndim,par,icp,nint,u,uold,udot,upold,ijac,fi,dint)
end subroutine icnd

subroutine fopt(ndim,u,icp,par,ijac,fs,dfdu,dfdp)
  use user_c, only: user, fopt_c
  use, intrinsic :: iso_c_binding, only: c_f_procpointer
  integer, intent(in) :: ndim, icp(*), ijac
  double precision, intent(in) :: u(ndim), par(*)
  double precision, intent(out) :: fs
  double precision, intent(inout) :: dfdu(ndim),dfdp(*)

  procedure(fopt_c), pointer :: fopt_f
  call c_f_procpointer(user%fopt, fopt_f)
  call fopt_f(ndim,u,icp,par,ijac,fs,dfdu,dfdp)
end subroutine fopt

subroutine pvls(ndim,u,par)
  use user_c, only: user, pvls_c
  use, intrinsic :: iso_c_binding, only: c_f_procpointer
  integer, intent(in) :: ndim
  double precision, intent(in) :: u(ndim)
  double precision, intent(inout) :: par(*)

  procedure(pvls_c), pointer :: pvls_f
  call c_f_procpointer(user%pvls, pvls_f)
  call pvls_f(ndim,u,par)
end subroutine pvls
