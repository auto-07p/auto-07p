# 
#  This file has to be sourced before activating AUTO if you are using
#  a 'sh' compatible shell, such as sh, bash, ksh, or ash.
#
AUTO_DIR=$HOME/auto/07p
PATH=$HOME/gcc-trunk/bin:$AUTO_DIR/cmds:$AUTO_DIR/bin:$PATH
export LD_LIBRARY_PATH=$HOME/gcc-trunk/lib64
# the following is an example (to be uncommented) for Windows+MSYS:
#PATH="/c/Python24:/c/Program Files/gfortran/bin:$PATH"
export AUTO_DIR
export PATH
#
# DON'T ADD ANYTHING AFTER THIS LINE
#
