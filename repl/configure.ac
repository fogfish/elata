dnl
dnl   Copyright (c) 2011, Nokia Corporation
dnl   All Rights Reserved.
dnl
dnl   The contents of this file are subject to the 3-clause BSD License,
dnl   (the "License"); you may not use this file except in compliance 
dnl   with the License. You should have received a copy of the 3-clause
dnl   BSD Licensee along with this software. If not, it can be
dnl   retrieved online at http://www.opensource.org/licenses/BSD-3-Clause.
dnl
dnl   Software distributed under the License is distributed on an "AS IS"
dnl   basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
dnl   the License for the specific language governing rights and limitations
dnl   under the License.
dnl
AC_INIT([elata_repl], [0.1.0], [dmitry.kolesnikov@nokia.com])
AM_INIT_AUTOMAKE

AC_ARG_WITH(erlang,  [--with-erlang=PREFIX path to erlang runtime])
   
dnl check erlang runtime
erlang_path=$with_erlang:$with_erlang/bin:$PATH
AC_PATH_PROG([ERLC], [erlc], [], [$erlang_path])
AC_PATH_PROG([ERL],  [erl],  [], [$erlang_path])
AC_PATH_PROG([PERL], [perl])

dnl rrdtools
AC_PATH_PROG([RRDTOOL],   [rrdtool])
if test "x$RRDTOOL" == "x" ; then
   AC_WARN([
   -------------------------------------------------------------------------
   
   rrdtool is not found, data stream persistency is off
   
   -------------------------------------------------------------------------
   ])
   AM_CONDITIONAL(HAVE_RRD, [false])
else
   AM_CONDITIONAL(HAVE_RRD, [true])
fi

dnl check erlang directories
AC_MSG_CHECKING([Library dir])
liberldir=`$ERL -noshell -eval 'io:format("~s", [[code:lib_dir()]]),halt(0)'`
pkgliberldir=$liberldir/[$PACKAGE]-[$VERSION]
AC_SUBST(liberldir)
AC_SUBST(pkgliberldir)
AC_MSG_RESULT([$pkgliberldir])

AC_OUTPUT( \
   Makefile           \
   test/Makefile      \
)
