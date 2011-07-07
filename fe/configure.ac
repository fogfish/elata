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
AC_INIT([elata_fe], [0.3.3], [dmitry.kolesnikov@nokia.com])
AM_INIT_AUTOMAKE

AC_ARG_WITH(erlang,       [--with-erlang=PREFIX path to erlang runtime])
AC_ARG_WITH(yuicompressor,[--with-yuicompressor=PATH path to yuicompressor-x.y.z.jar, available on-line at http://yuilibrary.com/downloads])

dnl check erlang runtime
erlang_path=$with_erlang:$with_erlang/bin:$PATH
AC_PATH_PROG([ERLC], [erlc], [], [$erlang_path])
AC_PATH_PROG([ERL],  [erl],  [], [$erlang_path])
AC_PATH_PROG([PERL], [perl])


if test "x$with_yuicompressor" == "x" ; then
   AC_WARN([
-------------------------------------------------------------------------
 
   compression of JavaScript and Style-sheet is disabled
 
-------------------------------------------------------------------------
   ])
   AM_CONDITIONAL(HAVE_YUICOMPRESSOR, [false])
else
   AM_CONDITIONAL(HAVE_YUICOMPRESSOR, [true])
fi
AC_SUBST([with_yuicompressor])


dnl check erlang directories
AC_MSG_CHECKING([Library dir])
liberldir=`$ERL -noshell -eval 'io:format("~s", [[code:lib_dir()]]),halt(0)'`
pkgliberldir=$liberldir/[$PACKAGE]-[$VERSION]
AC_SUBST(liberldir)
AC_SUBST(pkgliberldir)
AC_MSG_RESULT([$pkgliberldir])

AC_OUTPUT( \
   Makefile      \
)
