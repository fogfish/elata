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
AC_INIT([elata], [0.4.1], [dmitry.kolesnikov@nokia.com])
AM_INIT_AUTOMAKE

AC_PATH_PROG([PDFTEX], [pdflatex])

AC_ARG_WITH(erlang,  [--with-erlang=PREFIX path to erlang runtime])
dnl check erlang runtime
erlang_path=$with_erlang:$with_erlang/bin:$PATH
AC_PATH_PROG([ERLC], [erlc], [], [$erlang_path])
AC_PATH_PROG([ERL],  [erl],  [], [$erlang_path])
AC_PATH_PROG([PERL], [perl])


AC_CONFIG_SUBDIRS(ek)
AC_CONFIG_SUBDIRS(emc)
AC_CONFIG_SUBDIRS(kvs)
AC_CONFIG_SUBDIRS(agt)
AC_CONFIG_SUBDIRS(be)
AC_CONFIG_SUBDIRS(fe)

AC_OUTPUT(
   Makefile     \
   doc/Makefile \
)
