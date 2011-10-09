#
#   Copyright (c) 2011, Nokia Corporation
#   All Rights Reserved.
#
#   The contents of this file are subject to the 3-clause BSD License,
#   (the "License"); you may not use this file except in compliance 
#   with the License. You should have received a copy of the 3-clause
#   BSD Licensee along with this software. If not, it can be
#   retrieved online at http://www.opensource.org/licenses/BSD-3-Clause.
#
#   Software distributed under the License is distributed on an "AS IS"
#   basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
#   the License for the specific language governing rights and limitations
#   under the License.
#
SUBDIRS = test

ESRC = \
   src/ek.erl \
   src/ek_uri.erl \
   src/ek_ws.erl \
   src/ek_ws_sup.erl \
   src/ek_prot.erl \
   src/ek_sup.erl \
   src/ek_echo.erl \
   src/ek_evt.erl \
   src/ek_node_sup.erl \
   src/ek_app.erl

EINC = \
   include/ek.hrl

EBIN  = $(addprefix ebin/, $(notdir $(ESRC:.erl=.beam)))
#EBIN += ebin/elata_ek.app

nobase_pkgliberl_SCRIPTS = $(EBIN) $(EINC)
CLEANFILES = $(EBIN)
EXTRA_DIST = $(ESRC)

include $(top_builddir)/Makefile.erlang

