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
ESRC = \
   src/elata_be.erl \
   src/elata_be_app.erl \
   src/elata_be_sup.erl \
   src/be_agt_if.erl \ 
   src/be_agt_if_sup.erl \
   src/be_static_view.erl \
   src/be_static_view_sup.erl

EBIN  = $(addprefix ebin/, $(notdir $(ESRC:.erl=.beam)))
EBIN += ebin/elata_be.app
EBIN += priv/agent.config
EBIN += priv/job.config
EBIN += priv/view.config

EINC  = include/def.hrl

nobase_pkgliberl_SCRIPTS = $(EBIN) $(EINC)
CLEANFILES = $(EBIN) 
EXTRA_DIST = $(ESRC)

include $(top_builddir)/Makefile.erlang

