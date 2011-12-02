#
#   Copyright (c) 2011, Nokia Corporation
#   All Rights Reserved.
#
#   The contents of this file are subject to the 3-clause BSD License,
#   (the "License"); you may not use this file except in compliance 
#   with the License. You should have received a copy of the 3-clause
#   BSD Licensee along with this software. 
#
#   Software distributed under the License is distributed on an "AS IS"
#   basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
#   the License for the specific language governing rights and limitations
#   under the License.
#
SUBDIRS = test

ESRC = \
   src/agt.erl \
   src/agt_app.erl \
   src/agt_sup.erl \
   src/agt_proc.erl

EBIN  = $(addprefix ebin/, $(notdir $(ESRC:.erl=.beam)))
EBIN += \
   ebin/sys.config \
   ebin/agt.app

nobase_pkgliberl_SCRIPTS = $(EBIN) $(ESRC)
CLEANFILES = $(EBIN) 
EXTRA_DIST = $(ESRC)

include $(top_builddir)/Makefile.erlang

