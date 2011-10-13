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
   src/agt_proc.erl \
   src/agt_proc_sup.erl

EBIN  = $(addprefix ebin/, $(notdir $(ESRC:.erl=.beam)))
EBIN += \
   priv/sys.config \
   priv/elata_agt \
   ebin/agt.app

#   ebin/elata_agt.rel \
#   ebin/elata_agt.script \
#   ebin/elata_agt.boot

nobase_pkgliberl_SCRIPTS = $(EBIN) $(ESRC)
#nobase_pkgliberl_SCRIPTS = $(EBIN) $(ESRC) $(distdir).tgz
CLEANFILES = $(EBIN) 
#CLEANFILES = $(EBIN) $(distdir).tgz 
EXTRA_DIST = $(ESRC)

rel: $(distdir).tgz ebin/agt.boot


include $(top_builddir)/Makefile.erlang

