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
   src/elata_agt.erl \
   src/elata_agt_app.erl \
   src/elata_agt_sup.erl \
   src/agt_kvs_io.erl \
   src/agt_job.erl \
   src/agt_job_sup.erl \
   src/agt_probe.erl \
   src/agt_rest_api.erl \
   src/uri_util.erl

EBIN  = $(addprefix ebin/, $(notdir $(ESRC:.erl=.beam)))
EBIN += \
   priv/sys.config \
   priv/elata_agt \
   ebin/elata_agt.app \
   ebin/elata_agt.rel \
   ebin/elata_agt.script \
   ebin/elata_agt.boot \
   ebin/elata_agt.tar.gz

EINC  = include/def.hrl 

nobase_pkgliberl_SCRIPTS = $(EBIN) $(EINC) $(ESRC) $(distdir).tgz
CLEANFILES = $(EBIN) $(distdir).tgz 
EXTRA_DIST = $(ESRC)

include $(top_builddir)/Makefile.erlang

