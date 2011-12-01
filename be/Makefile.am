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
   src/be.erl \
   src/be_app.erl \
   src/be_sup.erl \
   src/be_sync.erl \
   src/be_image.erl \
   src/be_rrd.erl

EBIN  = $(addprefix ebin/, $(notdir $(ESRC:.erl=.beam)))
EBIN += \
   ebin/sys.config \
   ebin/be.app

EDAT = \
   priv/availability.image \
   priv/http.image \
   priv/icon.image \
   priv/latency.image \
   priv/tcp.image \
   priv/uri.image

nobase_pkgliberl_SCRIPTS = $(EBIN) $(EDAT)
CLEANFILES = $(EBIN) 
EXTRA_DIST = $(ESRC) $(EDAT)

include $(top_builddir)/Makefile.erlang

