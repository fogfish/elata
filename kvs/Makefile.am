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
   src/kvs.erl \
   src/kvs_app.erl \
   src/kvs_sup.erl \
   src/kvs_sys.erl \
   src/kvs_evt.erl \
   src/kvs_evt_sup.erl \
   src/kvs_evt_handler_sup.erl \
   src/gen_kvs.erl \
   src/kvs_ets.erl \
   src/kvs_dets.erl \
   src/kvs_cache_sup.erl \
   src/kvs_cache.erl \
   src/kvs_rrd.erl \
   src/kvs_fed_cat.erl \
   src/kvs_fed_cat_sup.erl \
   src/kvs_fed.erl \
   src/kvs_ht.erl \
   src/kvs_sync_ht_tx.erl \
   src/kvs_sync_ht_tx_sup.erl


EBIN  = $(addprefix ebin/, $(notdir $(ESRC:.erl=.beam)))
EBIN += ebin/kvs.app

nobase_pkgliberl_SCRIPTS = $(EBIN)
CLEANFILES = $(EBIN)
EXTRA_DIST = $(ESRC)

include $(top_builddir)/Makefile.erlang

