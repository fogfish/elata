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
   src/elata_fe.erl \
   src/elata_fe_app.erl \
   src/elata_fe_sup.erl \
   src/fe_webapp_api.erl

EBIN  = $(addprefix ebin/, $(notdir $(ESRC:.erl=.beam)))
EBIN += \
   priv/sys.config   \
   priv/elata_fe \
   ebin/elata_fe.app \
   ebin/elata_fe.rel \
   ebin/elata_fe.script \
   ebin/elata_fe.boot \
   ebin/elata_fe.tar.gz \
   priv/webapp/index.html \
   priv/webapp/elata-$(VERSION).css \
   priv/webapp/elata-$(VERSION).js

EINC  = include/def.hrl

CSS   = \
   priv/webapp/css/copyright.css \
   priv/webapp/css/doc.css \
   priv/webapp/css/list.css \
   priv/webapp/css/navi.css \
   priv/webapp/css/grid.css \
   priv/webapp/css/editor.css \
   priv/webapp/css/button.css

JS    = \
   priv/webapp/js/copyright.js \
   priv/webapp/js/app.js \
   priv/webapp/js/kvset.js \
   priv/webapp/js/navigator.js \
   priv/webapp/js/grid.js \
   priv/webapp/js/locale.js \
   priv/webapp/js/mainview.js \
   priv/webapp/js/caseview.js \
   priv/webapp/js/newcaseview.js \
   priv/webapp/js/signin.js 

EXT = \
   priv/webapp/js/base64.js
   
nobase_pkgliberl_SCRIPTS = $(EBIN) $(EINC) $(EXT) $(distdir).tgz
CLEANFILES = $(EBIN) $(distdir).tgz
EXTRA_DIST = $(ESRC)

if HAVE_YUICOMPRESSOR

priv/webapp/elata-$(VERSION).css: $(CSS)
	cat $^ > /tmp/elata.css
	cat $< > $@
	java -jar $(with_yuicompressor) --type css --line-break 80 /tmp/elata.css >> $@

priv/webapp/elata-$(VERSION).js: $(JS)
	cat $^ > /tmp/elata.js
	cat $< > $@
	java -jar $(with_yuicompressor) --type js --line-break 80 /tmp/elata.js >> $@

else

priv/webapp/elata-$(VERSION).css: $(CSS)
	cat $^ > $@

priv/webapp/elata-$(VERSION).js: $(JS)
	cat $^ > $@  
	
endif

include $(top_builddir)/Makefile.erlang

priv/webapp/index.html: priv/webapp/index.html.in
	sed $(DOCONF) $^ > $@
	
