PDF = elata-user-manual.pdf

pkgdata_DATA = $(PDF)
CLEANFILES = $(PDF) $(PDF:.pdf=.log) $(PDF:.pdf=.aux) $(PDF:.pdf=.synctex.gz)

%.pdf : %.tex
	$(PDFTEX) $^ > /dev/null
