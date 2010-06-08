TARGET = Relazione.pdf

DIADIR = diagrammi

DIA_FILES := $(wildcard $(DIADIR)/*.dia)
PDF_FILES := $(DIA_FILES:.dia=.pdf)
PNG_FILES := $(wildcard $(DIADIR)/*.png)

ifeq ($(V),1)
Q =
NQ = @:
else
Q = @
NQ = @echo
endif

all: $(TARGET)

$(TARGET): $(TARGET:.pdf=.tex) $(PDF_FILES) $(PNG_FILES)
	$(NQ) "running pdflatex on" $<
	$(Q) pdflatex $<
	$(Q) pdflatex $<
	$(Q) pdflatex $<

$(DIADIR)/%.pdf: $(DIADIR)/%.dia
	$(NQ) "converting" $< "to" $@
	$(Q) dia -e $@ $<
	$(Q) pdftops -eps $@ - | ps2eps > $(@:.pdf=.eps)
	$(Q) epstopdf $(@:.pdf=.eps)
	$(Q) rm -f $(@:.pdf=.eps)

clean:
	$(Q) rm -f *~
	$(Q) rm -f *.pdf *.dvi *.aux *.bbl *.lof *.log *.lot *.out *.toc

distclean: clean
	$(Q) rm -f $(PDF_FILES)

.PHONY: all clean
