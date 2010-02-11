TARGET = Relazione.pdf

LATEX_SOURCES := $(wildcard *.tex)

ifeq ($(V),1)
Q =
NQ = @:
else
Q = @
NQ = @echo
endif

all: $(TARGET)

%.pdf: %.tex $(LATEX_SOURCES)
	$(NQ) "running pdflatex on" $<
	$(Q) pdflatex $<
	$(Q) pdflatex $<
	$(Q) pdflatex $<

clean:
	$(Q) rm -f *~ *.pdf *.dvi *.aux *.bbl *.log *.lot *.out *.toc

.PHONY: all clean
