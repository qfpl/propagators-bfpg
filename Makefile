all: slides

SLIDES_TEX = slides.tex
SLIDES_PDF = slides.pdf
PDFS = SLIDES_PDF
LATEX = pdflatex -shell-escape
LATEX_SLIDES = $(LATEX) $(SLIDES_TEX)
LATEX_EXTRA = $(LATEX) $(EXTRA_TEX)
SPELL = aspell check -len_GB

.PHONY: all slides spell open edit o clean diagrams

slides: slides.pdf

slides.pdf: $(SLIDES_TEX) celsius.tex circuit.pdf_tex coloured-map.pdf_tex diagrams interval.tex screen.tex set.tex
	$(LATEX_SLIDES)
	$(LATEX_SLIDES)
	$(LATEX_SLIDES)

diagrams: diagrams/Makefile
	$(MAKE) -C diagrams

circuit.pdf_tex : circuit.svg
	inkscape -D -z --file=circuit.svg --export-pdf=circuit.pdf --export-latex

coloured-map.pdf_tex : coloured-map.svg
	inkscape -D -z --file=coloured-map.svg --export-pdf=coloured-map.pdf --export-latex

spell: $(SLIDES_TEX) $(EXTRA_TEX)
	$(SPELL) $(SLIDES_TEX)
	$(SPELL) $(SLIDES_TEX)

open: slides
	xdg-open $(SLIDES_PDF)

edit: $(SLIDES_TEX)
	vim $(SLIDES_TEX)

o: open

clean:
	rm -rf $(PDFS) *.loc *.toc *.log *.idx *.aux *.out *.nav *.snm *.vrb *.blg *.bbl *.pdf_tex
	$(MAKE) -C diagrams clean

