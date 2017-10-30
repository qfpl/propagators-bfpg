all: clean slides

SLIDES_TEX = slides.tex
SLIDES_PDF = slides.pdf
LATEX = pdflatex -shell-escape
LATEX_SLIDES = $(LATEX) $(SLIDES_TEX)

.PHONY: slides spell open edit o clean

slides: slides.pdf

slides.pdf: $(SLIDES_TEX)
	$(LATEX_SLIDES)
	$(LATEX_SLIDES)
	$(LATEX_SLIDES)

spell: $(SLIDES_TEX)
	aspell check -len_GB $(SLIDES_TEX)

open: slides
	xdg-open $(SLIDES_PDF)

edit: $(SLIDES_TEX)
	vim $(SLIDES_TEX)

o: open

clean:
	rm -rf $(SLIDES_PDF) *.loc *.toc *.log *.idx *.aux *.out *.nav *.snm *.vrb *.blg *.bbl

