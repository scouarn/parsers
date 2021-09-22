


SVG_FILES := $(wildcard figures/*.svg)
PDF_FILES := $(patsubst figures/%.svg, figures/%.pdf, $(SVG_FILES))


all : $(PDF_FILES)
	pdflatex -shell-escape main.tex 
	bibtex   main.aux
	evince   main.pdf



$(PDF_FILES): figures/%.pdf : figures/%.svg
	inkscape $<  -o $@ --export-type=pdf
	#inkscape -D $<  -o $@ --export-latex

clean:
	rm -f *.aux *.log *.toc *.bbl *.blg sections/*.aux sections/*.log

