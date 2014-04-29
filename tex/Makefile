
all: pdf view
	echo 'pling'

pdf: bib
	pdflatex fp1.tex
	pdflatex fp1.tex > /dev/null

bib:
	pdflatex fp1.tex
	biber fp1

view:
	evince *.pdf &

clean:
	rm -rf *.toc
	rm -rf *.bbl
	rm -rf *.blg
	rm -rf *.aux
	rm -rf *.log
	rm -rf *.out
	rm -rf *.run.xml
	rm -rf *.bcf
	rm chapters/*.aux
