include ../Makefile.config

all: dvi ps www

www: wxlib.html
dvi: wxlib.dvi gwml.dvi
ps: wxlib.ps gwml.ps

wxlib.html: wxlib.dvi
	hevea wxlib.tex
	imagen wxlib
	mv wxlib.html html/
	mv *.gif html/

gwml.ps: gwml.dvi
	dvips -o gwml.ps gwml.dvi

gwml.dvi: gwml.tex
	echo R | latex gwml.tex

wxlib.dvi: wxlib.tex
	echo R | latex wxlib
	echo R | latex wxlib
	echo R | latex wxlib

wxlib.ps: wxlib.dvi
	dvips -o wxlib.ps wxlib.dvi
clean:
	rm -f *.log *.aux *.toc *.ps *.dvi *.image.tex