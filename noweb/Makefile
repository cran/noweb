all: fun doc

fun:  ../R/noweb.R 
doc:  code.pdf
R= R

# Use noweb
../R/noweb.R: nowebsource.Rnw noweave.Rnw
	echo "# Automatically generated from all.nw using noweb" > $@
	cat nowebsource.Rnw noweave.Rnw > temp.nw
	echo "library(noweb); notangle('temp.nw')" | $(R) --vanilla --slave
	mv temp.R $@
	rm temp.nw

code.tex: nowebsource.Rnw noweave.Rnw noweb.sty
	cat nowebsource.Rnw noweave.Rnw > code.nw
	cp ../inst/include/noweb.sty .
	echo "library(noweb); noweave('code.nw')" | $(R) --vanilla --slave

code.pdf: code.tex
	pdflatex code.tex

noweb.sty: ../inst/include/noweb.sty
	cp ../inst/include/noweb.sty .
clean: 
	-rm *.aux *.sty *.tex *.out *.log code.nw

# Original code using the standalone noweb function
#../R/noweb.R: noweb.Rnw noweave.Rnw
#	cat noweb.Rnw noweave.Rnw > temp.nw
#	notangle temp.nw > ../R/noweb.R
#	rm temp.nw

