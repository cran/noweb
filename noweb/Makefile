all: fun doc

fun:  ../R/noweb.R 
doc:  ../inst/doc/nowebsource.pdf
R= R

# Use noweb
../R/noweb.R: nowebsource.Rnw noweave.Rnw
	echo "# Automatically generated from all.nw using noweb" > $@
	cat nowebsource.Rnw noweave.Rnw > temp.nw
	echo "library(noweb); notangle('temp.nw')" | $(R) --vanilla --slave
	cat temp.R >> $@
	rm temp.nw

temp.tex: nowebsource.Rnw noweave.Rnw
	cat nowebsource.Rnw noweave.Rnw > temp.nw
	cp ../inst/include/noweb.sty .
	echo "library(noweb); noweave('temp.nw')" | $(R) --vanilla --slave

../inst/doc/nowebsource.pdf: temp.tex
	pdflatex temp.tex
	cp temp.pdf ../inst/doc/nowebsource.pdf

clean: 
	-rm noweb.aux noweb.sty noweb.tex noweb.out noweb.log temp*

# Original code using the standalone noweb function
#../R/noweb.R: noweb.Rnw noweave.Rnw
#	cat noweb.Rnw noweave.Rnw > temp.nw
#	notangle temp.nw > ../R/noweb.R
#	rm temp.nw

