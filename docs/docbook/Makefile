########################################################################
##
## Makefile for building HAsim documentation
##
########################################################################


##
## PNG images derived from SVG.  We don't include SVG directly in html
## because not all browsers and XML editors render it.  The make process
## renders PNG automatically for targets listed here.
##
PNG_FROM_SVG =

all: png
	+(cd build; make)

png: $(PNG_FROM_SVG)

clean:
	+(cd build; make clean)

.PHONY: all png clean

%.png: %.svg
	batik-rasterizer -m image/png -dpi 100 $<
	# Batik generates PNG files that screw up color management in Acrobat.
	# Running PNG through NetPBM and back fixes the problem.
	mv $@ $@.tmp
	pngtopnm -mix $@.tmp | pnmtopng -compression 2 -phys 4500 4500 1 > $@
	rm -f $@.tmp
