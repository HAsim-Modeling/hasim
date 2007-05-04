all: 
	-rm -rf html pdf
	#
	# Convert to html...
	for kind in one-page chapters; do \
	  mkdir -p html/$$kind ; \
	  tgt="`pwd`/html/$$kind" ; \
	  cp ../style/tldp-xsl/doc/style.css html/$$kind ; \
	  (cd ../xml; find . \( -name '*.jpg' -o -name '*.png' -o -name '*.svg' \) -print | tar -cf - -T /dev/stdin) | (cd html/$$kind; tar xf -) ; \
	  (cd html/$$kind; xsltproc --xinclude -novalid ../../../style/tldp-xsl/html/tldp-$${kind}.xsl ../../../xml/hasim.xml > top.html) ; \
	  (cd html/$$kind; if [ ! -f index.html ]; then mv top.html index.html; else rm top.html; fi) ; \
	done

	# Style guide
	(cd html/one-page; xsltproc --xinclude -novalid ../../../style/tldp-xsl/html/tldp-one-page.xsl ../../../xml/style-guide.xml > style-guide.html) ; \

	# Generate FOP version.
	#mkdir -p pdf
	#(cd pdf; xsltproc --xinclude -novalid ../../style/tldp-xsl/fo/tldp-print.xsl ../../xml/hasim.xml > hasim.fo)
	#(cd ../xml; fop -fo ../build/pdf/hasim.fo -pdf ../build/pdf/hasim.pdf)
	#rm -f pdf/hasim.fo

clean:
	-rm -rf html pdf
