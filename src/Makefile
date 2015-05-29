
# A helper makefile with shortcuts for rapid charts generation and
# cleanup
#
# This is primarely intended to be used with the suffix tree
# generation and handling code in the suffix-tree.lisp file


dot_files=$(wildcard *.dot)
png_files=$(subst .dot,.png,$(dot_files))

%.png: %.dot
	dot -Tpng -o $@ $<
	rm $<

charts: $(png_files)
#	eog .

.PHONY: clean

clean:
	rm -f *dot *png

# EOF
