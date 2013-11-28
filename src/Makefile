
# A helper makefile with shortcuts for rapid charts generation and
# cleanup


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
