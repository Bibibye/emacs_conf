EL=$(shell find -name "*.el")
ELC=$(EL:%.el=%.elc)

compile: $(ELC)

%.elc: %.el
	emacs -batch -f batch-byte-compile $<

clean:
	find -name *~ -delete
	find -name \#* -delete
