PWD=$(shell pwd)
EL=$(shell find -name "*.el")
ELC=$(EL:%.el=%.elc)

install:
	@echo "The following action will overwrite your .emacs.d directory and your .emacs file"
	@echo "Do you want to continue ? (y/N)"
	@read CONFIRM && case $$CONFIRM in y|Y|YES|yes|Yes) echo "installing..." && rm -rf ~/.emacs.d ~/.emacs && cd ~ && ln -s $(PWD)/.emacs.d && ln -s $(PWD)/.emacs && cd $(PWD) && echo "installed";; *) echo "aborted";; esac

compile: $(ELC)

%.elc: %.el
	emacs -batch -f batch-byte-compile $<

clean:
	find -name "*~" -delete
	find -name "\#*" -delete

mrproper: clean
	find -name "*.elc" -delete
