PWD=$(shell pwd)

install: emms
	@echo "The following action will overwrite your .emacs.d directory and your .emacs file"
	@echo "Do you want to continue ? (y/N)"
	@read CONFIRM && \
	case $$CONFIRM in \
		y|Y|YES|yes|Yes) \
			echo "[Installing...]" && \
			rm -rf ~/.emacs.d ~/.emacs && \
			cp .emacs ~/ && \
			cp -r .emacs.d ~/ && \
			echo "[Intalled]" && \
			echo "Emacs will be longer than usual the first time you launch it.";; \
		*) echo "[Aborted]";; \
	esac


link: emms
	@echo "The following action will overwrite your .emacs.d directory and your .emacs file"
	@echo "Do you want to continue ? (y/N)"
	@read CONFIRM && \
	case $$CONFIRM in \
		y|Y|YES|yes|Yes) \
			rm -rf ~/.emacs.d ~/.emacs && \
			cd ~ && \
			ln -s $(PWD)/.emacs.d && \
			ln -s $(PWD)/.emacs && \
			cd $(PWD) && \
			echo "[Done]";; \
		*) echo "[Aborted]";; \
	esac

emms:
	cd .emacs.d/emms/ && make && cd ../..

clean:
	find -name "*~" -delete
	find -name "\#*" -delete

mrproper: clean
	find -name "*.elc" -delete
