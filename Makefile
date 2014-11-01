EMACS ?= emacs

spritz.elc : spritz.el
	$(EMACS) -Q -batch -f batch-byte-compile $^

.PHONY : test clean

test : spritz.elc
	$(EMACS) -batch -Q -L . -l spritz-test.el -f ert-run-tests-batch

clean :
	$(RM) spritz.elc
