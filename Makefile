.POSIX:
EMACS = emacs

compile: spritz.elc

check: test
test: spritz-test.elc
	$(EMACS) -batch -Q -L . -l spritz-test.elc -f ert-run-tests-batch

spritz-test.elc: spritz-test.el spritz.elc

clean:
	rm -f spritz.elc spritz-test.elc

.SUFFIXES: .el .elc
.el.elc:
	$(EMACS) -batch -Q -L . -f batch-byte-compile $<
