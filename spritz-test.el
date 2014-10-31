;;; spritz-test-el --- test suite for spritz package

;; This is free and unencumbered software released into the public domain.

;;; Commentary:

;; These test vectors come right out of the Spritz white paper.

;; emacs -batch -Q -L . -l spritz-test.el -f ert-run-tests-batch

;;; Code:

(require 'ert)
(require 'spritz)

(defun spritz--test-basic (string output)
  (let ((spritz (spritz-create)))
    (spritz-absorb spritz string)
    (should (equal output (vconcat (spritz-squeeze spritz (length output)))))))

(ert-deftest spritz-basic ()
  (spritz--test-basic "ABC"     [#x77 #x9a #x8e #x01 #xf9 #xe9 #xcb #xc0])
  (spritz--test-basic "spam"    [#xf0 #x60 #x9a #x1d #xf1 #x43 #xce #xbf])
  (spritz--test-basic "arcfour" [#x1a #xfa #x8b #x5e #xe3 #x37 #xdb #xc7]))

(defun spritz--test-hash (string size hash)
  (let ((result (spritz-hash string :size size :binary t)))
    (should (equal hash (vconcat (substring result 0 (length hash)))))))

(ert-deftest spritz-hash ()
  (spritz--test-hash "ABC"     32 [#x02 #x8f #xa2 #xb4 #x8b #x93 #x4a #x18])
  (spritz--test-hash "spam"    32 [#xac #xbb #xa0 #x81 #x3f #x30 #x0d #x3a])
  (spritz--test-hash "arcfour" 32 [#xff #x8c #xf2 #x68 #x09 #x4c #x87 #xb9]))

;;; spritz-test.el ends here
