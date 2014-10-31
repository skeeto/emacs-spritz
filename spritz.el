;;; -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;;; Commentary:

;; http://people.csail.mit.edu/rivest/pubs/RS14.pdf

;;; Code:

(require 'cl-lib)

(cl-defstruct (spritz (:constructor spritz--create)
                      (:copier nil)
                      (:type vector)) ; disable type checking
  (-s (vconcat (let ((s (make-vector 256 0)))
                 (dotimes (v 256 s)
                   (setf (aref s v) v)))))
  (-i 0)
  (-j 0)
  (-k 0)
  (-a 0)
  (-z 0)
  (-w 1))

(defmacro spritz--defun (name args &rest body)
  "This macro allows the core functions to be written as clearly as possible."
  (declare (indent defun))
  (let ((func-name (intern (concat "spritz--" (symbol-name name)))))
    `(defun ,func-name (this ,@args)
       (cl-symbol-macrolet ((i (spritz--i this))
                            (j (spritz--j this))
                            (k (spritz--k this))
                            (a (spritz--a this))
                            (z (spritz--z this))
                            (w (spritz--w this))
                            (n 256))
         (cl-macrolet ((s (v) `(aref (spritz--s this) (mod ,v 256)))
                       (absorb-byte (b) `(spritz--absorb-byte this ,b))
                       (absorb-nibble (x) `(spritz--absorb-nibble this ,x))
                       (absorb-stop () `(spritz--absorb-stop this))
                       (shuffle () `(spritz--shuffle this))
                       (whip (r) `(spritz--whip this ,r))
                       (crush () `(spritz--crush this))
                       (squeeze (r) `(spritz--squeeze this ,r))
                       (drip () `(spritz--drip this))
                       (update () `(spritz--update this))
                       (output () `(spritz--output this)))
           ,@body)))))

;; Core functions (private):

(spritz--defun absorb (string)
  (dotimes (v (length string))
    (absorb-byte (aref string v))))

(spritz--defun absorb-byte (b)
  (absorb-nibble (logand b #x0f))
  (absorb-nibble (lsh b -4)))

(spritz--defun absorb-nibble (x)
  (when (= a (/ n 2))
    (shuffle))
  (cl-rotatef (s a) (s (+ (/ n 2) x)))
  (cl-incf a))

(spritz--defun absorb-stop ()
  (when (= a (/ n 2))
    (shuffle))
  (cl-incf a))

(spritz--defun shuffle ()
  (whip (* 2 n))
  (crush)
  (whip (* 2 n))
  (crush)
  (whip (* 2 n))
  (setf a 0))

(spritz--defun whip (r)
  (dotimes (_ r)
    (update))
  (cl-loop do (setf w (mod (1+ w) n))
           until (= 1 (cl-gcd w n))))

(spritz--defun crush ()
  (dotimes (v (/ n 2))
    (when (> (s v) (s (- n 1 v)))
      (cl-rotatef (s v) (s (- n 1 v))))))

(spritz--defun squeeze (r)
  (when (> a 0)
    (shuffle))
  (let ((p (make-string r 0)))
    (dotimes (v r p)
      (setf (aref p v) (drip)))))

(spritz--defun drip ()
  (when (> a 0)
    (shuffle))
  (update)
  (output))

(spritz--defun update ()
  (setf i (mod (+ i w) n))
  (setf j (mod (+ k (s (+ j (s i)))) n))
  (setf k (mod (+ i k (s j)) n))
  (cl-rotatef (s i) (s j)))

(spritz--defun output ()
  (setf z (s (+ j (s (+ i (s (+ z k))))))))

;; Sponge API:

(defun spritz-create (&optional key iv)
  "Return a new Spritz state, optionally keying it with KEY."
  (let ((spritz (spritz--create)))
    (prog1 spritz
      (when key
        (spritz--absorb spritz key))
      (when iv
        (spritz--absorb-stop spritz)
        (spritz--absorb spritz iv)))))

(defun spritz-absorb (spritz value)
  "Absorb VALUE into SPRITZ, returning SPRITZ.
VALUE can be a string or an integer. Integers are converted into
a big endian byte strings for absorption. If VALUE is a multibyte
string, its UTF-8 representation is absorbed."
  (prog1 spritz
    (cl-etypecase value
      (string (spritz--absorb spritz (string-as-unibyte value)))
      (integer (cl-loop for x = value then (lsh x -8)
                        while (> x 0)
                        collect (logand #xff x) into bytes
                        finally (let ((string (concat (nreverse bytes))))
                                  (spritz--absorb spritz string)))))))

(defun spritz-absorb-stop (spritz)
  "Absorb the special \"stop\" symbol, returning SPRITZ.
The purpose is to cleanly separate different inputs."
  (prog1 spritz
    (spritz--absorb-stop spritz)))

(defun spritz-squeeze (spritz n)
  "Produce N bytes of output from SPRITZ."
  (spritz--squeeze spritz n))

(defalias 'spritz-drip #'spritz--drip
  "Produce a single byte of output from SPRITZ.")

;; High-level API:

(defun spritz--get-string (object start end)
  "Get a string representation of OBJECT, which may be the OBJECT itself."
  (cl-etypecase object
    (string
     (if (and (null start) (null end))
         object
       (substring object (or start 0) (or end (length object)))))
    (buffer
     (with-current-buffer object
       (buffer-substring (or start (point-min)) (or end (point-max)))))))

(cl-defun spritz-hash (object &key (size 20) start end binary domain)
  "Produce an N-byte Spritz hash of OBJECT.
OBJECT is either a string or a buffer.

If :binary is non-nil, returns a the binary form of the hash.

The :domain key is used to apply domain separation. This can be
used to compute MAC (Message Authentication Codes)."
  (let ((spritz (spritz-create)))
    (when domain
      (spritz-absorb spritz domain)
      (spritz-absorb-stop spritz))
    (spritz-absorb spritz (spritz--get-string object start end))
    (spritz-absorb-stop spritz)
    (spritz-absorb spritz size)
    (let ((hash (spritz-squeeze spritz size)))
      (if binary
          hash
        (mapconcat (lambda (v) (format "%02x" v)) hash "")))))
