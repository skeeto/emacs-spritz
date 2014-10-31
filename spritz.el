;;; -*- lexical-binding: t; -*-

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
  (declare (indent defun))
  (let ((func-name (intern (concat "spritz--" (symbol-name name)))))
    `(defun ,func-name (this ,@args)
       (cl-symbol-macrolet ((i (spritz--i this))
                            (j (spritz--j this))
                            (k (spritz--k this))
                            (a (spritz--a this))
                            (z (spritz--z this))
                            (w (spritz--w this)))
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

(spritz--defun absorb (string)
  (dotimes (v (length string))
    (absorb-byte (aref string v))))

(spritz--defun absorb-byte (b)
  (absorb-nibble (logand b #x0f))
  (absorb-nibble (lsh b -4)))

(spritz--defun absorb-nibble (x)
  (when (= a 128)
    (shuffle))
  (cl-rotatef (s a) (s (+ 128 x)))
  (cl-incf a))

(spritz--defun absorb-stop ()
  (when (= a 128)
    (shuffle))
  (cl-incf a))

(spritz--defun shuffle ()
  (whip (* 2 256))
  (crush)
  (whip (* 2 256))
  (crush)
  (whip (* 2 256))
  (setf a 0))

(spritz--defun whip (r)
  (dotimes (_ r)
    (update))
  (cl-loop do (setf w (mod (+ w 1) 256))
           while (= 1 (cl-gcd w 256))))

(spritz--defun crush ()
  (dotimes (v 128)
    (when (> (s v) (s (- 255 v)))
      (cl-rotatef (s v) (s (- 255 v))))))

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
  (setf i (mod (+ i w) 256))
  (setf j (mod (+ k (s (+ j (s i)))) 256))
  (setf k (mod (+ i k (s j)) 256))
  (cl-rotatef (s i) (s j)))

(spritz--defun output ()
  (setf z (s (+ j (s (+ i (s (+ z k))))))))

;; Public API:

(defun spritz-create (&optional key iv)
  "Return a new Spritz state, optionally keying it with KEY."
  (let ((spritz (spritz--create)))
    (prog1 spritz
      (when key
        (spritz--absorb spritz key))
      (when iv
        (spritz--absorb-stop spritz)
        (spritz--absorb spritz iv)))))

(defun spritz-absorb (spritz string)
  "Absorb STRING into SPRITZ, returning SPRITZ."
  (prog1 spritz
    (spritz--absorb spritz string)))

(defun spritz-absorb-stop (spritz)
  "Absorb the special \"stop\" symbol, returning SPRITZ.
The purpose is to cleanly separate different inputs."
  (prog1 spritz
    (spritz--absorb-stop spritz)))

(defun spritz-squeeze (spritz n)
  "Produce N bytes of output from SPRITZ."
  (spritz--squeeze spritz n))
