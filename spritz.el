;;; -*- lexical-binding: t; -*-

;; absorb
;; absorb-byte b
;; absorb-nibble x
;; absorb-stop
;; shuffle
;; whip r
;; crush
;; squeeze r
;; drip
;; update
;; output

(require 'cl)

(cl-defstruct (spritz (:constructor spritz--create)
                      (:copier nil)
                      (:type vector)) ; disable type checking
  (-s (vconcat (number-sequence 0 255)))
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
                       (whip (r) `(spritz--whip this r))
                       (crush () `(spritz--crush this))
                       (squeeze (r) `(spritz--squeeze this r))
                       (drip () `(spritz--drip this))
                       (update () `(spritz--update this))
                       (output () `(spritz--output this)))
           ,@body)))))

(defun spritz-create (&optional key)
  (let ((spritz (spritz--create)))
    (when key
      (spritz-absorb spritz key))
    spritz))

(spritz--defun absorb (string)
  (dotimes (v (length string))
    (absorb-byte (aref string v))))

(spritz--defun absorb-byte (b)
  (absorb-nibble (logand b #x0f))
  (absorb-nibble (lsh b -4)))

(spritz--defun absorb-nibble (x)
  (when (= a 128)
    (shuffle))
  (rotatef (s a) (s (+ 128 x))))

(spritz--defun absorb-stop ()
  (when (= a 128)
    (shuffle))
  (incf a))

(spritz--defun shuffle ()
  (prog1 this
    (whip (* 2 256))
    (crush)
    (whip (* 2 256))
    (crush)
    (whip (* 2 256))
    (setf a 0)))

(spritz--defun whip (r)
  (dotimes (v r)
    (update))
  (cl-loop do (incf w)
           while (= 1 (gcd w 256))))

(spritz--defun crush ()
  (dotimes (v 128)
    (when (> (s v) (s (- 255 v)))
      (cl-rotatef (s v) (s (- 255 v))))))

(spritz--defun squeeze (r)
  (when (> a 0)
    (shuffle))
  (let ((p (make-string r 0)))
    (dotimes (v r p)
      (setf (aref p v (drip))))))

(spritz--defun drip ()
  (when (> a 0)
    (shuffle))
  (update)
  (output))

(spritz--defun update ()
  (incf i w)
  (setf j (+ k (s (+ j (s i)))))
  (setf k (+ i k (s j)))
  (rotatef (s i) (s j)))

(spritz--defun output ()
  (setf z (s (+ j (s (+ i (s (+ z k))))))))

;(spritz-create "foo")
