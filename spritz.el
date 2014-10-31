;;; -*- lexical-binding: t; -*-

(require 'cl)

(cl-defstruct (spritz (:constructor spritz--create)
                      (:copier nil)
                      (:type vector)) ; disable type checking
  (s (vconcat (number-sequence 0 255)))
  (i 0)
  (j 0)
  (k 0)
  (a 0)
  (z 0)
  (w 1))

(defmacro spritz--body (var &rest body)
  (declare (indent defun))
  `(cl-symbol-macrolet ((i (spritz-i ,var))
                        (j (spritz-j ,var))
                        (k (spritz-k ,var))
                        (a (spritz-a ,var))
                        (z (spritz-z ,var))
                        (w (spritz-w ,var)))
     (cl-macrolet ((s (v) `(aref (spritz-s ,',var) (mod ,v 256))))
       ,@body)))

(defun spritz-create (&optional key)
  (let ((spritz (spritz--create)))
    (when key
      (spritz-absorb spritz key))
    spritz))

(defun spritz-absorb (spritz string)
  (dotimes (v (length string))
    (spritz--absorb-byte spritz (aref string v))))

(defun spritz--absorb-byte (spritz b)
  (spritz--absorb-nibble spritz (logand b #x0f))
  (spritz--absorb-nibble spritz (lsh b -4)))

(defun spritz--absorb-nibble (spritz x)
  (spritz--body spritz
    (when (= a 128)
      (spritz--shuffle spritz))
    (rotatef (s a) (s (+ 128 x)))))

(defun spritz--absorb-stop (spritz)
  (spritz--body spritz
    (when (= a 128)
      (spritz--shuffle spritz))
    (incf a)))

(defun spritz--shuffle (spritz)
  (spritz--body spritz
    (prog1 spritz
      (spritz--whip spritz (* 2 256))
      (spritz--crush spritz)
      (spritz--whip spritz (* 2 256))
      (spritz--crush spritz)
      (spritz--whip spritz (* 2 256))
      (setf a 0))))

(defun spritz--whip (spritz r)
  (spritz--body spritz
    (dotimes (v r)
      (spritz--update spritz))
    (cl-loop do (incf w)
             while (= 1 (gcd w 256)))))

(defun spritz--crush (spritz)
  (let ((s (spritz-s spritz)))
    (dotimes (v 128)
      (when (> (aref s v) (aref s (- 255 v)))
        (cl-rotatef (aref s v) (aref s (- 255 v)))))))

(defun spritz--squeeze (spritz r)
  (spritz--body spritz
    (when (> a 0)
      (spritz--shuffle spritz))
    (let ((p (make-string r 0)))
      (dotimes (v r p)
        (setf (aref p v (spritz--drip spritz)))))))

(defun spritz--drip (spritz)
  (spritz--body spritz
    (when (> a 0)
      (spritz--shuffle spritz))
    (spritz--update spritz)
    (spritz--output spritz)))

(defun spritz--update (spritz)
  (spritz--body spritz
    (incf i w)
    (setf j (+ k (s (+ j (s i)))))
    (setf k (+ i k (s j)))
    (rotatef (s i) (s j))))

(defun spritz--output (spritz)
  (spritz--body spritz
    (setf z (s (+ j (s (+ i (s (+ z k)))))))))

(spritz-create "foo")
