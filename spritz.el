;;; spritz.el --- RC4-like stream cipher -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;;; Commentary:

;; Spritz is a new stream cipher by Ron Rivest and Jacob Schuldt. It's
;; basically a redesign of RC4 using modern cryptographics tools and
;; knowledge. This implementation is based directly on the whitepaper,
;; implementing the the exact set of low-level primitives.

;; http://people.csail.mit.edu/rivest/pubs/RS14.pdf

;; Spritz is a sponge function and, as such, has an API analgous to a
;; sponge. It absorbs bytes and is squeezed to emit bytes. This is the
;; sponge API:

;; `spritz-create'      -- initializes a new state
;; `spritz-copy'        -- copy an existing state
;; `spritz-absorb'      -- absorb bytes into the state
;; `spritz-absorb-stop' -- cleanly separates different inputs
;; `spritz-drip'        -- output a single byte
;; `spritz-squeeze'     -- output multiple bytes (convenience)

;; These methods are used to implement all sorts of cryptographic
;; functions: hash, MAC, PRNG, encryption, and decryption. Some of
;; these are provided as a slightly highter level API:

;; `spritz-hash'        -- hashes a string or a buffer
;; `spritz-random'      -- random number generator (like `cl-random')
;; `spritz-random-iv'   -- generate a secure initialization vector (IV)
;; `spritz-random-uuid' -- generate a version 4 UUID

;; The "random" functions will, by default, use an internal random
;; state that is seeded at package load time by a dozen different
;; sources of entropy. This ensures the output of the PRNG functions
;; is secure. Additional entropy, such as bytes from /dev/random, can
;; be absorbed into this state by passing it to `spritz-random-stir'.
;; Accidentally absorbing non-random data is harmless.

;; Finally, two user interface commands are defined. These use Spritz
;; as both a stream cipher and key derivation function.

;; `spritz-encrypt-buffer'
;; `spritz-decrypt-buffer'

;;; Code:

(require 'cl-lib)

(defun spritz--create ()
  "A Spritz state is a 262-byte unibyte string.
The first 6 bytes are the values i, j, k, a, z, and w. The
remaining bytes are s."
  (let ((spritz (make-string (+ 256 6) 0)))
    (prog1 spritz
      (setf (aref spritz 5) 1)
      (dotimes (v 256)
        (setf (aref spritz (+ 6 v)) v)))))

(defmacro spritz--defun (name args &rest body)
  "This macro allows the core functions to be written as clearly as possible."
  (declare (indent defun))
  (let ((func-name (intern (concat "spritz--" (symbol-name name)))))
    `(defsubst ,func-name (this ,@args)
       (cl-symbol-macrolet ((i (aref this 0))
                            (j (aref this 1))
                            (k (aref this 2))
                            (a (aref this 3))
                            (z (aref this 4))
                            (w (aref this 5))
                            (n 256))
         (cl-macrolet ((s (v) `(aref this (+ (mod ,v 256) 6)))
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

;; Core primitives (private):

(spritz--defun output ()
  (setf z (s (+ j (s (+ i (s (+ z k))))))))

(spritz--defun update ()
  (setf i (mod (+ i w) n))
  (setf j (mod (+ k (s (+ j (s i)))) n))
  (setf k (mod (+ i k (s j)) n))
  (cl-rotatef (s i) (s j)))

(spritz--defun crush ()
  (dotimes (v (/ n 2))
    (when (> (s v) (s (- n 1 v)))
      (cl-rotatef (s v) (s (- n 1 v))))))

(spritz--defun whip (r)
  (dotimes (_ r)
    (update))
  (cl-loop do (setf w (mod (1+ w) n))
           until (= 1 (cl-gcd w n))))

(spritz--defun shuffle ()
  (whip (* 2 n))
  (crush)
  (whip (* 2 n))
  (crush)
  (whip (* 2 n))
  (setf a 0))

(spritz--defun drip ()
  (when (> a 0)
    (shuffle))
  (update)
  (output))

(spritz--defun absorb-nibble (x)
  (when (= a (/ n 2))
    (shuffle))
  (cl-rotatef (s a) (s (+ (/ n 2) x)))
  (cl-incf a))

(spritz--defun absorb-byte (b)
  (absorb-nibble (logand b #x0f))
  (absorb-nibble (lsh b -4)))

(spritz--defun absorb (string)
  (dotimes (v (length string))
    (absorb-byte (aref string v))))

(spritz--defun absorb-stop ()
  (when (= a (/ n 2))
    (shuffle))
  (cl-incf a))

(spritz--defun squeeze (r)
  (when (> a 0)
    (shuffle))
  (let ((p (make-string r 0)))
    (prog1 p
      (dotimes (v r)
        (setf (aref p v) (drip))))))

;; Sponge API:

(defun spritz-create (&optional key iv)
  "Return a new Spritz state, optionally keying it with KEY."
  (let ((spritz (spritz--create)))
    (prog1 spritz
      (when key
        (spritz-absorb spritz key))
      (when iv
        (spritz-absorb-stop spritz)
        (spritz-absorb spritz iv)))))

(defun spritz-copy (spritz)
  "Return an independent copy of SPRITZ with matching internal state."
  (copy-sequence spritz))

(defun spritz-p (object)
  "Return t if OBJECT is a valid Spritz state."
  (and (stringp object)
       (not (multibyte-string-p object))
       (= (+ 256 6) (length object))
       (equal (sort (nthcdr 6 (cl-coerce object 'list)) #'<)
              (number-sequence 0 255))))

(defun spritz-absorb (spritz value)
  "Absorb VALUE into SPRITZ, returning SPRITZ.
VALUE can be a string, a buffer, or an integer. Integers are
converted into a big endian byte strings for absorption. If a
buffer, the content of the buffer is used. If VALUE is multibyte,
its UTF-8 representation is absorbed."
  (prog1 spritz
    (cl-etypecase value
      (string
       (spritz--absorb spritz (encode-coding-string value 'utf-8-unix t)))
      (buffer
       (with-current-buffer value
         (spritz--absorb spritz
                         (encode-coding-region (point-min)
                                               (point-max) 'utf-8-unix t))))
      (integer
       (cl-loop for x = value then (lsh x -8)
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

(defun spritz-drip (spritz)
  "Produce a single byte of output from SPRITZ."
  (spritz--drip spritz))

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
  "Produce a SIZE-byte Spritz hash of OBJECT.
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

;; PRNG API:

(defvar spritz-random-state (spritz-create)
  "Global random state for Spritz PRNG functions.")

(defun spritz-random-stir (&rest entropy)
  "Add additional entropy to `spritz-random-state'."
  (cl-labels ((mix (e)
                (spritz-absorb-stop spritz-random-state)
                (spritz-absorb spritz-random-state (format "%s" e))))
    (prog1 spritz-random-state
      (if entropy
          (dolist (e entropy)
            (if (bufferp e)
                (with-current-buffer e
                  (mix (buffer-string)))
              (mix e)))
        (mix (current-time))
        (mix (emacs-uptime))
        (mix (garbage-collect))
        (mix (random))
        (mix command-history)
        (mix (list-system-processes))
        (mix (cl-random 1.0))
        (mix (buffer-list))
        (mix (recent-keys))))))

(cl-eval-when (load eval)
  (spritz-random-stir) ; gather up default entropy sources
  (spritz-random-stir  ; gather up some non-changing system entropy
   (user-uid) (emacs-pid) (system-name) (user-full-name) user-mail-address))

(defun spritz-random-iv (size)
  "Create a secure, random IV of SIZE bytes."
  (when (= 0 (random 1000))
    (spritz-random-stir)) ; occasionally mix in more entropy
  (spritz-squeeze spritz-random-state size))

(defun spritz--random-to-integer (random)
  "Read big-endian integer from unibyte string RANDOM."
  (cl-reduce (lambda (accum x) (logior (lsh accum 8) x)) random))

(cl-defun spritz-random (limit &optional (state spritz-random-state))
  "Like `cl-random', but use Spritz-based PRNG, returning [0, LIMIT).
LIMIT is a float or an integer."
  (cl-etypecase limit
    (integer
     (if (= limit 1)
         0
       (cl-loop with bytes = (ceiling (ceiling (log limit) (log 2)) 8)
                for random = (spritz-squeeze state bytes)
                for n = (spritz--random-to-integer random)
                when (< n limit)
                return n)))
    (float
     (let ((random (spritz-squeeze state 6)))
       (* limit
          (/ (spritz--random-to-integer random)
             (expt 2.0 (* 8 6))))))))

(defvar spritz--uuid-format
  "%02x%02x%02x%02x-%02x%02x-%02x%02x-%02x%02x-%02x%02x%02x%02x%02x%02x")

(cl-defun spritz-random-uuid ()
  "Generate a random version 4 UUID."
  (let* ((iv (cl-coerce (spritz-random-iv 16) 'list))
         (uuid (apply #'format spritz--uuid-format iv)))
    (prog1 uuid
      (setf (aref uuid 14) ?4))))

;; UI:

(require 'password-cache)

(defvar spritz-encrypt-iv-size 64
  "Size of IV for `spritz-encrypt-buffer' and `spritz-decrypt-buffer'.")

(cl-defun spritz-derive-key (passphrase &optional (n (* 1024 16)))
  "Derive an encryption key from PASSPHRASE."
  (let ((spritz (spritz-create)))
    (dotimes (_ (1- n))
      (spritz-absorb spritz passphrase)
      (spritz-absorb-stop spritz))
    (spritz-absorb spritz passphrase)
    (spritz-squeeze spritz 128)))

(defun spritz-encrypt-buffer (passphrase)
  "Encrypt the current buffer with KEY.
This automatically generates and uses an IV."
  (interactive (list (password-read "Passphrase: ")))
  (let* ((iv (spritz-random-iv spritz-encrypt-iv-size))
         (key (spritz-derive-key passphrase))
         (spritz (spritz-create key iv)))
    (spritz--process-buffer spritz #'+)
    (setf (point) (point-min))
    (insert iv)))

(defun spritz-decrypt-buffer (passphrase)
  "Decrypt the current buffer with KEY."
  (interactive (list (password-read "Passphrase: ")))
  (let* ((iv (buffer-substring 1 (1+ spritz-encrypt-iv-size)))
         (key (spritz-derive-key passphrase))
         (spritz (spritz-create key iv)))
    (setf (point) (point-min))
    (delete-char spritz-encrypt-iv-size)
    (spritz--process-buffer spritz #'-))
  (set-buffer-multibyte t))

(defun spritz--process-buffer (spritz op)
  (set-buffer-multibyte nil)
  (buffer-disable-undo)
  (setf (point) (point-min))
  (while (< (point) (point-max))
    (let ((c (char-after))
          (z (spritz-drip spritz)))
      (delete-char 1)
      (insert-char (mod (funcall op c z) 256)))))

(provide 'spritz)

;;; spritz.el ends here
