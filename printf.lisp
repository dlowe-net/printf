;;;; printf.lisp

(in-package #:printf)

(defun printf-flags ()
  (zero-or-more (=or (=and (=char #\#) (result :alternate))
                     (=and (=char #\0) (result :zero-padded))
                     (=and (=char #\-) (result :left-aligned))
                     (=and (=char #\+) (result :signed))
                     (=and (=char #\') (result :grouped))
                     (=and (=char #\space) (result :space-padded)))))

(defun printf-argument-index ()
  (=let* ((_ (=char #\*))
          (num (natural-number))
          (_ (=char #\$)))
    (result (list :arg num))))

(defun printf-width ()
  (=or
   (printf-argument-index)
   (=char #\*)
   (natural-number)))

(defun printf-precision ()
  (=and (=char #\.)
        (=or (natural-number)
             (printf-argument-index)
             (=char #\*)
             (result 0))))

(defun printf-length-modifier ()
  (=or (=and (=string "hh") (result :char-size))
       (=and (=string "h") (result :short-size))
       (=and (=string "ll") (result :longlong-size))
       (=and (=string "l") (result :long-size))
       (=and (=string "L") (result :long-double-size))
       (=and (=string "j") (result :max-size))
       (=and (=string "z") (result :sizet-size))
       (=and (=string "t") (result :ptrdiff-size))))

(defun printf-specifier ()
  (=or (=and (=char #\d) (result :decimal))
       (=and (=char #\i) (result :decimal))
       (=and (=char #\o) (result :octal))
       (=and (=char #\u) (result :decimal))
       (=and (=char #\x) (result :unsigned-lowercase-hex))
       (=and (=char #\X) (result :unsigned-uppercase-hex))
       (=and (=char #\e) (result :lowercase-exponential))
       (=and (=char #\E) (result :uppercase-exponential))
       (=and (=char #\f) (result :floating))
       (=and (=char #\F) (result :floating))
       (=and (=char #\g) (result :lowercase-optional-exp))
       (=and (=char #\G) (result :uppercase-optional-exp))
       (=and (=char #\a) (result :lowercase-double-hex))
       (=and (=char #\A) (result :uppercase-double-hexsize))
       (=and (=char #\c) (result :char))
       (=and (=char #\s) (result :string))
       (=and (=char #\%) (result :percent))))

(defun printf-percent ()
  (=let* ((_ (=char #\%))
          (flags (maybe (printf-flags)))
          (width (maybe (printf-width)))
          (precision (maybe (printf-precision)))
          (_ (maybe (printf-length-modifier)))
          (specifier (printf-specifier)))
    (result (if (eql specifier :percent)
                #\%
                (list specifier flags width precision)))))

(defun printf-backslash ()
  (=let* ((_ (=char #\\))
          (c (item)))
    (case c
      (#\a (result #\bell))
      (#\b (result #\backspace))
      (#\e (result #\escape))
      (#\f (result #\formfeed))
      (#\n (result #\newline))
      (#\o (=let* ((xs (natural-number 8)))
             (result (code-char xs))))
      (#\r (result #\return))
      (#\t (result #\tab))
      (#\x (=let* ((xs (natural-number 16)))
             (result (code-char xs))))
      (#\\ (result #\\))
      (t (fail)))))

(defun make-printf-parser ()
  (zero-or-more (=or (printf-percent)
                     (printf-backslash)
                     (item))))

(defun printf-expand-specifier (specifier
                                flags
                                precision
                                arg)
  (with-output-to-string (result)
    (ecase specifier
      (:char
       (write-char arg result))
      (:string
       (let* ((len (if precision
                       (min precision (length arg))
                       (length arg))))
         (write-string arg result :end len)))
      (:decimal
       (let ((precision (or precision 1)))
         (unless (and (zerop precision) (zerop arg))
           (format result "~:[~v,'0d~;~v,'0:d~]"
                   (member :grouped flags)
                   precision
                   (abs arg)))))
      (:octal
       (let* ((require-leading-zero (and (member :alternate flags)
                                         (not (zerop arg))))
              (precision (if require-leading-zero
                             (max 0 (1- (or precision 1)))
                             (or precision 1))))
         (unless (and (zerop precision) (zerop arg))
           (format result "~:[~;0~]~v,'0o"
                   require-leading-zero
                   precision
                   (abs arg)))))
      ((:uppercase-hex :lowercase-hex)
       (let ((precision (or precision 1)))
         (unless (and (zerop precision) (zerop arg))
           (format result "~:[~(~:[~;0x~]~v,'0x~)~;~:@(~:[~;0x~]~v,'0x~)~]"
                   (eql specifier :uppercase-hex)
                   (and (member :alternate flags)
                        (not (zerop arg)))
                   precision
                   (abs arg))))))))

(defun printf-numeric-specifier-p (specifier)
  (member specifier '(:decimal :octal :uppercase-hex :lowercase-hex)))

(defun printf-sign-char (specifier flags arg)
  (cond
    ((not (printf-numeric-specifier-p specifier))
     nil)
    ((minusp arg)
     #\-)
    ((member :signed flags)
     #\+)
    ((member :space-padded flags)
     #\space)))

(defun printf-padding-char (specifier flags precision)
  (if (and (member :zero-padded flags)
           (not (member :left-aligned flags))
           (not (and precision
                     (printf-numeric-specifier-p specifier))))
      #\0
      #\space))

(defun vfprintf (stream fmt args)
  (let ((arg-idx 0))
    (flet ((next-arg ()
             (prog1
                 (elt args arg-idx)
               (incf arg-idx))))
      (dolist (element (run (make-printf-parser) fmt))
        (if (atom element)
            (write-char element stream)
            (destructuring-bind (specifier flags width-spec precision-spec)
                element
              (let* ((width (cond
                              ((eql width-spec #\*)
                               (next-arg))
                              ((consp width-spec)
                               (elt args (second width-spec)))
                              (t
                               width-spec)))
                     (precision (cond
                              ((eql precision-spec #\*)
                               (next-arg))
                              ((consp precision-spec)
                               (elt args (second precision-spec)))
                              (t
                               precision-spec)))
                     (arg (next-arg))
                     (sign-char (printf-sign-char specifier flags arg))
                     (padding-char (printf-padding-char specifier flags precision))
                     (expansion (printf-expand-specifier specifier
                                                         flags
                                                         precision
                                                         arg))
                     (left-padding (if (or (not width) (member :left-aligned flags))
                                       0
                                       (max 0
                                            (- width (length expansion)
                                               (if sign-char 1 0)))))
                     (right-padding (if (and width (member :left-aligned flags))
                                       (max 0 (- width (length expansion)
                                                 (if sign-char 1 0)))
                                       0)))
                (when (and sign-char (eql padding-char #\0))
                  (write-char sign-char stream))
                (loop repeat left-padding do
                     (write-char padding-char stream))
                (when (and sign-char (eql padding-char #\space))
                  (write-char sign-char stream))
                (write-string expansion stream)
                (loop repeat right-padding do
                     (write-char padding-char stream)))))))))

(defun fprintf (stream fmt &rest args)
  (vfprintf stream fmt args))

(defun vsprintf (fmt args)
  (with-output-to-string (result)
    (vfprintf result fmt args)))

(defun sprintf (fmt &rest args)
  (with-output-to-string (result)
    (vfprintf result fmt args)))

(defun vprintf (fmt args)
  (vfprintf *standard-output* fmt args))

(defun printf (fmt &rest args)
  (vfprintf *standard-output* fmt args))
