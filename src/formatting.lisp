;;;; formatting.lisp --- Formatting of binary data.
;;;;
;;;; Copyright (C) 2014, 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:utilities.binary-dump)

;;; Unit formatter
;;;
;;; A unit formatter is a function with lambda-list (stream unit) that
;;; prints UNIT, which is an integer or float number, to STREAM.
;;;
;;; Each formatter has an associated output width: the number of
;;; characters required for formatting one unit with this
;;; formatter. For example, the hexadecimal formatter for 1-byte
;;; integers requires two characters.

(defun make-unit-formatter (width endian type base)
  (declare (ignore endian))
  (ecase type
    ((signed-byte unsigned-byte)
     (let ((digits (ceiling width (log base 2))))
       (list
        (eval `(formatter ,(format nil "~~~D,'0~A"
                                   digits (ecase base
                                            (2  "B")
                                            (8  "O")
                                            (10 "D")
                                            (16 "X")))))
        digits)))
    (float
     (ecase width
       (32 (list (formatter "~16@A") 16))
       (64 (list (formatter "~24@A") 24))))))

(defvar *unit-formatters*
  (make-hash-table :test #'equal #+sbcl :synchronized #+sbcl t))

(defun find-unit-formatter (width endian type base)
  (let+ ((key (list width endian type base)))
    (gethash key *unit-formatters*)))

(defun (setf find-unit-formatter) (new-value width endian type base)
  (let ((key (list width endian type base)))
    (setf (gethash key *unit-formatters*) new-value)))

(defun ensure-unit-formatter (width endian type base)
  (or (find-unit-formatter width endian type base)
      (setf (find-unit-formatter width endian type base)
            (make-unit-formatter width endian type base))))

;;; Chunk printers
;;;
;;; `print-chunk/numeric' and `print-chunk/string' print a numeric and
;;; string representation respectively of one chunk of the data on a
;;; single output line.

(defun print-chunk/numeric (data start end shortened?
                            length endian type base
                            stream width
                            &optional (formatter (ensure-unit-formatter
                                                  length endian type base)))
  (let+ (((formatter-function formatter-width) formatter)
         (count 0))
    ;; Values and ellipsis.
    (map-units (lambda (unit last-unit?)
                 (if (and shortened? last-unit?)
                     (write-string ".." stream)
                     (funcall formatter-function stream unit))
                 (unless last-unit?
                   (write-char #\Space stream))
                 (incf count))
               data length endian type :start start :end end)
    ;; Padding
    (dotimes (i (- width (* (1+ formatter-width) (1- count)) 2))
      (write-char #\Space stream))))

(defun print-chunk/string (data start end shortened? stream width)
  (let+ (((&flet printable-character (thing)
            (cond
              ((stringp thing) thing)
              ((let ((char (code-char thing)))
                 (when (and (standard-char-p char)
                            (not (member char '(#\Space #\Newline #\Tab))))
                   char)))
              (t #\.))))
         (chunk/list (map 'list #'printable-character
                          (subseq data start end)))) ; TODO avoid copy
    (when shortened?
      (setf (lastcar chunk/list) #\.))
    (format stream "~V@<~{~A~}~>" width chunk/list)))

(defun numeric-part-width (width unit-width count)
  (declare (type (or null non-negative-integer) width)
           (type non-negative-integer           unit-width count))
  (if width
      ;; Width of numeric part and string is UNIT-WIDTH +
      ;; 1. WIDTH-RATIO⁻¹ is width of numeric part over both
      ;; parts. floor(WIDTH / WIDTH-RATIO) is ratio of width for
      ;; numeric part.
      (let* ((width-ratio (/ (1+ unit-width) unit-width))
             (x           (floor width width-ratio)))
        (1- (- x (mod x unit-width))))
      ;; If WIDTH is not available, compute the required width for
      ;; formatting all numeric parts on one line.
      (max 0 (1- (* unit-width count)))))

(defun %binary-dump (data start end stream width lines shortened?
                     offset-base
                     length endian type base
                     print-type)
  (let+ (((&optional formatter/offset width/offset)
          (when offset-base
            (ensure-unit-formatter
             (integer-length end) :little 'unsigned-byte offset-base)))
         ((&whole formatter/unit &ign width/unit)
          (ensure-unit-formatter length endian type base))
         ;; Width calculations
         (width/offset  (if width/offset (+ width/offset 2) 0))
         (width         (when width
                          (max width (+ width/offset 5))))
         (width/unit    (1+ width/unit))
         (width/numeric (numeric-part-width
                         (when width (- width width/offset 1)) width/unit end))
         (width/string  (if width
                            (- width width/offset width/numeric 1)
                            end))
         (chunk-length  (if width
                            (* (/ length 8) (floor (1+ width/numeric) width/unit))
                            end)))
    (when (zerop chunk-length)
      (error "~@<Invalid combination of formatting parameters: the ~
              requested width ~S is too small for even a single ~
              unit with parameters ~{~S~^ ~}.~@:>"
             width
             (list :length length :endian endian :type type :base base)))

    (let ((values))
      (pprint-logical-block (stream (list data))
        (when print-type
          (format stream "~:D-byte ~S~:@_" (length data) (type-of data)))
        (setf values
              (multiple-value-list
               (map-chunks
                (lambda (offset data start end last-chunk?)
                  (let ((chunk-shortened? (and shortened? last-chunk?)))
                    ;; Offset
                    (when offset-base
                      (funcall formatter/offset stream offset)
                      (write-char #\Space stream))
                    ;; Numeric
                    (print-chunk/numeric data start end chunk-shortened?
                                         length endian type base
                                         stream width/numeric formatter/unit)
                    (write-char #\Space stream)
                    ;; String
                    (print-chunk/string data start end chunk-shortened?
                                        stream width/string)
                    ;; Newline
                    (unless last-chunk?
                      (pprint-newline :mandatory stream))))
                data chunk-length :start start :end end :max-chunks lines))))
      (apply #'values values))))

;;; API

(defun binary-dump (data
                    &key
                    (start      0)
                    (end        (length data)                      end-supplied?)

                    stream
                    (width      (%stream-remaining-columns stream))
                    (lines      *print-lines*)

                    offset-base

                    (length     8)
                    (endian     :little)
                    (type       'unsigned-byte)
                    (base       *print-base*)

                    print-type)
  "Print DATA to STREAM as a binary, octal, decimal, hexadecimal,
   etc. dump of the form

     [OFFSET ]B₁ B₂ B₃ ... S₁S₂S₃ ...
     ...

   where OFFSET - the offset of B₁ printed in base OFFSET-BASE - is
   only printed when OFFSET-BASE is an integer designating a base.

   B₁, B₂, ... are the bytes (or larger units according to LENGTH) of
   DATA printed in base BASE. LENGTH, ENDIAN and TYPE characterize the
   length, type and decoding of units:

     LENGTH is either 8, 16, 32 or 64 if TYPE is [UN]SIGNED-BYTE and
     either 32 or 64 if TYPE is FLOAT.

     ENDIAN is either :LITTLE or :BIG and only matters if LENGTH is
     not 8.

     TYPE is one of [UN]SIGNED-BYTE and FLOAT.

   The default behavior is formatting unsigned byte units in base
   *PRINT-BASE*.

   S₁S₂... is the part of DATA which corresponds to B₁ B₂ ... rendered
   as a string. In S₁S₂..., unprintable and whitespace characters are
   replaced with \".\".

   Return four values: 1) DATA 2) the start index of the processed
   sub-sequence of DATA (i.e. START) 3) the corresponding end
   index (not necessarily END) 4) the number of processed chunks.

   If START and/or END are supplied, the subsequence of DATA bounded
   by START and END instead of all of DATA is processed.

   Additionally, if LINES is non-nil (either the keyword argument is
   supplied or its default value, the value of `*print-lines*' is
   non-nil), the output is limited to LINES lines. Supplying :lines
   nil removes this limitation, even if `*print-lines*' is non-nil.

   When PRINT-TYPE is true, the output is preceded by a line of the
   form

     N-byte TYPE

   where TYPE is the type of DATA.

   Depending on the length of DATA and WIDTH, the printed
   representation can span multiple lines."
  (let+ ((stream (case stream
                   ((nil) *standard-output*)
                   ((t)   *terminal-io*)
                   (t     stream)))
         ((&values end* shortened?)
          (cond
            ((and (not end-supplied?) *print-length*)
             (let ((end* (min (+ start *print-length*) end)))
               (values end* (< end* end))))
            (end)
            (t
             (length data)))))
    (%binary-dump data start end* stream width lines shortened?
                  offset-base
                  length endian type base
                  print-type)))

(defun print-binary-dump (stream data
                          &optional
                          colon? at? width start end (base *print-base*))
  "Print DATA to STREAM as a binary, octal, decimal, hexadecimal,
   etc. dump of the form

     [OFFSET ]B₁ B₂ B₃ ... S₁S₂S₃ ...
     ...

   For details, see `binary-dump'. This function is designed for use
   in ~/ format directives."
  (binary-dump data :start (or start 0) :end (or end (length data))
               :stream      stream
               :width       (or width (%stream-remaining-columns stream))
               :offset-base (when colon? base)
               :base        base
               :print-type  at?))


;; Local Variables:
;; coding: utf-8
;; End:
