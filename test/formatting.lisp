;;;; formatting.lisp --- Unit tests for formatting functions.
;;;;
;;;; Copyright (C) 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:utilities.binary-dump.test)

(in-suite :utilities.binary-dump)

(defun make-expected (string)
  (let* ((lines                  (split-sequence #\Newline string))
         (lines/trimmed          (mapcar (curry #'string-left-trim '(#\Space))
                                         lines))
         (expected               (format nil "~{~A~^~%~}"   lines/trimmed))
         (expected/logical-block (format nil "~{| ~A~^~%~}" lines/trimmed)))
    (values expected expected/logical-block)))

(test binary-dump.smoke
  "Smoke test for the `binary-dump' function."

  (mapc
   (lambda+ ((data args expected/raw))
     (let+ (((&flet dump (stream)
               (let ((*print-pretty* t)
                     (*print-base*   8)
                     (*print-case*   :downcase)) ; for hexadecimal cases
                (apply #'binary-dump data :stream stream args))))
            ((&flet dump-string ()
               (with-output-to-string (stream)
                 (dump stream))))
            ((&flet dump-string/logical-block ()
               (with-output-to-string (stream)
                 (pprint-logical-block (stream (list data) :per-line-prefix "| ")
                   (dump stream)))))
            ;; Expectations
            ((&values expected expected/logical-block)
             (make-expected expected/raw)))
       (is (string= expected               (dump-string)))
       (is (string= expected/logical-block (dump-string/logical-block)))))

   `(;; Default base (set to 8 via `*print-base*' binding above)
     (,*octets-1* (:width  8)                          "001  .
                                                        017  .
                                                        377  .
                                                        101  A   ")
     (,*octets-1* (:width 16)                          "001 017 377  ...
                                                        101          A   ")
     (,*octets-1* (:width 24)                          "001 017 377 101  ...A    ")
     ;; Base 10
     (,*octets-1* (:width  8 :base 10)                 "001  .
                                                        015  .
                                                        255  .
                                                        065  A   ")
     (,*octets-1* (:width 16 :base 10)                 "001 015 255  ...
                                                        065          A   ")
     (,*octets-1* (:width 24 :base 10)                 "001 015 255 065  ...A    ")
     ;; Base 16
     (,*octets-1* (:width  8 :base 16)                 "01 .
                                                        0F .
                                                        FF .
                                                        41 A    ")
     (,*octets-1* (:width 16 :base 16)                 "01 0F FF ...
                                                        41       A      ")
     (,*octets-1* (:width 24 :base 16)                 "01 0F FF 41    ...A     ")
     ;; Offsets
     (,*octets-1* (:width  8 :base 16 :offset-base  8) "0 01 .
                                                        1 0F .
                                                        2 FF .
                                                        3 41 A ")
     (,*octets-1* (:width  8 :base 16 :offset-base 16) "0 01 .
                                                        1 0F .
                                                        2 FF .
                                                        3 41 A ")
     ;; Sequence limits
     (,*octets-1* (:width 16 :start 1)                 "017 377 101  ..A ")
     (,*octets-1* (:width 16 :end 1)                   "001          .   ")
     ;; Line limit
     (,*octets-1* (:width  8 :lines 1)                 "001  .   "))))
