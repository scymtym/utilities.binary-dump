;;;; util.lisp --- Utility functions used in the utilities.binary-dump system.
;;;;
;;;; Copyright (C) 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:utilities.binary-dump)

(defun %stream-remaining-columns (stream)
  (let ((right-margin (or *print-right-margin*
                          #+sbcl (if (sb-pretty:pretty-stream-p stream)
                                     (sb-pretty::pretty-stream-line-length stream)
                                     (sb-impl::line-length stream))
                          80))
        (column       (or #+sbcl (when (sb-pretty:pretty-stream-p stream)
                                   (sb-pretty::logical-block-start-column
                                    (car (sb-pretty::pretty-stream-blocks stream))))
                          0)))
    (- right-margin column 4)))
