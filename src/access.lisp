;;;; access.lisp --- Access to binary data.
;;;;
;;;; Copyright (C) 2014, 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:utilities.binary-dump)

(defvar *unit-accessors*
  (make-hash-table :test #'equal #+sbcl :synchronized #+sbcl t))

(defun find-unit-accessor/no-cache (width endian type)
  (when (and (= width 8) (eq type 'unsigned-byte ))
    (return-from find-unit-accessor/no-cache 'aref))

  (format-symbol :nibbles "~A~AREF/~A"
                 (ecase type
                   (unsigned-byte '#:ub)
                   (signed-byte   '#:sb)
                   (float         '#:ieee))
                 (case type
                   ((unsigned-byte signed-byte) width)
                   (float                       (ecase width
                                                  (32 '#:-single-)
                                                  (64 '#:-double-))))
                 (ecase endian
                   (:little '#:le)
                   (:big    '#:be))))

(defun find-unit-accessor (width endian type)
  (let ((key (list width endian type)))
    (ensure-gethash
     key *unit-accessors*
     (find-unit-accessor/no-cache width endian type))))

(declaim (ftype (function (t sequence positive-integer
                           (member :little :big)
                           (member unsigned-byte signed-byte float)
                           &key (:start non-negative-integer)
                                (:end   non-negative-integer))
                          sequence)
                map-units))
(defun map-units (function data length endian type
                  &key (start 0) (end (length data)))
  "Call FUNCTION on subsequent \"units\" in DATA, return DATA.

   Units are subsequences characterized by and interpreted according
   to LENGTH, ENDIAN and TYPE:

   * LENGTH specifies the number of bits in each unit. Must be 8, 16,
     32 or 64 if TYPE is [un]signed-byte and 32 or 64 if TYPE is
     `float'.

   * ENDIAN specifies the endianess for the interpretation of the
     unit. Possible values: the keywords `:little' and `:big'.

   * TYPE specifies the type for the interpretation of the
     unit. Possible value: the symbols `unsigned-byte', `signed-byte'
     and `float'"
  (unless (zerop (mod length 8))
    (error "~@<~A must be a positive multiple of 8~@:>" 'length))
  (unless (<= start end (length data))
    (error "~@<Bounding indices ~D and ~D are invalid for ~S.~@:>"
           start end data))

  (let ((function (coerce function 'function))
        (accessor (find-unit-accessor length endian type))
        (step     (floor length 8))) ; number of octets in a unit
    (loop :for offset :from start :below end :by step :do
       (let ((unit       (funcall accessor data offset))
             (last-unit? (>= (+ offset step) end)))
         (funcall function unit last-unit?)))
    data))

(declaim (ftype (function (t sequence positive-integer
                           &key (:start      non-negative-integer)
                                (:end        non-negative-integer)
                                (:max-chunks non-negative-integer))
                          sequence)
                map-chunks))
(defun map-chunks (function data chunk-length
                   &key
                   (start     0)
                   (end       (length data))
                   max-chunks)
  "Call FUNCTION with subsequent chunks of CHUNK-LENGTH octets of DATA.

   Return four values: 1) DATA 2) the start index of the processed
   sub-sequence of DATA (i.e. START) 3) the corresponding end
   index (not necessarily END) 4) the number of processed chunks.

   FUNCTION has to have a lambda-list compatible to the following one:

     (offset data start end last-chunk?)

   where

   * OFFSET is the offset in octets of the current chunk relative to
     the beginning of DATA.

   * DATA passed through unmodified.

   * START and END are the offset in octets of the beginning and end
     of the current chunk relative to the beginning of DATA
     respectively.

   * LAST-CHUNK? is true when the current chunk is the last in DATA.

   The last chunk may be shorter than CHUNK-LENGTH.

   When supplied, START and/or END select a subsequence of DATA for
   processing."
  (unless (<= start end (length data))
    (error "~@<Bounding indices ~D and ~D are invalid for ~S.~@:>"
           start end data))

  (let ((function (coerce function 'function))
        (end      (if max-chunks
                      (min end (* chunk-length max-chunks))
                      end)))
    (loop :for offset :from start :below end :by chunk-length
          :for count :from 1 :do
             (let* ((last-chunk? (>= (+ offset chunk-length) end))
                    (start       offset)
                    (end         (min end (+ offset chunk-length))))
               (funcall function offset data start end last-chunk?))
          :finally (return (values data start (min end offset) count)))))
