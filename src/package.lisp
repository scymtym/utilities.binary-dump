;;;; package.lisp --- Package definition for the utilities.binary-dump system.
;;;;
;;;; Copyright (C) 2014, 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:utilities.binary-dump
  (:use
   #:cl
   #:alexandria
   #:let-plus
   #:nibbles)

  ;; Chunk and unit access protocol
  (:export
   #:map-chunks
   #:map-units)

  ;; Formatting protocol
  (:export
   #:binary-dump
   #:print-binary-dump)

  (:documentation
   "This package contains functions for printing binary data.

    The formatting possibilities resemble some of the ways supported
    by the od(1) UNIX program.

    The functions `binary-dump' and `print-binary-dump' constitute the
    API. The former is intended to be called directly while the latter
    is intended for use in ~/ `cl:format' directives."))
