;;;; package.lisp --- Package definition for the utilities.binary-dump system.
;;;;
;;;; Copyright (C) 2014 Jan Moringen
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
   "TODO"))
