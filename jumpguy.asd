;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(defpackage #:jumpguy-asd
  (:use :cl :asdf))

(in-package :jumpguy-asd)

(defsystem jumpguy
  :name "jumpguy"
  :version "0.1"
  :author "yourname"
  :components ((:file "packages")
               (:file "src/jumpguy"))
  :depends-on (#:vert))
