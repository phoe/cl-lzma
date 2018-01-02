;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CL-LZMA
;;;; © Michał "phoe" Herda 2017
;;;; cl-lzma.asd

(asdf:defsystem #:cl-lzma
  :description "CFFI wrapper around LZMA (de)compressor foreign library"
  :author "Michał \"phoe\" Herda <phoe@openmailbox.org>"
  :license "Public domain"
  :serial t
  :depends-on (#:cl-autowrap
               #:cffi
               #:static-vectors
               #:fast-io)
  :components ((:file "package")
               (:file "cl-lzma")))
