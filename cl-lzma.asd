;;;; cl-lzma.asd

(asdf:defsystem #:cl-lzma
  :description "LZMA decompressor in portable Common Lisp"
  :author "Michał \"phoe\" Herda <phoe@openmailbox.org>"
  :license "BSD 2-clause"
  :serial t
  :depends-on (:binary-types)
  :components ((:file "package")
               (:file "cl-lzma")))
