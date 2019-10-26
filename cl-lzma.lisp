;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CL-LZMA
;;;; © Michał "phoe" Herda 2017
;;;; cl-lzma.lisp

(in-package #:cl-lzma)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; FFI

(eval-when (:compile-toplevel :load-toplevel :execute)
  (declaim (inline system-relative-namestring))
  (defun system-relative-namestring (system name)
    (namestring (asdf:system-relative-pathname system name))))

(cffi:define-foreign-library lzma
  (:darwin #.(system-relative-namestring
              :cl-lzma
              #+x86 "bin/lzma-mac32.dylib"
              #+x86-64 "bin/lzma-mac64.dylib"))
  (:unix #.(system-relative-namestring
            :cl-lzma
            #+x86 "bin/lzma-lin32.so"
            #+x86-64 "bin/lzma-lin64.so"))
  (:windows #.(system-relative-namestring
               :cl-lzma
               #+x86 "bin/lzma-win32.dll"
               #+x86-64 "bin/lzma-win64.dll"))
  (t (:default "lzma")))

(cffi:use-foreign-library lzma)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CL-Autowrap

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *spec-path*
    (asdf:system-relative-pathname :cl-lzma "spec/"))
  (c-include (merge-pathnames "lzma.h" *spec-path*)
             :spec-path *spec-path*
             :definition-package :cl-lzma/autowrap
             :function-package :cl-lzma/autowrap
             :wrapper-package :cl-lzma/autowrap
             :accessor-package :cl-lzma/autowrap
             :constant-package :cl-lzma/autowrap
             :extern-package :cl-lzma/autowrap))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; C callbacks

(defcallback lzma-alloc :pointer ((allocptr :pointer) (size size-t))
  (declare (ignore allocptr))
  (foreign-alloc :char :count size))

(defcallback lzma-free :void ((allocptr :pointer) (address :pointer))
  (declare (ignore allocptr))
  (unless (cffi:null-pointer-p address)
    (foreign-free address)))

(defvar *alloc-functions*
  (let* ((ptr (cffi:foreign-alloc :pointer :count 2))
         (struct (make-i-sz-alloc :ptr ptr)))
    (setf (i-sz-alloc.alloc struct) (autowrap:callback 'lzma-alloc)
          (i-sz-alloc.free struct) (autowrap:callback 'lzma-free))
    struct))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Functions

(declaim (inline %init-props))
(defun %init-props (props input-size)
  (let ((dict-size (min input-size (expt 2 20))))
    (lzma-enc-props-init (c-lzma-enc-props-ptr props))
    (setf (c-lzma-enc-props.dict-size props) dict-size
          (c-lzma-enc-props.fb props) 40)))

;;; TODO fix variable names
(defun lzma-compress (vector)
  (check-type vector vector)
  (let ((dest-len (truncate (max 1024 (* (length vector) 1.5)))))
    (with-static-vectors ((src-vector (length vector) :initial-contents vector)
                          (dest-vector dest-len))
      (let* ((src-len (length vector))
             (props-vector (make-static-vector 5))
             (src (static-vector-pointer src-vector))
             (dest (static-vector-pointer dest-vector))
             (props-encoded (static-vector-pointer props-vector)))
        (with-many-alloc ((props 'c-lzma-enc-props 1)
                          (dest-len-ptr :unsigned-int 1)
                          (props-size :unsigned-int 1))
          (setf (mem-ref dest-len-ptr :unsigned-int) dest-len
                (mem-ref props-size :unsigned-int) 5)
          (%init-props props src-len)
          (let ((status (lzma-encode dest dest-len-ptr src src-len
                                     props props-encoded props-size
                                     0 (cffi:null-pointer)
                                     (i-sz-alloc-ptr *alloc-functions*)
                                     (i-sz-alloc-ptr *alloc-functions*))))
            (unless (= status +sz-ok+)
              (error "LZMA compression failed with code ~D." status))
            (let* ((output-length (mem-ref dest-len-ptr :unsigned-int))
                   (result-vector (make-static-vector output-length))
                   (result (static-vector-pointer result-vector)))
              (replace-foreign-memory result dest output-length)
              (values result-vector props-vector src-len))))))))

(defun lzma-decompress (vector props-enc unc-len)
  "Deprecated API for decompression."
  ;; (warn "LZMA-DECOMPRESS is deprecated. Please use DECOMPRESS-FROM-* ~
  ;; functions instead.")
  (check-type vector vector)
  (check-type props-enc (vector * 5))
  (check-type unc-len (unsigned-byte 64))
  (with-static-vectors ((data (length vector) :initial-contents vector)
                        (props (length props-enc) :initial-contents props-enc))
    (decompress-from-static-vectors data props unc-len)))

;;; TODO export them all
(defun decompress-from-vector (vector)
  "Decompresses the raw LZMA data from VECTOR.
\
Arguments:
* VECTOR - must be a (VECTOR (UNSIGNED-BYTE 8)).
Return value: a static octet-vector."
  (assert (equal (array-element-type vector) '(unsigned-byte 8)))
  (with-fast-input (buffer vector)
    (decompress-from-buffer buffer)))

(defun decompress-from-stream (stream)
  "Decompresses the raw LZMA data from STREAM.
\
Arguments:
* STREAM - must be a stream of (UNSIGNED-BYTE 8)s.
Return value: a static octet-vector."
  (assert (subtypep (stream-element-type stream) '(unsigned-byte 8)))
  (with-fast-input (buffer nil stream)
    (decompress-from-buffer buffer)))

(defun decompress-from-buffer (buffer)
  "Decompresses the raw LZMA data from BUFFER.
\
Arguments:
* BUFFER - must be a FAST-IO input buffer of (UNSIGNED-BYTE 8)s.
Return value: a static octet-vector."
  (with-static-vectors ((props 5))
    (fast-read-sequence props buffer)
    (let* ((length (readu64-le buffer))
           (safe-length (max 1024 (truncate (* 1.5 length)))))
      ;; TODO add COMPRESSED-LENGTH instead of the above SAFE-LENGTH shamanism
      (with-static-vectors ((data safe-length))
        (fast-read-sequence data buffer)
        (decompress-from-static-vectors data props length)))))

(defun decompress-from-static-vectors (data props-encoded decompressed-length)
  "Decompresses the raw LZMA data from VECTOR, using the provided PROPS-ENCODED
and LENGTH.
\
Arguments:
* VECTOR - vector representing LZMA compressed data; must be a static
  octet-vector.
* PROPS-ENCODED - vector representing encoded LZMA properties; must be a static
  octet-vector of length 5.
* DECOMPRESSED-LENGTH - integer representing the length of decompressed data;
  must be an (UNSIGNED-BYTE 64).
Return value: a static octet-vector of length DECOMPRESSED-LENGTH."
  (check-type data (vector (unsigned-byte 8)))
  (check-type props-encoded (vector (unsigned-byte 8) 5))
  (check-type decompressed-length (unsigned-byte 64))
  (%decompress (static-vector-pointer data)
               (static-vector-pointer props-encoded)
               (length data) decompressed-length))

(defun %decompress (data-pointer props-pointer
                    compressed-length decompressed-length)
  "Internal decompression function.
\
Arguments:
* DATA-POINTER - foreign pointer to memory containing LZMA compressed data.
* PROPS-POINTER - foreign pointer to memory containing encoded LZMA properties.
* COMPRESSED-LENGTH - integer representing the length of compressed data.
* DECOMPRESSED-LENGTH - integer representing the length of decompressed data.
Return value: a static octet-vector of length DECOMPRESSED-LENGTH."
  (let* ((result (make-static-vector decompressed-length))
         (result-pointer (static-vector-pointer result)))
    (with-many-alloc ((e-lzma-status 'e-lzma-status 1)
                      (proc-out-size :unsigned-long 1)
                      (proc-in-size :unsigned-long 1))
      (setf (mem-ref proc-out-size :unsigned-long) decompressed-length
            (mem-ref proc-in-size :unsigned-long) compressed-length)
      (let ((status (lzma-decode result-pointer proc-out-size data-pointer
                                 proc-in-size props-pointer 5 +lzma-finish-end+
                                 e-lzma-status
                                 (i-sz-alloc-ptr *alloc-functions*))))
        (let ((actual-length (mem-ref proc-out-size :unsigned-int)))
          (unless (= status +sz-ok+)
            (error "LZMA compression failed with code ~D." status))
          (unless (= decompressed-length actual-length)
            (error "Expected to uncompress ~D bytes, but got ~D bytes."
                   decompressed-length actual-length))
          result)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Tests

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun random-vector (length)
    (let ((vector (make-array length :element-type '(unsigned-byte 8)))
          (*random-state* (make-random-state t)))
      (loop for i below (length vector)
            do (setf (aref vector i) (random 256)))
      vector)))

(defun %lzma-test-input ()
  (list (make-array 1000 :element-type '(unsigned-byte 8))
        (make-array 1000000 :element-type '(unsigned-byte 8))
        (make-array 1000 :element-type '(unsigned-byte 8)
                         :initial-element 255)
        (make-array 1000000 :element-type '(unsigned-byte 8)
                            :initial-element 255)
        (make-array 100000000 :element-type '(unsigned-byte 8)
                              :initial-element #b10101010)
        (random-vector 1000)
        (random-vector 1000000)))

(defun %lzma-test (input)
  (apply #'lzma-decompress (multiple-value-list (lzma-compress input))))

(defun lzma-test (&optional quietp)
  (loop for input in (%lzma-test-input)
        with output = nil
        do (let ((*print-length* 8))
             (unless quietp
               (format t "Testing vector ~S of length ~D..."
                       input (length input)))
             (setf output (%lzma-test input)))
        if (equalp input output)
          do (unless quietp (format t "Passed.~%"))
        else do (error "Test failed for vector of length ~S.~%Input:~A~%"
                       (length input) input))
  (lzma-fuzz 1000 1000 quietp t))

(defun lzma-fuzz (iterations vector-length &optional quietp random-length-p)
  (let ((*random-state* (make-random-state t)))
    (loop for i below iterations
          for input = (random-vector (if random-length-p
                                         (1+ (random vector-length))
                                         vector-length))
          with output = nil
          do (let ((*print-length* 8))
               (unless quietp
                 (format t "Testing array ~S of length ~D..."
                         input (length input)))
               (setf output (%lzma-test input)))
          if (equalp input output)
            do (unless quietp (format t "Passed.~%"))
          else do (error "Test failed for vector of length ~S.~%Input:~A~%"
                         (length input) input)))
  (values))
