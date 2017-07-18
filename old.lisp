
;; ;;;; UTILS

;; (defun read-uint32 (stream) "TODO" (read-uint stream 4))

;; (defun read-uint64 (stream) "TODO" (read-uint stream 8))

;; (defun read-uint (stream nbytes)
;;   "TODO"
;;   (loop for i from 0 below nbytes
;;         for byte = (read-byte stream)
;;         sum (* byte (ash 1 (* 8 i)))))

;; ;;;; CONDITIONS

;; (define-condition lzma-error (error)
;;   ((message :initarg :message
;;             :reader lzma-error-message))
;;   (:report (lambda (condition stream)
;;              (princ (lzma-error-message condition) stream))))

;; ;;;; FUNCTIONS

;; (defun decode-lzma-properties (byte)
;;   "TODO"
;;   (let ((properties byte) lc lp pb)
;;     (when (>= properties (* 9 5 5))
;;       (error 'lzma-error :message "Incorrect LZMA properties."))
;;     (setf (values properties lc) (floor properties 9)
;;           (values pb lp) (floor properties 5))
;;     (values lc lp pb)))

;; (defun decode-lzma-header (stream)
;;   "TODO"
;;   (multiple-value-bind (lc lp pb) (decode-lzma-properties (read-byte stream))
;;     (let ((dict-size (max (ash 1 12) (read-uint32 stream)))
;;           (uncompressed-size (read-uint64 stream)))
;;       (when (= uncompressed-size (ldb (byte 64 0) -1))
;;         (setf uncompressed-size nil))
;;       (values lc lp pb dict-size uncompressed-size))))
