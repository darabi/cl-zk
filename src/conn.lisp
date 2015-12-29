(in-package :cl-zk)

;; TODO: Have the Makefile make install the proper .so file
;; TODO: Document how to have this work properly
(cffi:define-foreign-library zookeeper
  (:unix
   (:or "libzookeeper.so"
        "libzookeeper_mt.so"
        "libzookeeper_st.so")))

(cffi:use-foreign-library zookeeper)

(defvar np (cffi:null-pointer))

(cffi:defcvar ("errno" *errno* :read-only t) :int)

(defun get-data (zhandle path watch stat)
  ;; TODO: How to handle dynamically sized get-data calls, without
  ;; resorting to just allocating a buffer the max size of data
  ;; allowed?
  (let ((buf-size 512))
    (cffi:with-foreign-object (buf :char buf-size)
      (with-pointer-to-int (i buf-size)
        (let* ((get-result (%zoo-get zhandle path watch buf i stat))
               (get-kw (cffi:foreign-enum-keyword 'zoo-errors get-result)))
          (ccase get-kw
            ;; TODO: handle other cases
            (:zok
             (cffi:foreign-string-to-lisp buf))))))))

;; TODO: defgeneric this have it operate on strings and buffers and
;; streams and all that good stuff. Implement it with just strings
;; now.
(defun set-data (zhandle path data &key (version -1))
  (cffi:with-foreign-string (buf data)
    (%zoo-set zhandle path buf (+ (length data) 1) version)))

(defun exists? (zhandle path watch stat)
  (let* ((exists? (%zoo-exists zhandle path watch stat))
         (exists-kw (cffi:foreign-enum-keyword 'zoo-errors exists?)))
    (ccase exists-kw
      (:zok T)
      (:znonode NIL)
      ;; This should signal an error since people would use this like:
      ;; (when (exists? ..)) and then the kw returned would be truthy
      (t exists-kw))))

(defun make-connection (host &key (timeout 10000) (clientid np) log-callback)

  (let ((conn (if log-callback
                  ;; TODO: This blocks when a debugged connection tries to do something with it.
                  ;; TODO: Debug^^
                  ;; TODO: This requires a defcallback'd function, not a Lisp function.
                  ;; TODO: Provide a way to pass a proper Lisp function.
                  (zookeeper-init2 host np timeout clientid np 0 logcallback)
                  (zookeeper-init host np timeout clientid np 0))))
    (if (eq np conn)
        ;; TODO: make this a signal/restart
        (error (format nil "Error creating connection: ~d" *errno*))
        conn)))

(cffi:defcallback logcb :void ((message :string))
  (format t "~A~%" message))
