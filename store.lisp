;;;; store.lisp

(in-package #:humblecast)

(defvar *store* nil)

(defgeneric relative-to (object path))
(defgeneric sget (key store))
(defgeneric (setf sget) (new-value key store))
(defgeneric key-exists-p (key store))
(defgeneric sdelete (key store))
(defgeneric map-store (fun store &key prefix))

(defmethod relative-to ((pathname pathname) path)
  (merge-pathnames path pathname))

(defclass store ()
  ((path
    :initarg :path
    :reader path)))

(defun store-id-file (store-directory)
  (merge-pathnames "humblecast-store"
                   store-directory))

(defun store-exists-p (store-directory)
  (probe-file (store-id-file store-directory)))

(defun open-store (store-directory)
  (unless (store-exists-p store-directory)
    (error "~A is not a store directory"
           store-directory))
  (make-instance 'store :path store-directory))

(defun create-store (store-directory)
  (let ((id-file (store-id-file store-directory)))
    (when (probe-file id-file)
      (error "~A is already a store directory" store-directory))
    (ensure-directories-exist id-file)
    (open id-file :direction :probe :if-exists :error
          :if-does-not-exist :create)
    (open-store store-directory)))

(defun canonicalize-key (key)
  (typecase key
    (string key)
    (list
     (with-standard-io-syntax
       (let ((*package* (find-package :keyword)))
         (format nil "~{~A~^/~}" key))))
    (t
     (error "Don't know how to process key -- ~A" key))))

(defun encode-key-for-filesystem (key)
  (check-type key string)
  (digest key))

(defmethod relative-to ((store store) path)
  (merge-pathnames path (path store)))

(defun hashed-storage-file (store &key (base "storage") key (type "dat"))
  (check-type key string )
  (let* ((encoded (encode-key-for-filesystem key))
         (a (char encoded 0))
         (b (char encoded 1)))
    (relative-to store
                 (make-pathname :name encoded
                                :type type
                                :directory (list :relative base
                                                 (string a)
                                                 (string b))))))

(defun attribute-storage-file (key store)
  (let* ((encoded (encode-key-for-filesystem key))
         (a (char encoded 0))
         (b (char encoded 1)))
    (relative-to store
                 (make-pathname :name encoded
                                :type "sexp"
                                :directory (list :relative "attributes"
                                                 (string a)
                                                 (string b))))))

(defun all-attribute-files (store)
  (directory (relative-to store (make-pathname :name :wild
                                               :type "sexp"
                                               :directory
                                               (list :relative "attributes"
                                                     :wild-inferiors)))))

(defmethod key-exists-p (key store)
  (setf key (canonicalize-key key))
  (probe-file (attribute-storage-file key store)))

(defun first-form (file)
  (with-open-file (stream file)
    (read stream)))

(defun first-two-forms (file)
  (with-open-file (stream file)
    (list (read stream) (read stream))))

(defun second-form (file)
  (second (first-two-forms file)))

(defun write-forms (forms file)
  (with-standard-io-syntax
    (let ((*package* (find-package '#:keyword))
          (*print-circle* t))
      (with-open-file (stream file :direction :output
                              :if-exists :rename-and-delete
                              :if-does-not-exist :create)
        (dolist (form forms)
          (write form :stream stream)
          (terpri stream))))))

(defmethod sget (key store)
  (setf key (canonicalize-key key))
  (let ((file (key-exists-p key store)))
    (if file
        (values (second-form file) t)
        (values nil nil))))

(defmethod (setf sget) (new-value key store)
  (let* ((ckey (canonicalize-key key))
         (file (attribute-storage-file ckey store)))
    (ensure-directories-exist file)
    (write-forms (list key new-value) file)
    new-value))

(defmethod sdelete (key store)
  (setf key (canonicalize-key key))
  (let ((file (key-exists-p key store)))
    (when file
      (delete-file file)
      t)))

(defun prefix-matcher (prefix)
  (let ((prefix-length (length prefix)))
    (if prefix
        (lambda (key)
          (let ((string (canonicalize-key key)))
            (and (<= prefix-length (length string))
                 (string= prefix string :end2 prefix-length))))
        #'identity)))

(defmethod map-store (fun store &key prefix)
  "Call FUN with two arguments, the key and the value of that key in
the store, for each attribute in STORE. If PREFIX is given, only keys
starting with that prefix are considered."
  (let ((predicate (prefix-matcher (canonicalize-key prefix)))
        key value)
    (dolist (file (all-attribute-files store))
      (with-open-file (stream file)
        (setf key (read stream))
        (let ((ckey (canonicalize-key key)))
          (when (funcall predicate ckey)
            (setf value (read stream))
            (go :matched))
          (go :next)))
      :matched
      (funcall fun key value)
      :next)))

(defun mapcollect-store (fun store &key prefix)
  (let ((result '()))
    (map-store (lambda (key value)
                 (push (funcall fun key value) result))
               store :prefix prefix)
    (nreverse result)))

(defun merge-store-pathname (store pathname)
  (unless (eql (first (pathname-directory pathname))
               :relative)
    (error "Pathname must be relative"))
  (relative-to store pathname))

(defun header-parameters (&rest names-and-values)
  (loop for (name value) on names-and-values by #'cddr
        when value
        collect (cons (string-downcase name) value)))

(defun url-body-file (url store)
  (hashed-storage-file store
                       :base "url-cache"
                       :key url))

(defun store-relative-pathname (store pathname)
  (pathname (enough-namestring pathname (path store))))

(defun url-cache-key (url &rest parts)
  (list* "url-cache" url parts))

(define-condition unexpected-http-status (error)
  ((url
    :reader url
    :initarg :url
    :reader unexpected-http-status-url)
   (status
    :reader unexpected-http-status-status
    :initarg :status)
   (headers
    :reader unexpected-http-status-headers
    :initarg :headers))
  (:report (lambda (condition stream)
             (format stream "Unexpected status ~A for ~A"
                     (unexpected-http-status-status condition)
                     (unexpected-http-status-url condition)))))

(defun fetch-into-store (url store &key refetch)
  (let* ((pathname (url-body-file url store))
         (temp-file (make-pathname :type "tmp"
                                   :defaults pathname))
         (etag (sget (url-cache-key url "etag") store))
         (last-modified (sget (url-cache-key url "last-modified") store))
         (relative-pathname (store-relative-pathname store pathname))
         (additional-headers (and (not refetch)
                                  (header-parameters :if-none-match etag
                                                     :if-modiified-since last-modified))))
    (ensure-directories-exist pathname)
    (multiple-value-bind (stream status headers)
        (drakma:http-request url
                             :want-stream t
                             :additional-headers additional-headers)
      (flet ((save (key value)
               (when value
                 (setf (sget (url-cache-key url key) store) value))))
        (cond ((eql status 304)
               (save "fetched-time" (get-universal-time))
               (values pathname headers :not-modified))
              ((eql status 200)
               (let ((binary-stream (flexi-streams:flexi-stream-stream stream))
                     (buffer (make-array 64000
                                         :element-type '(unsigned-byte 8))))
                 (with-open-file (output-stream temp-file
                                                :direction :output
                                                :element-type '(unsigned-byte 8)
                                                :if-exists :supersede)
                   (loop
                     (let ((last-index (read-sequence buffer binary-stream)))
                       (when (zerop last-index)
                         (return))
                       (write-sequence buffer output-stream :end last-index))))
                 (rename-file temp-file pathname))
               (save "fetched-time" (get-universal-time))
               (save "body-file" relative-pathname)
               (save "headers" headers)
               (save "etag" (drakma:header-value :etag headers))
               (save "last-modified" (drakma:header-value :last-modified headers))
               (values pathname headers :fetched))
              (t
               (error 'unexpected-http-status
                      :headers headers
                      :status status
                      :url url)))))))

(defun url-properties (url store)
  (mapcollect-store #'cons store :prefix (url-cache-key url)))

(defun delete-if-exists (file)
  (when (probe-file file)
    (delete-file file)))

(defun body-file (url store)
  (probe-file
   (relative-to store (sget (url-cache-key url "body-file") store))))

(defun clear-url (url store)
  (let ((body-file (body-file url store)))
    (when body-file
      (delete-file body-file))
    (loop for (key . value) in (url-properties url store)
          do (sdelete key store))))

(defvar *max-cache-age* 86400)

(defun cachedp (url store)
  (let* ((body-file (probe-file (url-body-file url store)))
         (headers (sget (url-cache-key url "headers") store))
         (fetched-time (sget (url-cache-key url "fetched-time") store))
         (age (and fetched-time (- (get-universal-time) fetched-time))))
    (values (and headers body-file)
            headers
            age)))



;;; Humble store



(defgeneric storage-prefix (object)
  (:method (object)
    (string-downcase (class-name (class-of object)))))

(defgeneric storage-id (object))
(defgeneric storage-key (object)
  (:method (object)
    (list "autostore"
          (storage-prefix object)
          (storage-id object))))

(defgeneric initargs-plist (object)
  (:method (object)
    (let ((class (class-of object)))
      (c2mop:finalize-inheritance class)
      (let* ((slots (c2mop:class-slots class))
             (names (mapcar #'c2mop:slot-definition-name slots))
             (initargs (mapcar 'first
                               (mapcar #'c2mop:slot-definition-initargs
                                       slots)))
             (values (mapcar (lambda (name) (slot-value object name)) names)))
        (mapcan #'list initargs values)))))

(defgeneric restore (class key)
  (:method (class key)
    (let* ((object (make-instance class))
           (plist (sget (list "autostore" (storage-prefix object) key) *store*)))
        (apply #'reinitialize-instance object plist))))

(defgeneric store (object)
  (:method (object)
    (setf (sget (storage-key object)
                *store*)
          (initargs-plist object))
    object))

(defgeneric destore (class key)
  (:method (class key)
    (let* ((temp (make-instance class))
           (key (storage-key temp)))
      (sdelete key *store*))))

(defun map-stored-objects (fun class)
  (let* ((temp (make-instance class))
         (prefix (list "autostore" (storage-prefix temp))))
    (map-store (lambda (key value)
                 (declare (ignore key))
                 (funcall fun
                          (apply #'make-instance class
                                 value)))
               *store*
               :prefix prefix)))

(defun stored-objects-of-type (class)
  (let ((result '()))
    (map-stored-objects (lambda (object) (push object result))
                        class)
    result))

(defun lookup (key)
  "Like SGET, but implicitly uses *STORE*."
  (sget key *store*))

(defun (setf lookup) (new-value key)
  (setf (sget key *store*) new-value))
