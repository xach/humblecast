;;;; scraping.lisp

(in-package #:humblecast)

(defclass page ()
  ((base
    :initarg :base
    :reader base)
   (file
    :initarg :file
    :reader file)
   (headers
    :initarg :headers
    :reader headers)))

(defmethod print-object ((page page) stream)
  (print-unreadable-object (page stream :type t)
    (format stream "~S" (puri:render-uri (base page) nil))))

(defmethod url ((page page))
  (puri:render-uri (base page) nil))

(defvar *trimmed-whitespace*
  (list #\Newline #\Space #\Tab #\Return))

(defun head (url)
  (multiple-value-bind (body status headers)
      (drakma:http-request url :want-stream nil
                           :method :head)
    (declare (ignore body))
    (if (= status 200)
        headers
        (values nil
                (make-condition 'unexpected-http-status
                                :url url
                                :status status
                                :headers headers)))))

(defclass mp3-link ()
  ((url
    :initarg :url
    :reader url
    :reader storage-id)
   (description
    :initarg :description
    :reader description)))

(defmethod print-object ((object mp3-link) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "~S" (description object))))

(defmethod initargs-plist ((mp3-link mp3-link))
  (list :url (url mp3-link)
        :description (description mp3-link)))

(defmethod slot-unbound ((class t) link (slot-name (eql 'page)))
  (setf (page link) (fetch (url link))))

(defmethod headers ((mp3-link mp3-link))
  (headers (page mp3-link)))

(defgeneric last-modified (object)
  (:method (object)
    (let* ((headers (headers object))
           (date-string (cdr (assoc :last-modified headers))))
      (when date-string
        (drakma:parse-cookie-date date-string)))))

(defun trim-anchor-text (text)
  (remove-if (lambda (c) (member c '(#\Return #\Newline)))
             (string-trim *trimmed-whitespace* text)))

(defun merge-urls (url base)
  (puri:render-uri (puri:merge-uris url base) nil))

(defun uri-file-namestring (uri)
  (file-namestring (puri:uri-path (puri:uri uri))))

(defun file-size (file)
  (with-open-file (stream file :element-type '(unsigned-byte 8))
    (file-length stream)))

(defun fetch-from-cache (url)
  (multiple-value-bind (body-file headers age)
      (cachedp url *store*)
    (when body-file
      (values (make-instance 'page
                             :base (puri:uri url)
                             :file body-file
                             :headers headers)
              age))))

(defun fetch (url &key refetch)
  (multiple-value-bind (page age)
      (fetch-from-cache url)
    (cond ((and page (< age *max-cache-age*))
           (values page :cached))
          (t
           (multiple-value-bind (file headers status)
               (fetch-into-store url *store*
                                 :refetch refetch)
             (values (make-instance 'page
                                    :base (puri:uri url)
                                    :file file
                                    :headers headers)
                     status))))))

(defun deadp (link)
  (sget (list "dead-links" (url link)) *store*))

(defun mark-dead (link)
  (setf (sget (list "dead-links" (url link)) *store*) t))

(defun processedp (link)
  (or (deadp link)
      (sget (list "processed-links" (url link)) *store*)))

(defun mark-processed (link)
  (setf (sget (list "processed-links" (url link)) *store*) t))

(defun mark-unprocessed (link)
  (setf (sget (list "processed-links" (url link)) *store*) nil))

(defun podcast-bucket ()
  (or (sget (list "s3" "bucket") *store*)
      (error "No key for (\"s3\" \"bucket\") in store")))

(defun process-link (link)
  (unless (processedp link)
    ;; FIXME: Check amazon *first*
    (handler-case
        (let* ((mp3-file-name (uri-file-namestring (url link)))
               (page (fetch (url link)))
               (mp3-file (file page))
               (key (format nil "audio/~A" mp3-file-name))
               (bucket (podcast-bucket))
               (s3-link (zs3:resource-url :bucket bucket
                                          :key key
                                          :vhost :cname))
               (duration (pretty-duration (mp3-file-duration mp3-file)))
               (size (file-size mp3-file )))
          (unless (head s3-link)
            (zs3:put-file mp3-file bucket key
                          :public t
                          :content-type "audio/mpeg"))
          (let ((item (make-instance 'humble-item
                                     :original-url (url link)
                                     :description (description link)
                                     :pub-date (last-modified page)
                                     :title (description link)
                                     :summary (description link)
                                     :link s3-link
                                     :duration duration
                                     :size (princ-to-string size)
                                     :enclosure-url s3-link
                                     :guid (url link))))
            (store item)
            (mark-processed link)))
      (unexpected-http-status ()
        (mark-dead link)
        (return-from process-link :dead)))))


(defun link-item (link)
  (if (deadp link)
      nil
      (restore 'humble-item
               (url link))))
