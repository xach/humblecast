;;;; humblecast.lisp

(in-package #:humblecast)

;;; "humblecast" goes here. Hacks and glory await!

(defun publish-cover-image (file)
  (multiple-value-bind (width height)
      (jpeg-dimensions file)
    (unless (and width height)
      (error "No dimensions found in ~A -- not a JPEG?" file))
    (unless (and (<= 1400 height)
                 (<= 1400 width))
      (error "Image should be >1400px square"))
    (zs3:put-file file (podcast-bucket) "cover.jpg"
                  :public t
                  :content-type "image/jpeg")
    (values (zs3:resource-url :bucket (podcast-bucket)
                              :key "cover.jpg"
                              :vhost :cname)
            width height)))

(defclass humblecast (podcast)
  ((title
    :reader title
    :initform "The humble Farmer")
   (link
    :reader link
    :initform "http://www.thehumblefarmer.com/")
   (description
    :reader description
    :initform (trim-anchor-text *description*))
   (language
    :reader language
    :initform "en-us")
   (copyright
    :reader copyright
    :initform "Robert Skoglund")
   (managing-editor
    :reader managing-editor
    :initform "thehumblefarmer@gmail.com (Robert Karl Skoglund)")
   (pub-date
    :reader pub-date
    :initform (get-universal-time))
   (last-build-date
    :reader last-build-date
    :initform (get-universal-time))
   (docs
    :reader docs
    :initform "http://www.thehumblefarmer.com/")
   (author
    :reader author
    :initform "Robert Skoglund")
   (explicit
    :reader explicit
    :initform "no")
   (image
    :reader image
    :initform "http://humblecast.thehumblefarmer.com/cover.jpg")
   (owner-name
    :reader owner-name
    :initform "Robert Skoglund")
   (owner-email
    :reader owner-email
    :initform "thehumblefarmer@gmail.com")
   (summary
    :reader summary
    :initform (trim-anchor-text *description*))
   (subtitle
    :reader subtitle
    :initform "Maine Private Radio and No Things Considered")
   (completep
    :reader completep
    :initform nil)
   (new-feed-url
    :reader new-feed-url
    :initform nil)
   (items
    :reader items
    :initform nil)
   (blocked
    :reader blocked
    :initform nil)
   (category
    :reader category
    :initform '("Comedy" "Music")))
  (:default-initargs
   :category '("Comedy" "Music")))

(defclass humble-item (item)
  ((original-url
    :initarg :original-url
    :reader original-url
    :reader storage-id)))


