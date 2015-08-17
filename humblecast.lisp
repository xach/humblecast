;;;; humblecast.lisp

(in-package #:humblecast)

;;; "humblecast" goes here. Hacks and glory await!

(defclass humblecast (podcast)
  ((title
    :reader title
    :initform "The humble Farmer")
   (link
    :reader link
    :initform "http://www.thehumblefarmer.com/")
   (description
    :reader description
    :initform "Join host Robert Skoglund, The humble Farmer, for a weekly podcast featuring old fashioned music and dry Maine humor.")
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
    :initform "Join host Robert Skoglund, The humble Farmer, for a weekly podcast featuring old fashioned music and dry Maine humor.")
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


