;;;; podcast.lisp

(in-package #:humblecast)

(defclass podcast ()
  ((title
    :reader title
    :initarg :title)
   (link
    :reader link
    :initarg :link)
   (description
    :reader description
    :initarg :description)
   (language
    :reader language
    :initarg :language)
   (copyright
    :reader copyright
    :initarg :copyright)
   (managing-editor
    :reader managing-editor
    :initarg :managing-editor)
   (pub-date
    :reader pub-date
    :initarg :pub-date)
   (last-build-date
    :reader last-build-date
    :initarg :last-build-date)
   (docs
    :reader docs
    :initarg :docs)
   (author
    :reader author
    :initarg :author)
   (explicit
    :reader explicit
    :initarg :explicit)
   (image
    :reader image
    :initarg :image)
   (owner-name
    :reader owner-name
    :initarg :owner-name)
   (owner-email
    :reader owner-email
    :initarg :owner-email)
   (summary
    :reader summary
    :initarg :summary)
   (subtitle
    :reader subtitle
    :initarg :subtitle)
   (completep
    :reader completep
    :initarg :completep)
   (new-feed-url
    :reader new-feed-url
    :initarg :new-feed-url)
   (items
    :reader items
    :initarg :items)
   (blocked
    :reader blocked
    :initarg :blocked)
   (category
    :reader category
    :initarg :category))
  (:default-initargs
   :language "en-us"
   :pub-date (get-universal-time)
   :last-build-date (get-universal-time)
   :explicit "no"
   :completep nil
   :new-feed-url nil
   :blocked nil
   :category "Comedy"))

(defmethod initialize-instance :after ((podcast podcast)
                                       &key
                                         owner-name
                                         owner-email
                                         summary
                                         description)
  (flet ((maybe-set-slot (slot-name value)
           (when (and value (not (slot-boundp podcast slot-name)))
             (setf (slot-value podcast slot-name) value))))
    (maybe-set-slot 'summary description)
    (maybe-set-slot 'description summary)
    (maybe-set-slot 'author owner-name)
    (maybe-set-slot 'copyright owner-name)
    (maybe-set-slot 'managing-editor
                    (and owner-name owner-email
                         (format nil "~A (~A)" owner-email owner-name)))))

(defclass item ()
  ((title
    :reader title
    :initarg :title)
   (link
    :reader link
    :initarg :link)
   (pub-date
    :reader pub-date
    :initarg :pub-date)
   (explicit
    :reader explicit
    :initarg :explicit)
   (subtitle
    :reader subtitle
    :initarg :subtitle)
   (summary
    :reader summary
    :initarg :summary)
   (description
    :reader description
    :initarg :description)
   (duration
    :reader duration
    :initarg :duration)
   (guid
    :reader guid
    :initarg :guid)
   (enclosure-url
    :reader enclosure-url
    :initarg :enclosure-url)
   (size
    :reader size
    :initarg :size))
  (:default-initargs
   :pub-date (get-universal-time)
   :explicit "no"
   :subtitle ""))

(defmethod print-object ((item item) stream)
  (print-unreadable-object (item stream :type t)
    (format stream "~S from ~A"
            (title item)
            (iso8601-date-string (pub-date item)))))

(defparameter *required-item-slots*
  '(title
    link
    pub-date
    explicit
    subtitle
    duration
    size))

(defmethod initialize-instance :after ((item item)
                                       &key
                                         summary
                                         description
                                         enclosure-url
                                         link)
  (flet ((maybe-set-slot (slot-name value)
           (when (and value (not (slot-boundp item slot-name)))
             (setf (slot-value item slot-name) value)))
         (check-slot (slot-name)
           (unless (and (slot-boundp item slot-name)
                        (slot-value item slot-name))
             (error "~S must be provided for an item to be valid"
                    slot-name))))
    #+nil
    (dolist (slot *required-item-slots*)
      (check-slot slot))
    (maybe-set-slot 'summary description)
    (maybe-set-slot 'description summary)
    (maybe-set-slot 'enclosure-url link)
    (maybe-set-slot 'link enclosure-url)))
