;;;; protocols.lisp

(in-package #:humblecast)

(defgeneric language (podcast))
(defgeneric items (channel))
(defgeneric managing-editor (channel))
(defgeneric copyright (channel))
(defgeneric generator (channel))
(defgeneric docs (channel))

(defgeneric author (object))
(defgeneric category (object))
(defgeneric owner (object))
(defgeneric owner-name (object))
(defgeneric owner-email (object))
(defgeneric pub-date (object))
(defgeneric last-build-date (object))
(defgeneric title (object))
(defgeneric image (object))
(defgeneric summary (object))

(defgeneric link (object))
(defgeneric description (object))
(defgeneric guid (object))
(defgeneric explicit (object))
(defgeneric subtitle (object))
(defgeneric summary (object))
(defgeneric duration (object))
(defgeneric size (object))
(defgeneric enclosure-url (object))

(defgeneric new-feed-url (object))
(defgeneric completep (object))

(defgeneric blocked (object))
