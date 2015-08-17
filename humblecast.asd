;;;; humblecast.asd

(asdf:defsystem #:humblecast
  :description "Describe humblecast here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:drakma
               #:zs3
               #:cxml
               #:closer-mop
               #:clss)
  :serial t
  :components ((:file "package")
               (:file "specials")
               (:file "jpeg")
               (:file "utility")
               (:file "pubdate")
               (:file "duration")
               (:file "store")
               (:file "scraping")
               (:file "protocols")
               (:file "podcast")
               (:file "generate-xml")
               (:file "s3")
               (:file "humblecast")
               (:file "update")
               (:file "interface")))

