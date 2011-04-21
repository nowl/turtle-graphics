(asdf:defsystem :turtle-graphics
    :description "A turtle graphics simulator."
    :version "0.1"
    :author "Nowl <esologic@gmail.com>"
    :license "GNU Public License"
    :depends-on ("cl-opengl"
                 "cl-glut"
                 "cl-glu")
    :components ((:file "package")
                 (:file "vect-utils")
                 (:file "display")
                 (:file "turtle"))
    :serial t)
