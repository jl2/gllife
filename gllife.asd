;;;; gllife.asd

(asdf:defsystem #:gllife
  :serial t
  :version "0.1"
  :description "Conway's Game Of Life in Common Lisp using OpenGL and lispbuilder-sdl."
  :author "Jeremiah LaRocco <jeremiah.larocco@gmail.com"
  :license "ISC"
  :depends-on (#:lispbuilder-sdl
               #:cl-opengl
               #:cl-glu)
  :components ((:static-file "LICENSE")
			   (:file "package")
               (:file "gllife")))

