(require 'sb-posix)

;;; Define Portacle system support package
(defpackage #:portacle
  (:use #:cl)
  (:shadow
   #:load)
  (:export
   #:*platform*
   #:*binary*
   #:*root*
   #:find-root
   #:path
   #:load))
(in-package #:portacle)

(defvar *platform*
  #+linux "lin"
  #+win32 "win"
  #+darwin "mac")

(defvar *binary*
  (make-pathname :name NIL :type NIL :defaults (first sb-ext:*posix-argv*)))

(defun find-root (&optional (source *binary*))
  (let ((dir (make-pathname :name NIL :type NIL :defaults source)))
    (labels ((try (dir)
               (cond ((probe-file (make-pathname :name ".portacle_root" :defaults dir))
                      dir)
                     ((rest (pathname-directory dir))
                      (try (make-pathname :directory (butlast (pathname-directory dir)) :defaults dir)))
                     (T
                      (warn "Failed to find the Portacle root directory!")
                      *binary*))))
      (try dir))))

(defvar *root*
  (or (sb-posix:getenv "ROOT")
      (find-root)))

(defun path (file &optional (platform *platform*))
  (merge-pathnames (format NIL "~@[~a/~]~a" platform file) *root*))

(defun load (file &optional (platform *platform*))
  (let ((file (path file platform)))
    (when (probe-file file) (cl:load file))))

;;; Configure the system for Portacle.
(in-package #:cl-user)

;; Fix up the source locations
(sb-ext:set-sbcl-source-location (portacle:path "sbcl/share/src/"))

;; Load ASDF
#-asdf3
(or (portacle:load "asdf/asdf.fasl")
    (portacle:load "asdf/asdf.lisp")
    (warn "Failed to load ASDF."))

;; Fix up the ASDF cache location
#+asdf3
(setf asdf:*user-cache*
      (merge-pathnames (format NIL "~a-~a-~a-~a/"
                               (lisp-implementation-type)
                               (lisp-implementation-version)
                               (software-type)
                               (machine-type))
                       (portacle:path "asdf/cache/")))

;; Load quicklisp
#-quicklisp
(or (portacle:load "quicklisp/setup.lisp" "all")
    (warn "Failed to load quicklisp."))

;; Add the project folder to Quicklisp's local-projects directories.
#+quicklisp
(pushnew (portacle:path "projects/" NIL) ql:*local-project-directories*)

;; All set.
(push :portacle *features*)
