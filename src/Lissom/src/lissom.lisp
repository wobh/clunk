;;;; lissom.lisp

#|
DESCRIPTION

Common Lisp blog generator like blosxom

AUTHORS

William Clifford (wobh@yahoo.com)

NOTES

http://www.blosxom.com/
http://pyblosxom.github.com/

- lissom :: lithe, nimble, supple
|#

(defpackage "ORG.WOBH.LISSOM"
  (:nicknames "LISSOM" "LISXOM")
  (:use "COMMON-LISP")
  ;;(:export)
  (:documentation "")
  )

;;;; --- Configurable variables -----

(defparameter *blog-title* "My Weblog"
  "What's this blog's title")

(defparameter *blog-description* "Yet another Blosxom-like weblog."
  "What's this blogs description (for outgoing RSS feed)?")

(defparameter *blog-language* "en"
  "What's this blog's primary language (for outgoing RSS feed)?")

(defparameter *datadir* "/Library/WebServer/Documents/blosxom"
"Where are this blog's entries kept?")

(defparameter *url* Nil
  "What's my preferred base URL for this blog (leave blank for automatic)?")


(defparameter *depth* 0
  "Should I stick only to the datadir for items or travel down the
directory hierarchy looking for items?  If so, to what depth?
0 = infinite depth (aka grab everything), 1 = datadir only, n = n levels down")

(defparameter num-entries 40
  "How many entries should I show on the home page?")

(defparameter *file-extension* "txt"
  "What file extension signifies a blosxom entry?")

(defparameter *default-flavour* "html"
  "What is the default flavour?")

(defparameter *show-future-entries* Nil
"Should I show entries from the future (i.e. dated after now)?")


;;; --- Plugins (Optional) -----

(defparameter *plugin-dir* ""
  "Where are my plugins kept?")

(defparameter *plugin-state-dir* (format Nil "~A/state" *plugin-dir*)
  "Where should my modules keep their state information?")


;;; --- Static Rendering -----

(defparameter *static-dir* "/Library/WebServer/Documents/blog"
"Where are this blog's static files to be created?")

(defparameter *static-password* ""
  "What's my administrative password (you must set this for static rendering)?")

(defparameter *static-flavours* (list "html" "rss")
  "What flavours should I generate statically?")

(defparameter *static-entries* T
  "Should I statically generate individual entries?")



;;;; --------------------------------

(defconstant +version+ "1.0")

(defconstant +months+
  (list Nil "Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))

(defun month2num (month-str)
  (format Nil "~2,0D" (position month-str +months+ :test equalp)))

(defun num2month (num)
  (elt +months+ num))

;; Use the stated preferred URL or figure it out automatically
;; Drop ending any / from dir settings
;; Fix depth to take into account datadir's path

(defparameter *path-info* nil
  "Global variable to be used in head/foot.{flavour} templates")

(defun gatewayp ())
(defun static-auth-p ())

(defparameter *static-or-dynamic* (if (and (not gatewayp) (static-auth-p))
				      'static
				      'dynamic))

;; Path Info Magic
;; Take a gander at HTTP's PATH_INFO for optional blog name, archive yr/mo/day

(defparameter *flavour* nil
  "Flavour specified by ?flav={flav} or index.{flav}")

;; Strip spurious slashes

;; Date fiddling

;; Define standard template subroutine, plugin-overridable at Plugins: Template
(defun template (path chunk flavour)
  )

;;; Bring in the templates


;;; Plugins: Start
(when *plugin-dir*
  (let ((plugins (directory
		  (merge-pathnames
		   (make-pathname :name :wild)
		   (pathname *plugin-dir*)))))
    (loop for file in plugins do (load file))))

(directory
   (make-pathname :name chunk
		  :type flavour
		  :directory (list :absolute *data-dir* path)))

;;; Generate
(defun generate (staticp, currentdir, date, flavor, content-type)
  
  )
