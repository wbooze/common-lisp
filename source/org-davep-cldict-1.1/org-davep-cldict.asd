;;; org-davep-cldict --- RFC2229 client for Common Lisp and CLIM.
;;
;; org-davep-cldict.asd --- asdf package defintion file.
;; Copyright 2004 by Dave Pearson <davep@davep.org>
;; $Revision: 1.2 $
;;
;; This software is Copyright (C) Dave Pearson <davep@davep.org> 2004
;;
;; Dave Pearson grants you the rights to distribute and use this software as
;; governed by the terms of the Lisp Lesser GNU Public License
;; <URL:http://opensource.franz.com/preamble.html>, known as the LLGPL.

;;; Commentary:
;;
;; cldict is a command line based RFC 2229 client for Common Lisp and CLIM.
;; See <URL:http://www.dict.org/> for more details about dictd.
;;
;; You can always find the latest version of this code at:
;;
;;   <URL:http://www.davep.org/lisp/#org-davep-cldict>
;;
;; This code uses <URL:http://www.davep.org/lisp/#org-dave-dict>.

(defpackage #:org-davep-cldict-system
  (:use #:common-lisp #:asdf))

(in-package :org-davep-cldict-system)

(defsystem org-davep-cldict
  :name        "org-davep-cldict"
  :author      "Dave Pearson <davep@davep.org>"
  :maintainer  "Dave Pearson <davep@davep.org>"
  :licence     "LLGPL"
  :version     "1.1"
  :description "RFC2229 client for Common Lisp and CLIM."
  :long-description
  "org-davep-cldict provides a command line based RFC 2229 client for Common
Lisp and CLIM. See <URL:http://www.dict.org/> for more details about dict
servers and clients.

See <URL:http://www.davep.org/lisp/#org-davep-cldict> for the latest version
of this package."
  :depends-on  (:org-davep-dict :cl-ppcre)
  :components  ((:file "packages")
                (:file "cldict" :depends-on ("packages"))))

;;; org-davep-cldict.asd ends here.
