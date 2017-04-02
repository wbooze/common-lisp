;;; org-davep-cldict --- RFC2229 client for Common Lisp and CLIM.
;;
;; cldict.lisp --- Main code for org-davep-cldict.
;; Copyright 2004 by Dave Pearson <davep@davep.org>
;; $Revision: 1.10 $
;;
;; This software is Copyright (C) Dave Pearson <davep@davep.org> 2004
;;
;; Dave Pearson grants you the rights to distribute and use this software as
;; governed by the terms of the Lisp Lesser GNU Public License
;; <URL:http://opensource.franz.com/preamble.html>, known as the LLGPL.

;;; Commentary:
;;
;; org-davep-cldict is a command line based RFC 2229 client for Common Lisp
;; and CLIM. See <URL:http://www.dict.org/> for more details about
;; org-davep-dictd.
;;
;; You can always find the latest version of this code at:
;;
;;   <URL:http://www.davep.org/lisp/#org-davep-cldict>
;;
;; This code uses <URL:http://www.davep.org/lisp/#org-dave-dict>.

;;; Code:

(in-package :org.davep.cldict)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utility functions and macros.

(defun print-interactor-prompt (stream frame)
  "Print the prompt for the interactor pane."
  (with-drawing-options (stream :ink +royalblue+)
    (format stream "~&~%dict~A: "
            (if (dict:connectedp (dict frame))
                (format nil "[~A:~A]" (dict:host (dict frame)) (dict:port (dict frame)))
              ""))
    (finish-output stream)))

(defun output-list (data &key (formatter (lambda (item) (format t "~&~A~%" item))))
  "Output DATA.

Optionally format the output with the function given to the FORMATTER keyword."
  (mapc formatter (dict:data data)))
  
(defun output-info-list (data &key name-presentation description-presentation)
  "Output DATA as a name/description list.

If the NAME-PRESENTATION keyword is provided then that presentation type
will be used when displaying the name portion of the output.

If the DESCRIPTION-PRESENTATION keyword is provided then that presentation
type will be used when displaying the description portion of the output."
  (output-list data
               :formatter (lambda (info)
                            (format t "~&")
                            (if name-presentation
                                (with-output-as-presentation (t (dict:name info) name-presentation)
                                  (format t "~A" (dict:name info)))
                              (format t "~A" (dict:name info)))
                            (format t "~10T  - ")
                            (if description-presentation
                                (with-output-as-presentation (t (dict:description info) description-presentation)
                                  (format t "~A" (dict:description info)))
                              (format t "~A" (dict:description info)))
                            (format t "~%"))))

(defmacro with-error-trap (&body body)
  "Trap any errors and report them."
  `(handler-case
    (progn ,@body)
    (error (e) (with-drawing-options (t :ink +red+) (format t "~&Error: ~A~%" e)))))
           
(defmacro with-reconnect ((client) &body body)
  "Force a reconnect to CLIENT after BODY has been evaluated."
  `(prog1
       (progn
         ,@body)
     (when (dict:connectedp ,client)
       (dict:disconnect ,client)
       (with-error-trap
        (dict:connect ,client)))))

(defmacro with-feedback-face (&body body)
  "Sets the face for printing feedback."
  `(with-drawing-options (t :text-style (make-text-style :serif :italic nil))
     ,@body))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Menu bar.

(make-command-table 'file-command-table
                    :errorp nil
                    :menu '(("Connect"    :command com-connect)
                            ("Disconnect" :command com-disconnect)
                            ("Quit"       :command com-quit)))

(make-command-table 'show-command-table
                    :errorp nil
                    :menu '(("Host"                :command com-show-host)
                            ("Port"                :command com-show-port)
                            ("Server Information"  :command com-show-server-information)
                            ("Server Help"         :command com-show-server-help)
                            ("Server Capabilities" :command com-show-server-capabilities)
                            ("Databases"           :command com-show-databases)
                            ("Match Strategies"    :command com-show-strategies)))

(make-command-table 'menubar-command-table
                    :errorp nil
                    :menu '(("File" :menu file-command-table)
                            ("Show" :menu show-command-table)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The application's frame.

(define-application-frame cldict ()
  ((dict
    :initform (dict:make-dict-client)
    :accessor dict))
  (:menu-bar menubar-command-table)
  (:pointer-documentation t)
  (:panes (interactor :interactor :scroll-bars t :width 600))
  (:top-level (default-frame-top-level :prompt 'print-interactor-prompt))
  (:layouts (default (vertically () interactor))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Presentation types.

(define-presentation-type dict-host            () :inherit-from 'string)
(define-presentation-type dict-port            () :inherit-from 'integer)
(define-presentation-type dict-database-info   () :inherit-from 'string)
(define-presentation-type dict-word-definition () :inherit-from 'string)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Commands.

(define-cldict-command (com-clear-history :name "Clear History") ()
  (window-clear *standard-output*))

(define-cldict-command (com-set-host :name "Set Host")
  ((hostname 'dict-host :gesture :select :prompt "hostname"))
  (with-reconnect ((dict *application-frame*))
    (setf (dict:host (dict *application-frame*)) hostname)))

(define-cldict-command (com-set-port :name "Set Port")
  ((port 'dict-port :gesture :select :prompt "port"))
  (with-reconnect ((dict *application-frame*))
    (setf (dict:port (dict *application-frame*)) port)))

(define-cldict-command (com-show-host :name "Show Host") ()
  (with-feedback-face
   (format t "~&Current host is ")
   (with-output-as-presentation (t (dict:host (dict *application-frame*)) 'dict-host)
     (format t "~A~%" (dict:host (dict *application-frame*))))))

(define-cldict-command (com-show-port :name "Show Port") ()
  (with-feedback-face
   (format t "~&Current port is ")
   (with-output-as-presentation (t (dict:port (dict *application-frame*)) 'dict-port)
     (format t "~A~%" (dict:port (dict *application-frame*))))))

(define-cldict-command (com-connect :name "Connect") ()
  (let ((dict (dict *application-frame*)))
    (unless (dict:connectedp dict)
      (with-error-trap
       (dict:connect dict)))
    (when (dict:connectedp dict)
      (with-feedback-face
       (format t "~&Connected~%~A~%Capabilities:~{ ~A~}~%Message-ID: ~A~%"
               (dict:server-details dict)
               (dict:capabilities dict)
               (dict:message-id dict))))))
    
(define-cldict-command (com-disconnect :name "Disconnect") ()
  (let ((dict (dict *application-frame*)))
    (when (dict:connectedp dict)
      (dict:disconnect dict))))

(define-cldict-command (com-define :name "Define")
  ((word 'dict-word-definition :gesture :select :prompt "word")
   &key
   (database 'string :default dict:+all-db+ :prompt "database"))
  (with-error-trap
   (let ((defs (dict:define (dict *application-frame*) word :database database)))
     (if defs
         (loop for definition in defs
               do (progn
                    ;; Show the title of the database that the definition came from.
                    (with-drawing-options (t :text-style (make-text-style :fixed :bold nil))
                      (with-output-as-presentation (t (dict:database definition) 'dict-database-info)
                        (format t "~&~%~A - ~A~%~%" (dict:database definition) (dict:name definition))))
                    ;; Turn the list of lines in the definition into one big string.
                    (let ((text (make-array '(0) :element-type 'base-char :fill-pointer 0 :adjustable t)))
                      (with-output-to-string (s text)
                        (loop for line in (dict:definition definition) do (format s "~A~%" line)))
                      ;; Turn multiple highlights into single highligts.
                      (let ((text (regex-replace-all "{{+" (regex-replace-all "}}+" text "}") "{")))
                        ;; Output the definition while making linked words "clickable".
                        (loop for start              = 0 then (1+ end-of-highlight)
                              for start-of-highlight = (position #\{ text) then (position #\{ text :start (1+ start-of-highlight))
                              for end-of-highlight   = (position #\} text) then (position #\} text :start (1+ end-of-highlight))
                              while start-of-highlight do
                              (progn
                                (format t "~A" (subseq text start start-of-highlight))
                                (with-output-as-presentation (t (regex-replace-all "\\n\\s*" (subseq text (1+ start-of-highlight) end-of-highlight) " ") 'dict-word-definition)
                                  (with-drawing-options (t :ink +blue+)
                                    (format t "~A" (subseq text (1+ start-of-highlight) end-of-highlight)))))
                              finally (format t (subseq text start)))))))
     (with-feedback-face
      (format t "~&No definitions found for \"~A\"~%" word))))))

(define-cldict-command (com-match :name "Match")
  ((word 'string :prompt "word")
   &key
   (database 'string :default dict:+all-db+                 :prompt "database")
   (strategy 'string :default dict:+default-match-strategy+ :prompt "match strategy"))
  (with-error-trap
   (let ((matches (dict:match (dict *application-frame*) word :database database :strategy strategy)))
     (if (dict:data matches)
         (output-info-list matches
                           :name-presentation        'dict-database-info
                           :description-presentation 'dict-word-definition)
       (with-feedback-face
        (format t "~&No matches found~%"))))))

(define-cldict-command (com-show-database-information :name "Show Database Information")
  ((database 'dict-database-info :gesture :select :prompt "database"))
  (with-error-trap
   (let ((info (dict:info (dict *application-frame*) database)))
     (if (dict:data info)
         (output-list info)
       (if (= (dict:code info) 550)
           (with-feedback-face
            (format t "~&Invalid database \"~A\", use \"Show Databases\" for list.~%" database))
         (format t "~&~A~%" (dict:response info)))))))
  
(define-cldict-command (com-show-databases :name "Show Databases") ()
  (with-error-trap
   (output-info-list (dict:databases (dict *application-frame*)) :name-presentation 'dict-database-info)))

(define-cldict-command (com-show-strategies :name "Show Strategies") ()
  (with-error-trap
   (output-info-list (dict:strategies (dict *application-frame*)))))

(define-cldict-command (com-show-server-information :name "Show Server Information") ()
  (with-error-trap
   (output-list (dict:server-info (dict *application-frame*)))))

(define-cldict-command (com-show-server-help :name "Show Server Help") ()
  (with-error-trap
   (output-list (dict:server-help (dict *application-frame*)))))

(define-cldict-command (com-show-server-capabilities :name "Show Server Capabilities") ()
  (with-error-trap
   (with-feedback-face
    (format t "~&Server capabilities: ~{~(~A~) ~}~%" (mapcar #'symbol-name (dict:capabilities (dict *application-frame*)))))))

(define-cldict-command (com-quit :name "Quit") ()
  (frame-exit *application-frame*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Entry point.

(defun cldict ()
  "CLIM based dict client."
  (run-frame-top-level (make-application-frame 'cldict)))

;;; cldict.lisp ends here.
