;;; -*- mode: lisp; syntax: common-lisp; coding: utf-8-unix; package: cl-org-mode-parser; -*-

;; Copyright (c) 2013, Olof-Joachim Frahm
;; All rights reserved.

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:

;; 1. Redistributions of source code must retain the above copyright
;; notice, this list of conditions and the following disclaimer.

;; 2. Redistributions in binary form must reproduce the above copyright
;; notice, this list of conditions and the following disclaimer in the
;; documentation and/or other materials provided with the distribution.

;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;; A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;; OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
;; LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
;; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(in-package #:cl-org-mode-parser)

(defclass document ()
  ((options
    :initarg :options
    :initform NIL)
   (nodes
    :initarg :nodes
    :initform NIL)))

(defmethod print-object ((document document) stream)
  (print-unreadable-object (document stream :type T :identity T)
    (format stream "with ~D node~:*~p" (length (slot-value document 'nodes)))))

(defclass headline ()
  ((text
    :initarg :text
    :initform "")
   (tags
    :initarg :tags
    :initform NIL)
   (nodes
    :initarg :nodes
    :initform NIL)))

(defmethod print-object ((headline headline) stream)
  (print-unreadable-object (headline stream :type T :identity T)
    (format stream "~S~@[~* with ~D node~:*~p~]"
            (slot-value headline 'text)
            (slot-value headline 'nodes)
            (length (slot-value headline 'nodes)))))

(defun tags-equal-p (tags1 tags2)
  (and (length= tags1 tags2)
       (every #'equal tags1 tags2)))

(defun headline-equal-p (headline1 headline2)
  (or (eq headline1 headline2)
      (and (string= (slot-value headline1 'text)
                    (slot-value headline2 'text))
           (nodes-equal-p
            (slot-value headline1 'nodes)
            (slot-value headline2 'nodes))
           (tags-equal-p
            (slot-value headline1 'tags)
            (slot-value headline2 'tags)))))

(defun node-equal-p (node1 node2)
  (or (equal node1 node2)
      (and (typep node1 'headline)
           (typep node2 'headline)
           (headline-equal-p node1 node2))))

(defun nodes-equal-p (nodes1 nodes2)
  (and (length= nodes1 nodes2)
       (every #'node-equal-p nodes1 nodes2)))

(defun document-equal-p (document1 document2)
  (or (eq document1 document2)
      (nodes-equal-p
       (slot-value document1 'nodes)
       (slot-value document2 'nodes))))

(defun make-empty-document (&key (class 'document))
  (make-instance class))

(defgeneric start-document (handler)
  (:documentation "Is called at the start of the document, typically to
initialize some context in the handler (e.g. to reuse it for different
parses."))

(defgeneric end-document (handler)
  (:documentation "Is called at the end of the document.  The first result
is returned from the caller."))

(defgeneric characters (handler string start end)
  (:documentation "Is called with single lines of input.  START and END are
like arguments for SUBSEQ to extract the range of valid input: if a line
is empty (or whitespace-stripped), (- END START) is zero."))

(defgeneric start-headline (handler string start end)
  (:documentation "Is called with the text of the headline.  See CHARACTERS
for the string semantics.  TAGS are handled separately, see the handler of
the same name.  Also see PARSE-HEADLINE."))

(defgeneric tags (handler string tags-start tags-end tags-list)
  (:documentation "Is called with the range where tags where detected and
possibly a LIST of parsed or interned tags, see PARSE-TAGS."))

(defgeneric end-headline (handler)
  (:documentation "Is called if a headline is finished."))

(defgeneric option (handler name raw-value)
  (:documentation "Is called when a header option occurs."))

(defclass empty-handler () ()
  (:documentation "Default handler implementation doing nothing.  Useful
if you're only interested in a subset of the handler events."))

(defmethod start-document ((empty-handler empty-handler)))
(defmethod end-document ((empty-handler empty-handler)))
(defmethod characters ((empty-handler empty-handler) string start end))
(defmethod option ((empty-handler empty-handler) name value))
(defmethod start-headline ((empty-handler empty-handler) string start end))
(defmethod tags ((empty-handler empty-handler) string tags-start tags-end tags-list))
(defmethod end-headline ((empty-handler empty-handler)))

(defclass document-builder ()
  ((stack
    :initform NIL)
   (save-tags-p
    :initarg :save-tags-p
    :initform T)))

(defun make-document-builder (&rest keys &key (save-tags-p T))
  (declare (ignore save-tags-p))
  (apply #'make-instance 'document-builder keys))

(defmethod start-document ((handler document-builder))
  (setf (slot-value handler 'stack)
        (list (make-instance 'document))))

(defmethod end-document ((handler document-builder))
  (pop (slot-value handler 'stack)))

(defmethod characters ((handler document-builder) string start end)
  (setf (slot-value (car (slot-value handler 'stack)) 'nodes)
        (append (slot-value (car (slot-value handler 'stack)) 'nodes)
                (list (subseq string start end)))))

(defmethod option ((handler document-builder) name value)
  (push (cons name value) (slot-value (lastcar (slot-value handler 'stack)) 'options)))

(defmethod start-headline ((handler document-builder) string start end)
  (push (make-instance 'headline :text (subseq string start end))
        (slot-value handler 'stack)))

(defmethod tags ((handler document-builder) string tags-start tags-end tags-list)
  (when (slot-value handler 'save-tags-p)
    (setf (slot-value (car (slot-value handler 'stack)) 'tags)
          (or tags-list (subseq string tags-start tags-end)))))

(defmethod end-headline ((handler document-builder))
  (let ((headline (pop (slot-value handler 'stack))))
    (setf (slot-value (car (slot-value handler 'stack)) 'nodes)
          (append (slot-value (car (slot-value handler 'stack)) 'nodes)
                  (list headline)))))

;; TODO: encoding etc.
(defun open-input (input)
  (etypecase input
    (string (make-string-input-stream input))
    (pathname (open input))
    (stream input)))

(defun case-convert (case-conversion string)
  (funcall (ecase case-conversion
             (:upcase #'string-upcase)
             (:downcase #'string-downcase)
             (:capitalize #'string-capitalize)
             ((NIL) #'identity))
           string))

(defun parse-tags (string &key
                            (start 0)
                            (end (length string))
                            (intern-into #.(find-package '#:keyword))
                            (case-conversion (and intern-into :upcase)))
  (let ((result (split-sequence #\: string :remove-empty-subseqs T :start start :end end)))
    (setf result (map-into result (curry #'case-convert case-conversion)
                           result))
    (if intern-into
        (let ((package (or (find-package intern-into)
                           (error "can't resolve ~A to an existing package" intern-into))))
          (map-into result (lambda (tag) (intern tag package)) result))
        result)))

(defparameter *option-scanner* (create-scanner "^#\\+([a-zA-Z0-9-_]+): (.*)$"))

(defvar *headline-scanner* (create-scanner "^(\\*+) *(.*?) *$"))

(defvar *headline-scanner-with-tags* (create-scanner "^(\\*+) *(.*?) *(:(?:[@\\w]+:)+)? *$"))

(defun parse-option (string &key
                              (start 0)
                              (end (length string))
                              (intern-into #.(find-package '#:keyword)))
  (multiple-value-bind (match pieces) (scan-to-strings *option-scanner* string :start start :end end)
    (when match
      (let ((name (string-upcase (aref pieces 0)))
            (value (aref pieces 1)))
        (when (find #\Space value :test #'char/=)
          (values
           (if intern-into
               (let ((package (or (find-package intern-into)
                                  (error "can't resolve ~A to an existing package" intern-into))))
                 (intern name package))
               name)
           value))))))

(defun parse-headline (string &key
                                (start 0)
                                (end (length string))
                                (detect-tags-p T)
                                (parse-tags-p detect-tags-p)
                                (intern-into #.(find-package '#:keyword))
                                (case-conversion (and intern-into :upcase)))
  "Returns the number of stars and the start and end of the header text.
If DETECT-TAGS-P is set, the start and end of the tags are returned.  If
additionally PARSE-TAGS-P is set, a LIST of STRINGS is returned as well
\(or SYMBOLS if INTERN-INTO is set to a designator for an existing
package)."
  (multiple-value-bind (match-start match-end group-starts group-ends)
      (scan (if detect-tags-p *headline-scanner-with-tags* *headline-scanner*) string :start start :end end)
    (declare (ignore match-start match-end))
    (when group-starts
      (let ((tags-start (and detect-tags-p (aref group-starts 2)))
            (tags-end (and detect-tags-p (aref group-ends 2))))
        (values
         (- (aref group-ends 0) (aref group-starts 0))
         (aref group-starts 1)
         (aref group-ends 1)
         tags-start
         tags-end
         (and tags-start
              parse-tags-p
              (parse-tags string :start tags-start
                                 :end tags-end
                                 :intern-into intern-into
                                 :case-conversion case-conversion)))))))

(defvar +whitespace-scanner+ (create-scanner "^\\s*(.*?)\\s*$"))
(defvar +emacs-mode-line-scanner+ (create-scanner "-\\*-.*?-\\*-"))

(defun string-trim-whitespace (string)
  (multiple-value-bind (match-start match-end groups-start groups-end)
      (scan +whitespace-scanner+ string)
    (declare (ignore match-start match-end))
    (when groups-start
      (values
       (aref groups-start 0)
       (aref groups-end 0)))))

(defun parse (input &key
                      (handler (make-document-builder))
                      preserve-whitespace-p
                      (detect-tags-p T)
                      (parse-tags-p T)
                      (intern-into #.(find-package '#:keyword))
                      (case-conversion (and intern-into :upcase))
                      (detect-emacs-mode-line-p T))
  "Parse INPUT as an org-mode file.

If PARSE-TAGS-P is set, tags after headlines are parsed and possibly
interned (after uppercase conversion) if INTERN-TAGS is set to a package
designator which resolves to an existing package.  By default tags are
parsed and interned into the KEYWORD package, because typically tags
consist of few very often used names which will probably be used by the
application somehow.

INPUT may be an open STREAM (which is automatically closed afterwards),
or something designating a STREAM, i.e. a PATHNAME, which will be OPENED
for reading, or a STRING, whose contents will be used via a
MAKE-STRING-INPUT-STREAM.

If PRESERVE-WHITESPACE-P is set, empty lines (but not comment lines) and
trimmed whitespaces before and after headlines (but not after parsed tags)
are included in the events, therefore making e.g. layout information
available to downstream consumers.  Changing this setting during an active
parse is currently not supported, but could be useful for e.g. parsing
tables."
  (start-document handler)
  (with-open-stream (input (open-input input))
    (let ((level 0))
      (iterate
        (with start-of-context = T)
        (with headerp = T)
        (with previous-blank-line)
        (for line-number from 1)
        (for line = (read-line input NIL))
        (while line)
        (for length = (length line))
        (cond
          ((zerop length)
           (if preserve-whitespace-p
               (characters handler "" 0 0)
               (unless start-of-context
                 (setf previous-blank-line ""))))
          (T
           (when (and headerp
                      (char= #\# (char line 0)))
             (multiple-value-bind (name raw-value) (parse-option line :intern-into intern-into)
               (when name
                 (option handler name raw-value)
                 (switch (name :test #'string-equal)
                   ('startup
                    (let ((startup-options (split-sequence #\Space raw-value :remove-empty-subseqs t)))
                      (when (find 'odd startup-options :test #'string-equal)
                        (setf oddp t))))))))
           (case (char line 0)
             (#\*
              (setf previous-blank-line NIL)
              (setf start-of-context T)
              (setf headerp NIL)
              (multiple-value-bind (stars text-start text-end tags-start tags-end tags-list)
                  (parse-headline line
                                  :end length
                                  :detect-tags-p detect-tags-p
                                  :parse-tags-p parse-tags-p
                                  :intern-into intern-into
                                  :case-conversion case-conversion)
                ;; TODO: allow to parse fragments of a document and ignore this
                (unless (<= stars (1+ level))
                  ;; TODO: how could this proceed orderly?
                  ;; generate empty headlines?
                  ;; TODO: proper exceptions with line number and column number possibly as well
                  (error "heading level ~D invalid for current level ~D" stars level))
                (when (<= stars level)
                  (dotimes (i (1+ (- level stars)))
                    (end-headline handler)))
                (setf level stars)
                (let ((empty-text (eql text-start text-end)))
                  ;; when preserving whitespace, all whitespaces after the
                  ;; tags are lost, because we couldn't regenerate the
                  ;; original even if we added them to the beginning of the
                  ;; line; we may warn the user though
                  (when (and (= text-start stars) (not empty-text))
                    ;; TODO: another continuation to treat as regular line (default?)
                    (cerror "still accept as headline" "no whitespace between header stars and text in line ~D" line-number))
                  (when (and tags-start (= text-end tags-start) (not empty-text))
                    ;; TODO: another continuation to treat as regular text (default?)
                    (cerror "still accept as tags" "no whitespace between text and tags in line ~D" line-number)))
                (cond
                  (preserve-whitespace-p
                   (start-headline handler line
                                   (min (1+ stars) text-start)
                                   (or (and tags-start (min (1+ text-end) tags-start))
                                       length)))
                  (T
                   (start-headline handler line text-start text-end)))
                (when tags-start
                  (when preserve-whitespace-p
                    (when (> (- tags-start text-end) 1)
                      (warn "some whitespace was lost between text and parsed tags in line ~D" line-number))
                    (unless (= length tags-end)
                      (warn "some whitespace was lost after parsed tags in line ~D" line-number)))
                  (tags handler line tags-start tags-end tags-list))))
             (T
              (multiple-value-bind (trimmed-start trimmed-end)
                  (string-trim-whitespace line)
                (let ((trimmed-empty-p (eql trimmed-start trimmed-end)))
                  (when (and trimmed-start
                             (not trimmed-empty-p)
                             (char= (char line trimmed-start) #\#))
                    (next-iteration))
                  ;; TODO: is it okay to check this here and not in headlines?
                  ;; emacs will check is regardless of the current line layout,
                  ;; but instead always checks the first two lines ...
                  (when (and detect-emacs-mode-line-p
                             (<= 1 line-number 2)
                             (scan +emacs-mode-line-scanner+ line))
                    ;; TODO: allow to treat this as regular text though
                    (cerror "treat as comment" "emacs mode line detected in line ~D" line-number)
                    (next-iteration))
                  (if preserve-whitespace-p
                      (characters handler line 0 length)
                      (if (and trimmed-start (not trimmed-empty-p))
                          (progn
                            (when (and previous-blank-line (not start-of-context))
                              (setf previous-blank-line NIL)
                              (characters handler "" 0 0))
                            (characters handler line trimmed-start trimmed-end)
                            (setf start-of-context NIL))
                          (unless start-of-context
                            (setf previous-blank-line T)))))))))))
      (dotimes (i level)
        (end-headline handler))))
  (end-document handler))

;; (defun unparse (output handler))
