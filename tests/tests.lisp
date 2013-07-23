;;; -*- mode: lisp; syntax: common-lisp; coding: utf-8-unix; package: cl-org-mode-parser-tests; -*-

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

(in-package #:cl-org-mode-parser-tests)

(in-suite cl-org-mode-parser)

(def-test empty-string ()
  (is (document-equal-p (make-empty-document) (parse ""))))

(def-test empty-lines ()
  (is (document-equal-p (make-empty-document) (parse "


"))))

(def-test empty-lines.preserve-whitespace ()
  (let ((document (make-instance
                   'document
                   :nodes (list "" "" ""))))
    (is (document-equal-p document (parse "


" :preserve-whitespace-p T)))))

(def-test comments ()
  (is (document-equal-p (make-empty-document) (parse "

#

  #  asdf

"))))

(def-test comments.preserve-whitespace ()
  (let ((document (make-instance
                   'document
                   :nodes (list "" "   " "  " "    "))))
    (is (document-equal-p document (parse "
   
#
  
  #  asdf
    
" :preserve-whitespace-p T)))))

(def-test single-line ()
  (let ((document (make-instance 'document :nodes (list "bar" "" "foo"))))
    (is (document-equal-p document (parse "

#   
bar
   #   asdf

foo

")))))

(def-test single-header-line ()
  (let ((document (make-instance
                   'document
                   :nodes (list (make-instance
                                 'headline
                                 :text "Bar!"
                                 :nodes (list "foo"))))))
    (is (document-equal-p document (parse "

#   
* Bar!
   #   asdf

foo

")))))

(def-test regular-text ()
  (let ((document (make-instance 'document :nodes (list "foo"))))
    (is (document-equal-p document (parse "foo")))))

(def-test headlines ()
  (let ((document (make-instance
                   'document
                   :nodes (list (make-instance
                                 'headline
                                 :text "First level"
                                 :nodes (list "Text." "" "And some text."
                                              (make-instance
                                               'headline
                                               :text "Second level"
                                               :nodes (list "More text."))))
                                 (make-instance
                                  'headline
                                  :text "Another first level"
                                  :nodes (list (make-instance
                                                'headline
                                                :text "Another second level")))))))
    (is (document-equal-p document (parse (test-file "headlines"))))))

(def-test headlines.preserve-whitespace ()
  (let ((document (make-instance
                   'document
                   :nodes (list (make-instance
                                 'headline
                                 :text "First level    "
                                 :nodes (list "  Text." "  " "  And some text.  " "  "
                                              (make-instance
                                               'headline
                                               :text "Second level"
                                               :nodes (list "More text."))))
                                 (make-instance
                                  'headline
                                  :text "Another first level "
                                  :nodes (list (make-instance
                                                'headline
                                                :text "Another second level ")))))))
    (is (document-equal-p document (parse (test-file "headlines") :preserve-whitespace-p T)))))

(def-test invalid-headlines ()
  (signals simple-error (parse (test-file "invalid-headlines.org"))))

(def-test tags ()
  (let ((document (make-instance
                   'document
                   :nodes (list (make-instance
                                 'headline
                                 :text "First level"
                                 :tags '(:and :some :tags)
                                 :nodes (list (make-instance
                                               'headline
                                               :text "Second level :some:more:tags: and something afterwards")))))))
    (is (document-equal-p document (parse (test-file "tags"))))))

(def-test tags.preserve-whitespace ()
  (let ((document (make-instance
                   'document
                   :nodes (list (make-instance
                                 'headline
                                 :text "First level "
                                 :tags '(:and :some :tags)
                                 :nodes (list (make-instance
                                               'headline
                                               :text "Second level :some:more:tags: and something afterwards")))))))
    (is (document-equal-p document (parse (test-file "tags") :preserve-whitespace-p T)))))
