;;;; Tests
;;;; =====

;;;; Copyright (c) 2025 Susam Pal
;;;;
;;;; You can use, copy, modify, merge, publish, distribute, sublicence
;;;; and/or sell copies of it, under the terms of the MIT Licence.
;;;; See LICENSE.md for complete details.
;;;;
;;;; This software is provided "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;;; express or implied.  See LICENSE.md for complete details.

(require "uiop")


;;; Test Definitions
;;; ----------------

(defparameter *pass* 0)
(defparameter *fail* 0)
(defvar *quit* nil)

(defun remove-directory (path)
  "Remove the specified directory tree from the file system."
  (uiop:delete-directory-tree (pathname path) :validate t
                                              :if-does-not-exist :ignore))

(defmacro test-case (name &body body)
  "Execute a test case and print pass or fail status."
  `(progn
     (remove-directory #p"test-tmp/")
     (ensure-directories-exist #p"test-tmp/")
     (let ((test-name (string-downcase ',name)))
       (format t "~&~a: " test-name)
       (handler-case (progn ,@body)
         (:no-error (c)
           (declare (ignore c))
           (remove-directory #p"test-tmp/")
           (incf *pass*)
           (format t "pass~%")
           t)
         (error (c)
           (remove-directory #p"test-tmp/")
           (incf *fail*)
           (format t "FAIL~%")
           (format t "~&  ~a: error: ~a~%" test-name c))))))

(defmacro test-case! (name &body body)
  "Execute a test case and error out on failure.  Useful for debugging."
  `(progn
     (remove-directory #p"test-tmp/")
     (ensure-directories-exist #p"test-tmp/")
     (let ((test-name (string-downcase ',name)))
       (format t "~&~a: " test-name)
       ,@body
       (incf *pass*)
       (format t "pass!~%")
       (remove-directory #p"test-tmp/"))))

(defun test-done ()
  "Print test statistics."
  (format t "~&~%PASS: ~a~%" *pass*)
  (when (plusp *fail*)
    (format t "~&FAIL: ~a~%" *fail*))
  (when *quit*
    (format t "~&~%DONE~%~%")
    (uiop:quit (if (zerop *fail*) 0 1)))
  (zerop *fail*))


;;; Begin Test Cases
;;; ----------------

(defvar *main-mode* nil)
(load "nq.lisp")


;;; Test Cases for Reusable Definitions
;;; -----------------------------------

(test-case string-replace-empty
  (assert (string= (string-replace "" "" "") ""))
  (assert (string= (string-replace "" "x" "") "x"))
  (assert (string= (string-replace "" "bar" "") "bar"))
  (assert (string= (string-replace "" "-" "foo") "-f-o-o-"))
  (assert (string= (string-replace "" "-~" "foo") "-~f-~o-~o-~")))

(test-case string-replace-single
  (assert (string= (string-replace "foo" "foo" "foo") "foo"))
  (assert (string= (string-replace "foo" "bar" "") ""))
  (assert (string= (string-replace "foo" "bar" "foo") "bar"))
  (assert (string= (string-replace "foo" "bar" "foofoo") "barbar"))
  (assert (string= (string-replace "foo" "bar" "foo foo") "bar bar")))

(test-case string-replace-multiple
  (assert (string= (string-replace "foo" "x" "foo:foo") "x:x"))
  (assert (string= (string-replace "foo" "x" "foo:foo:") "x:x:")))

(test-case string-split
  (assert (not (string-split "" ":")))
  (assert (not (string-split "" ": ")))
  (assert (equal (string-split "foo" ":") '("foo")))
  (assert (equal (string-split "foo:" ":") '("foo")))
  (assert (equal (string-split "foo:bar" ":") '("foo" "bar")))
  (assert (equal (string-split "foo:bar" ":") '("foo" "bar")))
  (assert (equal (string-split "foo" ": ") '("foo")))
  (assert (equal (string-split "foo:" ": ") '("foo:")))
  (assert (equal (string-split "foo: " ": ") '("foo")))
  (assert (equal (string-split "foo:bar" ": ") '("foo:bar")))
  (assert (equal (string-split "foo: bar" ": ") '("foo" "bar")))
  (assert (equal (string-split "foo: bar: baz" ": ") '("foo" "bar" "baz")))
  (assert (equal (string-split "foo: bar: baz: " ": ") '("foo" "bar" "baz")))
  (assert (equal (string-split "foo: bar: : baz: " ": ") '("foo" "bar" "" "baz")))
  (assert (equal (string-split "foo::bar:::baz:" ":") '("foo" ""  "bar" "" "" "baz"))))

(test-case string-split-ignore-empty-splits
  (assert (not (string-split "" ":" :ignore-empty t)))
  (assert (not (string-split "" ": " :ignore-empty t)))
  (assert (equal (string-split "foo" ":" :ignore-empty t) '("foo")))
  (assert (equal (string-split "foo:" ":" :ignore-empty t) '("foo")))
  (assert (equal (string-split "foo:bar" ":" :ignore-empty t) '("foo" "bar")))
  (assert (equal (string-split "foo:bar" ":" :ignore-empty t) '("foo" "bar")))
  (assert (equal (string-split "foo" ": " :ignore-empty t) '("foo")))
  (assert (equal (string-split "foo:" ": " :ignore-empty t) '("foo:")))
  (assert (equal (string-split "foo: " ": " :ignore-empty t) '("foo")))
  (assert (equal (string-split "foo:bar" ": " :ignore-empty t) '("foo:bar")))
  (assert (equal (string-split "foo: bar" ": " :ignore-empty t) '("foo" "bar")))
  (assert (equal (string-split "foo: bar: baz" ": " :ignore-empty t) '("foo" "bar" "baz")))
  (assert (equal (string-split "foo: bar: baz: " ": " :ignore-empty t) '("foo" "bar" "baz")))
  (assert (equal (string-split "foo: bar: : baz: " ": " :ignore-empty t) '("foo" "bar" "baz")))
  (assert (equal (string-split "foo::bar:::baz:" ":" :ignore-empty t) '("foo" "bar" "baz"))))


;;; Test Cases for JS Generator
;;; ---------------------------

(test-case indent
  (assert (string= (indent 0) ""))
  (assert (string= (indent 1) "  "))
  (assert (string= (indent 2) "    ")))

(test-case to-js-atoms
  (assert (string= (to-js nil) "null"))
  (assert (string= (to-js 10.2) "10.2"))
  (assert (string= (to-js 5/2) "5/2"))
  (assert (string= (to-js "foo") "'foo'")))

(test-case to-js-object
  (assert (string= (to-js '(("a" . "apple") (b . "ball")))
                   "{
  a: 'apple',
  b: 'ball'
}")))

(test-case to-js-object-indent
  (assert (string= (to-js '(("a" . "apple") (b . "ball")) 1)
                   "{
    a: 'apple',
    b: 'ball'
  }")))

(test-case to-js-array
  (assert (string= (to-js (list "apple" 10 nil 1/3))
                   "[
  'apple',
  10,
  null,
  1/3
]")))

(test-case to-js-array-indent
  (assert (string= (to-js (list "apple" 10 nil 1/3) 2)
                   "[
      'apple',
      10,
      null,
      1/3
    ]")))

(test-case to-js-nested-arrays
  (assert (string= (to-js '("apple" 10
                            ("ball" 20 30)
                            ("cat" ("dog" 40 50) 60 70)))
                   "[
  'apple',
  10,
  [
    'ball',
    20,
    30
  ],
  [
    'cat',
    [
      'dog',
      40,
      50
    ],
    60,
    70
  ]
]")))

(test-case to-js-nested-arrays-indent
  (assert (string= (to-js '("apple" 10
                            ("ball" 20 30)
                            ("cat" ("dog" 40 50) 60 70)) 1)
                   "[
    'apple',
    10,
    [
      'ball',
      20,
      30
    ],
    [
      'cat',
      [
        'dog',
        40,
        50
      ],
      60,
      70
    ]
  ]")))

(test-case to-js-array-of-arrays
  (assert (string= (to-js '(("a" "apple") ("b" "ball")))
                   "{
  a: [
    'apple'
  ],
  b: [
    'ball'
  ]
}")))

(test-case to-js-array-of-objects
  (assert (string= (to-js '((("a" . "apple") (b . "ball"))
                            (("c" . 10) (d . 20))
                            (("e" . 50))))
                   "[
  {
    a: 'apple',
    b: 'ball'
  },
  {
    c: 10,
    d: 20
  },
  {
    e: 50
  }
]")))

(test-case to-js-array-of-objects-indent
  (assert (string= (to-js '((("a" . "apple") (b . "ball"))
                            (("c" . 10) (d . 20))
                            (("e" . 50))) 2)
                   "[
      {
        a: 'apple',
        b: 'ball'
      },
      {
        c: 10,
        d: 20
      },
      {
        e: 50
      }
    ]")))

(test-case to-js-list-nested
  (assert (string= (to-js '(("name" . "jdoe")
                            ("age" . 30)
                            ("address" . (("house" . 1)
                                          ("street" . "high street")))
                            ("telephone" . ("07000400400" "07000500500"))))
                   "{
  name: 'jdoe',
  age: 30,
  address: {
    house: 1,
    street: 'high street'
  },
  telephone: [
    '07000400400',
    '07000500500'
  ]
}")))

;;; End Test Cases
;;; --------------

(test-done)
