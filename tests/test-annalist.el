;;; test-annalist.el --- Tests for annalist.el -*- lexical-binding: t -*-

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; Tests for annalist.el

;;; Code:
;; * Setup
;; (when (require 'undercover nil t)
;;   (undercover "*.el"
;;               (:exclude "test-*.el")
;;               (:report-type :codecov)
;;               (:send-report nil)))
(require 'buttercup)
(require 'annalist)
(require 'org-element)

;; * Helpers
(defun annalist-describe-expect (annalist type &optional result view)
  "Compare the result of running `annalist-describe' with an expected one.
Call `annalist-describe' with ANNALIST, TYPE, and VIEW and compare the output to
RESULT. If RESULT has a leading newline, remove it first. If RESULT is nil, just
call `annalist-describe' (this is useful when writing tests)."
  (declare (indent 2))
  (annalist-describe annalist type view)
  (when result
    (when (string= (substring result 0 1) "\n")
      (setq result (substring result 1)))
    (with-current-buffer (format "*%s %s*" annalist type)
      (expect (buffer-string) :to-equal result))))

(defvar annalist-test-tome-default-records
  '((559 "The Siege of Dejagore begins" "Southern Continent")
    (-1442 "Temple of Traveller's Repose is founded" "Southern Continent")
    (0 "Beginning of the Domination" "Northern Continent")
    (470 "Tenth return of the Great Comet" "Outer Space")
    (470
     "The Lady and the Ten Who Were Taken are unearthed"
     "Northern Continent"))
  "Default records used by `annalist-test-tome-setup'.")

(cl-defun annalist-test-tome-setup (&key
                                    (records annalist-test-tome-default-records)
                                    start-index
                                    primary-key
                                    record-update
                                    preprocess
                                    test
                                    event-test
                                    view)
  "Create the annalist-test tome type and populate it with RECORDS.
Clear the existing tome beforehand if it exists. START-INDEX, PRIMARY-KEY,
RECORD-UPDATE, PREPROCESS, TEST, and EVENT-TEST are passed to the type
definition call. If VIEW is non-nil, the specified settings will be used for the
default view."
  (plist-put annalist--tomes 'annalist-test nil)
  (setf (annalist--get-view-settings 'annalist-test 'default) nil)
  (annalist-define-tome 'annalist-test
    (list
     :primary-key (or primary-key 'event)
     :table-start-index (or start-index 0)
     :record-update record-update
     :preprocess preprocess
     :test test
     'year
     (if event-test
         (list 'event :test event-test)
       'event)
     'location))
  (dolist (record records)
    (annalist-record 'annalist 'annalist-test record))
  (when view
    (annalist-define-view 'annalist-test 'default
      view)))

(defun annalist-no-tabs ()
  "Disable indenting with tabs in the current buffer."
  (setq indent-tabs-mode nil))

;; * Annalist Helpers
;; ** List
;; TODO plistify-settings, plistify, listify

;; ** Formatting
(describe "annalist-verbatim"
  (it "should surround an item with ="
    (expect (annalist-verbatim "rumel")
            :to-equal "=rumel=")
    (expect (annalist-verbatim 'spear)
            :to-equal "=spear=")))

(describe "annalist-code"
  (it "should surround an item with ~"
    (expect (annalist-code "foo")
            :to-equal "~foo~")
    (expect (annalist-code 'bar)
            :to-equal "~bar~")))

(describe "annalist-capitalize"
  (it "should capitalize an item"
    (expect (annalist-capitalize "the lady")
            :to-equal "The Lady")
    (expect (annalist-capitalize 'croaker)
            :to-equal "Croaker")))

(describe "annalist-compose"
  (it "should be capable of composing formatting functions"
    (expect (funcall (annalist-compose #'annalist-verbatim
                                       #'annalist-capitalize)
                     "the annals")
            :to-equal "=The Annals=")))

;; ** Sorting
(describe "annalist-string-<"
  (it "should return whether the string representation of a is less than b"
    (expect (annalist-string-< "a" "b"))
    (expect (annalist-string-< 'a 'b))))

(describe "annalist-key-<"
  (it "should return whether the string representation of key a is less than b"
    (expect (annalist-key-< "a" "b"))
    (expect (annalist-key-< (kbd "C-c a") (kbd "C-c b")))))

;; ** Source Block Formatting
(describe "annalist-multiline-source-blocks"
  (before-all
    (add-hook 'emacs-lisp-mode-hook #'annalist-no-tabs))
  (after-all
    (remove-hook 'emacs-lisp-mode-hook #'annalist-no-tabs))
  (it "should format all emacs lisp source blocks with `lispy-multiline'"
    (expect
     (with-temp-buffer
       (insert "* Test heading
#+begin_src emacs-lisp
;; these are some lambdas
(lambda () (lambda () (lambda () (lambda () (lambda () (lambda () (lambda ())))))))
(lambda () (lambda () (lambda () (lambda () (lambda () (lambda () (lambda ())))))))
(lambda () (lambda () (lambda () (lambda () (lambda () (lambda () (lambda ())))))))
#+end_src

* Test Heading 2
#+begin_src python
;; don't touch this, it's not elisp (well...)
(lambda () (lambda () (lambda () (lambda () (lambda () (lambda () (lambda ())))))))
#+end_src

#+begin_src emacs-lisp
(annalist-define-view 'keybindings 'my-view '(:defaults (:format #'capitalize) (key :format #'annalist-verbatim) (definition :format nil)))
#+end_src
")
       (annalist-multiline-source-blocks)
       (buffer-string))
     :to-equal "* Test heading
#+begin_src emacs-lisp
;; these are some lambdas
(lambda ()
  (lambda ()
    (lambda ()
      (lambda ()
        (lambda ()
          (lambda () (lambda ())))))))
(lambda ()
  (lambda ()
    (lambda ()
      (lambda ()
        (lambda ()
          (lambda () (lambda ())))))))
(lambda ()
  (lambda ()
    (lambda ()
      (lambda ()
        (lambda ()
          (lambda () (lambda ())))))))
#+end_src

* Test Heading 2
#+begin_src python
;; don't touch this, it's not elisp (well...)
(lambda () (lambda () (lambda () (lambda () (lambda () (lambda () (lambda ())))))))
#+end_src

#+begin_src emacs-lisp
(annalist-define-view
    'keybindings
    'my-view
  '(:defaults (:format #'capitalize)
              (key
               :format #'annalist-verbatim)
              (definition :format nil)))
#+end_src
")))

;; * Definition Keywords
(describe "The :table-start-index keyword"
  (it "should determine which items are printed in tables"
    (annalist-test-tome-setup :start-index 0)
    (annalist-describe-expect 'annalist 'annalist-test
      "
|  Year | Event                                             | Location           |
|-------+---------------------------------------------------+--------------------|
|   559 | The Siege of Dejagore begins                      | Southern Continent |
| -1442 | Temple of Traveller's Repose is founded           | Southern Continent |
|     0 | Beginning of the Domination                       | Northern Continent |
|   470 | Tenth return of the Great Comet                   | Outer Space        |
|   470 | The Lady and the Ten Who Were Taken are unearthed | Northern Continent |
")

    (annalist-test-tome-setup :start-index 1)
    (annalist-describe-expect 'annalist 'annalist-test
      "
* 559
| Event                        | Location           |
|------------------------------+--------------------|
| The Siege of Dejagore begins | Southern Continent |

* -1442
| Event                                   | Location           |
|-----------------------------------------+--------------------|
| Temple of Traveller's Repose is founded | Southern Continent |

* 0
| Event                       | Location           |
|-----------------------------+--------------------|
| Beginning of the Domination | Northern Continent |

* 470
| Event                                             | Location           |
|---------------------------------------------------+--------------------|
| Tenth return of the Great Comet                   | Outer Space        |
| The Lady and the Ten Who Were Taken are unearthed | Northern Continent |
")

    (annalist-test-tome-setup :start-index 2)
    (annalist-describe-expect 'annalist 'annalist-test
      "
* 559
** The Siege of Dejagore begins
| Location           |
|--------------------|
| Southern Continent |

* -1442
** Temple of Traveller's Repose is founded
| Location           |
|--------------------|
| Southern Continent |

* 0
** Beginning of the Domination
| Location           |
|--------------------|
| Northern Continent |

* 470
** Tenth return of the Great Comet
| Location    |
|-------------|
| Outer Space |

** The Lady and the Ten Who Were Taken are unearthed
| Location           |
|--------------------|
| Northern Continent |
")))

(describe "The :primary-key keyword"
  (it "should be used to determine whether to replace an existing record"
    (annalist-test-tome-setup
     :records '((0 "Beginning of the Domination" "Northern Continent")
                ;; these are the same with default primary key
                (551 "Battle" "Northern Continent")
                (559 "Battle" "Southern Continent")))
    (annalist-describe-expect 'annalist 'annalist-test
      "
| Year | Event                       | Location           |
|------+-----------------------------+--------------------|
|    0 | Beginning of the Domination | Northern Continent |
|  559 | Battle                      | Southern Continent |
"))
  (it "should support a list in addition to a single item"
    (annalist-test-tome-setup
     :primary-key '(year event)
     :records '(
                ;; these are the same
                (0 "Beginning of the Domination" "Northern Continent")
                (0 "Beginning of the Domination" "NC")
                ;; these are now different
                (551 "Battle" "Northern Continent")
                (559 "Battle" "Southern Continent")))
    (annalist-describe-expect 'annalist 'annalist-test
      "
| Year | Event                       | Location           |
|------+-----------------------------+--------------------|
|    0 | Beginning of the Domination | NC                 |
|  551 | Battle                      | Northern Continent |
|  559 | Battle                      | Southern Continent |
")))

(defun annalist-test-record-update (old-record new-record _settings)
  "Example :record-update function.
It concatenates the locations from OLD-RECORD and NEW-RECORD."
  (when old-record
    (setf (nth 2 new-record)
          (concat (nth 2 old-record) (nth 2 new-record))))
  new-record)

(describe "The :record-update keyword"
  (it "should allow updating previously existing records with a function"
    (annalist-test-tome-setup
     :record-update #'annalist-test-record-update
     :records '((0 "foo" "bar")
                (0 "foo" "baz")
                (0 "foo" "qux")))
    (annalist-describe-expect 'annalist 'annalist-test
      "
| Year | Event | Location  |
|------+-------+-----------|
|    0 | foo   | barbazqux |
")))

(defun annalist-test-preprocess (record)
  "Example :preprocess function that alters RECORD's location."
  (setf (nth 2 record) "Tower at Charm")
  record)

(describe "The :preprocess keyword"
  (it "should allow changing a record before recording it"
    (annalist-test-tome-setup
     :preprocess #'annalist-test-preprocess
     :records '((0 "foo" "location")))
    (annalist-describe-expect 'annalist 'annalist-test
      "
| Year | Event | Location       |
|------+-------+----------------|
|    0 | foo   | Tower at Charm |
")))

;; * View Keywords
;; ** Top-level Keywords
(describe "The top-level :predicate keyword"
  (it "should determine whether to print a record"
    (annalist-test-tome-setup
     :view '(:predicate (lambda (record) (cl-minusp (nth 0 record)))))
    (annalist-describe-expect 'annalist 'annalist-test
      "
|  Year | Event                                   | Location           |
|-------+-----------------------------------------+--------------------|
| -1442 | Temple of Traveller's Repose is founded | Southern Continent |
")))

(describe "The top-level :sort keyword"
  (it "should sort tables"
    (annalist-test-tome-setup
     :view '(:sort (lambda (record-a record-b) (< (nth 0 record-a)
                                                  (nth 0 record-b)))))
    (annalist-describe-expect 'annalist 'annalist-test
      "
|  Year | Event                                             | Location           |
|-------+---------------------------------------------------+--------------------|
| -1442 | Temple of Traveller's Repose is founded           | Southern Continent |
|     0 | Beginning of the Domination                       | Northern Continent |
|   470 | Tenth return of the Great Comet                   | Outer Space        |
|   470 | The Lady and the Ten Who Were Taken are unearthed | Northern Continent |
|   559 | The Siege of Dejagore begins                      | Southern Continent |
")))

(describe "The top-level :hooks keyword"
  (it "should run functions after printing the headings/tables and aligning them"
    (annalist-test-tome-setup
     :view '(:hooks (list (lambda ()
                            (goto-char (point-max))
                            (insert "Water sleeps\n")))))
    (annalist-describe-expect 'annalist 'annalist-test
      "
|  Year | Event                                             | Location           |
|-------+---------------------------------------------------+--------------------|
|   559 | The Siege of Dejagore begins                      | Southern Continent |
| -1442 | Temple of Traveller's Repose is founded           | Southern Continent |
|     0 | Beginning of the Domination                       | Northern Continent |
|   470 | Tenth return of the Great Comet                   | Outer Space        |
|   470 | The Lady and the Ten Who Were Taken are unearthed | Northern Continent |
Water sleeps
"))
  (it "should also support a single item (including a lambda)"
    (annalist-test-tome-setup
     :view '(:hooks (lambda ()
                      (goto-char (point-max))
                      (insert "My brother unforgiven\n"))))
    (annalist-describe-expect 'annalist 'annalist-test
      "
|  Year | Event                                             | Location           |
|-------+---------------------------------------------------+--------------------|
|   559 | The Siege of Dejagore begins                      | Southern Continent |
| -1442 | Temple of Traveller's Repose is founded           | Southern Continent |
|     0 | Beginning of the Domination                       | Northern Continent |
|   470 | Tenth return of the Great Comet                   | Outer Space        |
|   470 | The Lady and the Ten Who Were Taken are unearthed | Northern Continent |
My brother unforgiven
")))

(describe "The top-level :test keyword"
  (it "should be used for comparing the primary key of records"
    (annalist-test-tome-setup
     :test #'eq
     :records '((544 "Battle" "Windy Country")
                (544 "Battle" "Charm")))
    ;; test incorrect, so there should be duplicates
    (annalist-describe-expect 'annalist 'annalist-test
      "
| Year | Event  | Location      |
|------+--------+---------------|
|  544 | Battle | Windy Country |
|  544 | Battle | Charm         |
")
    (annalist-test-tome-setup
     :records '((544 "Battle" "Windy Country")
                (544 "Battle" "Charm")))
    (annalist-describe-expect 'annalist 'annalist-test
      "
| Year | Event  | Location |
|------+--------+----------|
|  544 | Battle | Charm    |
")
    ;; list primary key with correct tests
    (annalist-test-tome-setup
     :primary-key '(event location)
     :records '((543 "Battle" "Windy Country")
                (544 "Battle" "Windy Country")))
    (annalist-describe-expect 'annalist 'annalist-test
      "
| Year | Event  | Location      |
|------+--------+---------------|
|  544 | Battle | Windy Country |
"))
  (it "should be used for comparing the primary key with a user-defined test"
    (define-hash-table-test 'annalist-test-eq
      #'eq
      #'sxhash-eq)
    (annalist-test-tome-setup
     :test 'annalist-test-eq
     :records '((544 "Battle" "Windy Country")
                (544 "Battle" "Charm")))
    ;; because wrong test used, should get duplicates
    (annalist-describe-expect 'annalist 'annalist-test
      "
| Year | Event  | Location      |
|------+--------+---------------|
|  544 | Battle | Windy Country |
|  544 | Battle | Charm         |
")
    (define-hash-table-test 'annalist-test-equal
      #'equal
      #'sxhash-equal)
    (annalist-test-tome-setup
     :test 'annalist-test-equal
     :records '((544 "Battle" "Windy Country")
                (544 "Battle" "Charm")))
    (annalist-describe-expect 'annalist 'annalist-test
      "
| Year | Event  | Location |
|------+--------+----------|
|  544 | Battle | Charm    |
")))

;; ** Item Keywords
(describe "The item :test keyword"
  (it "should be used for comparing heading items"
    (annalist-test-tome-setup
     :start-index 2
     :event-test 'eq
     :records '((544 "Battle" "Windy Country")
                (544 "Battle" "Charm")))
    ;; because wrong test used, should get duplicates
    (annalist-describe-expect 'annalist 'annalist-test
      "
* 544
** Battle
| Location      |
|---------------|
| Windy Country |

** Battle
| Location |
|----------|
| Charm    |
")
    (annalist-test-tome-setup
     :start-index 2
     :event-test 'equal
     :primary-key '(year event location)
     :records '((544 "Battle" "Windy Country")
                (544 "Battle" "Charm")))
    (annalist-describe-expect 'annalist 'annalist-test
      "
* 544
** Battle
| Location      |
|---------------|
| Windy Country |
| Charm         |
"))
  (it "should be used for comparing heading items with a user-defined test"
    (define-hash-table-test 'annalist-test-eq
      #'eq
      #'sxhash-eq)
    (annalist-test-tome-setup
     :start-index 2
     :event-test 'annalist-test-eq
     :records '((544 "Battle" "Windy Country")
                (544 "Battle" "Charm")))
    ;; because wrong test used, should get duplicates
    (annalist-describe-expect 'annalist 'annalist-test
      "
* 544
** Battle
| Location      |
|---------------|
| Windy Country |

** Battle
| Location |
|----------|
| Charm    |
")
    (define-hash-table-test 'annalist-test-equal
      #'equal
      #'sxhash-equal)
    (annalist-test-tome-setup
     :start-index 2
     :event-test 'annalist-test-equal
     :primary-key '(year event location)
     :records '((544 "Battle" "Windy Country")
                (544 "Battle" "Charm")))
    (annalist-describe-expect 'annalist 'annalist-test
      "
* 544
** Battle
| Location      |
|---------------|
| Windy Country |
| Charm         |
")))

(describe "The item :predicate keyword"
  (it "should determine whether a heading should be printed"
    (annalist-test-tome-setup
     :start-index 1
     :view '((year :predicate (lambda (year) (< year 0)))))
    (annalist-describe-expect 'annalist 'annalist-test
      "
* -1442
| Event                                   | Location           |
|-----------------------------------------+--------------------|
| Temple of Traveller's Repose is founded | Southern Continent |
"))
  (it "should fall back to a value from :defaults"
    (annalist-test-tome-setup
     :start-index 1
     :view '(:defaults (:predicate (lambda (year) (< year 0)))))
    (annalist-describe-expect 'annalist 'annalist-test
      "
* -1442
| Event                                   | Location           |
|-----------------------------------------+--------------------|
| Temple of Traveller's Repose is founded | Southern Continent |
"))
  (it "should have precedence over a value in :defaults"
    (annalist-test-tome-setup
     :start-index 1
     :view '(:defaults (:predicate (lambda (year) (>= year 0)))
             (year :predicate (lambda (year) (< year 0)))))
    (annalist-describe-expect 'annalist 'annalist-test
      "
* -1442
| Event                                   | Location           |
|-----------------------------------------+--------------------|
| Temple of Traveller's Repose is founded | Southern Continent |
")))

(describe "The item :prioritize keyword"
  (it "should allow printing certain headings before any others"
    (annalist-test-tome-setup
     :start-index 1
     :view '((year :prioritize (0 470))))
    (annalist-describe-expect 'annalist 'annalist-test
      "
* 0
| Event                       | Location           |
|-----------------------------+--------------------|
| Beginning of the Domination | Northern Continent |

* 470
| Event                                             | Location           |
|---------------------------------------------------+--------------------|
| Tenth return of the Great Comet                   | Outer Space        |
| The Lady and the Ten Who Were Taken are unearthed | Northern Continent |

* 559
| Event                        | Location           |
|------------------------------+--------------------|
| The Siege of Dejagore begins | Southern Continent |

* -1442
| Event                                   | Location           |
|-----------------------------------------+--------------------|
| Temple of Traveller's Repose is founded | Southern Continent |
"))
  (it "should fall back to a value from :defaults"
    (annalist-test-tome-setup
     :start-index 1
     :view '(:defaults (:prioritize (0 470))))
    (annalist-describe-expect 'annalist 'annalist-test
      "
* 0
| Event                       | Location           |
|-----------------------------+--------------------|
| Beginning of the Domination | Northern Continent |

* 470
| Event                                             | Location           |
|---------------------------------------------------+--------------------|
| Tenth return of the Great Comet                   | Outer Space        |
| The Lady and the Ten Who Were Taken are unearthed | Northern Continent |

* 559
| Event                        | Location           |
|------------------------------+--------------------|
| The Siege of Dejagore begins | Southern Continent |

* -1442
| Event                                   | Location           |
|-----------------------------------------+--------------------|
| Temple of Traveller's Repose is founded | Southern Continent |
"))
  (it "should have precedence over a value in :defaults"
    (annalist-test-tome-setup
     :start-index 1
     :view '(:defaults (:prioritize (559))
             (year :prioritize (0 470))))
    (annalist-describe-expect 'annalist 'annalist-test
      "
* 0
| Event                       | Location           |
|-----------------------------+--------------------|
| Beginning of the Domination | Northern Continent |

* 470
| Event                                             | Location           |
|---------------------------------------------------+--------------------|
| Tenth return of the Great Comet                   | Outer Space        |
| The Lady and the Ten Who Were Taken are unearthed | Northern Continent |

* 559
| Event                        | Location           |
|------------------------------+--------------------|
| The Siege of Dejagore begins | Southern Continent |

* -1442
| Event                                   | Location           |
|-----------------------------------------+--------------------|
| Temple of Traveller's Repose is founded | Southern Continent |
"))
  (it "should work in combination with the item :sort keyword"
    (annalist-test-tome-setup
     :start-index 1
     :view '((year :prioritize (0 470)
                   :sort <)))
    (annalist-describe-expect 'annalist 'annalist-test
      "
* 0
| Event                       | Location           |
|-----------------------------+--------------------|
| Beginning of the Domination | Northern Continent |

* 470
| Event                                             | Location           |
|---------------------------------------------------+--------------------|
| Tenth return of the Great Comet                   | Outer Space        |
| The Lady and the Ten Who Were Taken are unearthed | Northern Continent |

* -1442
| Event                                   | Location           |
|-----------------------------------------+--------------------|
| Temple of Traveller's Repose is founded | Southern Continent |

* 559
| Event                        | Location           |
|------------------------------+--------------------|
| The Siege of Dejagore begins | Southern Continent |
")))

(describe "The item :sort keyword"
  (it "should sort headings"
    (annalist-test-tome-setup
     :start-index 1
     :view '((year :sort <)))
    (annalist-describe-expect 'annalist 'annalist-test
      "
* -1442
| Event                                   | Location           |
|-----------------------------------------+--------------------|
| Temple of Traveller's Repose is founded | Southern Continent |

* 0
| Event                       | Location           |
|-----------------------------+--------------------|
| Beginning of the Domination | Northern Continent |

* 470
| Event                                             | Location           |
|---------------------------------------------------+--------------------|
| Tenth return of the Great Comet                   | Outer Space        |
| The Lady and the Ten Who Were Taken are unearthed | Northern Continent |

* 559
| Event                        | Location           |
|------------------------------+--------------------|
| The Siege of Dejagore begins | Southern Continent |
"))
  (it "should fall back to a value from :defaults"
    (annalist-test-tome-setup
     :start-index 1
     :view '(:defaults (:sort <)))
    (annalist-describe-expect 'annalist 'annalist-test
      "
* -1442
| Event                                   | Location           |
|-----------------------------------------+--------------------|
| Temple of Traveller's Repose is founded | Southern Continent |

* 0
| Event                       | Location           |
|-----------------------------+--------------------|
| Beginning of the Domination | Northern Continent |

* 470
| Event                                             | Location           |
|---------------------------------------------------+--------------------|
| Tenth return of the Great Comet                   | Outer Space        |
| The Lady and the Ten Who Were Taken are unearthed | Northern Continent |

* 559
| Event                        | Location           |
|------------------------------+--------------------|
| The Siege of Dejagore begins | Southern Continent |
"))
  (it "should have precedence over a value in :defaults"
    (annalist-test-tome-setup
     :start-index 1
     :view '(:defaults (:sort >)
             (year :sort <)))
    (annalist-describe-expect 'annalist 'annalist-test
      "
* -1442
| Event                                   | Location           |
|-----------------------------------------+--------------------|
| Temple of Traveller's Repose is founded | Southern Continent |

* 0
| Event                       | Location           |
|-----------------------------+--------------------|
| Beginning of the Domination | Northern Continent |

* 470
| Event                                             | Location           |
|---------------------------------------------------+--------------------|
| Tenth return of the Great Comet                   | Outer Space        |
| The Lady and the Ten Who Were Taken are unearthed | Northern Continent |

* 559
| Event                        | Location           |
|------------------------------+--------------------|
| The Siege of Dejagore begins | Southern Continent |
")))

(describe "The item :title keyword"
  (it "should specify what to print for column titles"
    (annalist-test-tome-setup
     :view '((year :title "YEAR")))
    (annalist-describe-expect 'annalist 'annalist-test
      "
|  YEAR | Event                                             | Location           |
|-------+---------------------------------------------------+--------------------|
|   559 | The Siege of Dejagore begins                      | Southern Continent |
| -1442 | Temple of Traveller's Repose is founded           | Southern Continent |
|     0 | Beginning of the Domination                       | Northern Continent |
|   470 | Tenth return of the Great Comet                   | Outer Space        |
|   470 | The Lady and the Ten Who Were Taken are unearthed | Northern Continent |
"))
  (it "should not fall back to a value in :defaults"
    (annalist-test-tome-setup
     :view '(:defaults (:title "<placeholder>")))
    (annalist-describe-expect 'annalist 'annalist-test
      "
|  Year | Event                                             | Location           |
|-------+---------------------------------------------------+--------------------|
|   559 | The Siege of Dejagore begins                      | Southern Continent |
| -1442 | Temple of Traveller's Repose is founded           | Southern Continent |
|     0 | Beginning of the Domination                       | Northern Continent |
|   470 | Tenth return of the Great Comet                   | Outer Space        |
|   470 | The Lady and the Ten Who Were Taken are unearthed | Northern Continent |
")))

(describe "The item :format keyword"
  (it "should specify how to format an item in a heading"
    (annalist-test-tome-setup
     :start-index 1
     :view '((year :format annalist-verbatim)))
    (annalist-describe-expect 'annalist 'annalist-test
      "
* =559=
| Event                        | Location           |
|------------------------------+--------------------|
| The Siege of Dejagore begins | Southern Continent |

* =-1442=
| Event                                   | Location           |
|-----------------------------------------+--------------------|
| Temple of Traveller's Repose is founded | Southern Continent |

* =0=
| Event                       | Location           |
|-----------------------------+--------------------|
| Beginning of the Domination | Northern Continent |

* =470=
| Event                                             | Location           |
|---------------------------------------------------+--------------------|
| Tenth return of the Great Comet                   | Outer Space        |
| The Lady and the Ten Who Were Taken are unearthed | Northern Continent |
"))
  (it "should specify how to format an item in a table cell"
    (annalist-test-tome-setup
     :view '((year :format annalist-verbatim)))
    (annalist-describe-expect 'annalist 'annalist-test
      "
| Year    | Event                                             | Location           |
|---------+---------------------------------------------------+--------------------|
| =559=   | The Siege of Dejagore begins                      | Southern Continent |
| =-1442= | Temple of Traveller's Repose is founded           | Southern Continent |
| =0=     | Beginning of the Domination                       | Northern Continent |
| =470=   | Tenth return of the Great Comet                   | Outer Space        |
| =470=   | The Lady and the Ten Who Were Taken are unearthed | Northern Continent |
"))
  (it "should fall back to a value in :defaults"
    (annalist-test-tome-setup
     :view '(:defaults (:format upcase)
             (year :format annalist-verbatim)))
    (annalist-describe-expect 'annalist 'annalist-test
      "
| Year    | Event                                             | Location           |
|---------+---------------------------------------------------+--------------------|
| =559=   | THE SIEGE OF DEJAGORE BEGINS                      | SOUTHERN CONTINENT |
| =-1442= | TEMPLE OF TRAVELLER'S REPOSE IS FOUNDED           | SOUTHERN CONTINENT |
| =0=     | BEGINNING OF THE DOMINATION                       | NORTHERN CONTINENT |
| =470=   | TENTH RETURN OF THE GREAT COMET                   | OUTER SPACE        |
| =470=   | THE LADY AND THE TEN WHO WERE TAKEN ARE UNEARTHED | NORTHERN CONTINENT |
")))

;; ** Extraction Item Keywords
(describe "The item :max-width keyword"
  (it "should specify the max width for a table cell before truncation"
    (annalist-test-tome-setup
     :view '((event :max-width 40)))
    (annalist-describe-expect 'annalist 'annalist-test
      "
|  Year | Event                                   | Location           |
|-------+-----------------------------------------+--------------------|
|   559 | The Siege of Dejagore begins            | Southern Continent |
| -1442 | Temple of Traveller's Repose is founded | Southern Continent |
|     0 | Beginning of the Domination             | Northern Continent |
|   470 | Tenth return of the Great Comet         | Outer Space        |
|   470 | The Lady and the Ten Who Were Taken are | Northern Continent |
"))
  (it "should fall back to a value from :defaults"
    (annalist-test-tome-setup
     :view '(:defaults (:max-width 40)))
    (annalist-describe-expect 'annalist 'annalist-test
      "
|  Year | Event                                   | Location           |
|-------+-----------------------------------------+--------------------|
|   559 | The Siege of Dejagore begins            | Southern Continent |
| -1442 | Temple of Traveller's Repose is founded | Southern Continent |
|     0 | Beginning of the Domination             | Northern Continent |
|   470 | Tenth return of the Great Comet         | Outer Space        |
|   470 | The Lady and the Ten Who Were Taken are | Northern Continent |
"))
  (it "should take precedence over a value from defaults"
    (annalist-test-tome-setup
     :view '(:defaults (:max-width 20)
             (event :max-width 40)))
    (annalist-describe-expect 'annalist 'annalist-test
      "
|  Year | Event                                   | Location           |
|-------+-----------------------------------------+--------------------|
|   559 | The Siege of Dejagore begins            | Southern Continent |
| -1442 | Temple of Traveller's Repose is founded | Southern Continent |
|     0 | Beginning of the Domination             | Northern Continent |
|   470 | Tenth return of the Great Comet         | Outer Space        |
|   470 | The Lady and the Ten Who Were Taken are | Northern Continent |
")))

(describe "The item :extractp keyword (in combination with :max-width)"
  (it "should determine whether to extract long items into footnotes instead \
of truncating them"
    (annalist-test-tome-setup
     :view '((event :max-width 30
                    :extractp (lambda (_item) t))))
    (annalist-describe-expect 'annalist 'annalist-test
      "
|  Year | Event                        | Location           |
|-------+------------------------------+--------------------|
|   559 | The Siege of Dejagore begins | Southern Continent |
| -1442 | [fn:1]                       | Southern Continent |
|     0 | Beginning of the Domination  | Northern Continent |
|   470 | [fn:2]                       | Outer Space        |
|   470 | [fn:3]                       | Northern Continent |

[fn:3]
The Lady and the Ten Who Were Taken are unearthed

[fn:2]
Tenth return of the Great Comet

[fn:1]
Temple of Traveller's Repose is founded
"))
  (it "should fall back to a a value from :defaults"
    (annalist-test-tome-setup
     :view '(:defaults (:max-width 40 :extractp (lambda (_item) t))))
    (annalist-describe-expect 'annalist 'annalist-test
      "
|  Year | Event                                   | Location           |
|-------+-----------------------------------------+--------------------|
|   559 | The Siege of Dejagore begins            | Southern Continent |
| -1442 | Temple of Traveller's Repose is founded | Southern Continent |
|     0 | Beginning of the Domination             | Northern Continent |
|   470 | Tenth return of the Great Comet         | Outer Space        |
|   470 | [fn:1]                                  | Northern Continent |

[fn:1]
The Lady and the Ten Who Were Taken are unearthed
"))
  (it "should take precedence over a value from defaults"
    (annalist-test-tome-setup
     :view '(:defaults (:extractp (lambda (_item) nil))
             (event :max-width 40 :extractp (lambda (_item) t))))
    (annalist-describe-expect 'annalist 'annalist-test
      "
|  Year | Event                                   | Location           |
|-------+-----------------------------------------+--------------------|
|   559 | The Siege of Dejagore begins            | Southern Continent |
| -1442 | Temple of Traveller's Repose is founded | Southern Continent |
|     0 | Beginning of the Domination             | Northern Continent |
|   470 | Tenth return of the Great Comet         | Outer Space        |
|   470 | [fn:1]                                  | Northern Continent |

[fn:1]
The Lady and the Ten Who Were Taken are unearthed
")))

(describe "The item :src-block-p keyword (in combination with :max-width and \
:extractp)"
  (it "should determine whether to extract long items into source blocks \
instead of footnotes"
    (annalist-test-tome-setup
     :view '((event :max-width 30
                    :extractp (lambda (_item) t)
                    :src-block-p (lambda (_item) t))))
    (annalist-describe-expect 'annalist 'annalist-test
      "
|  Year | Event                        | Location           |
|-------+------------------------------+--------------------|
|   559 | The Siege of Dejagore begins | Southern Continent |
| -1442 | [fn:1]                       | Southern Continent |
|     0 | Beginning of the Domination  | Northern Continent |
|   470 | [fn:2]                       | Outer Space        |
|   470 | [fn:3]                       | Northern Continent |

[fn:3]
#+begin_src emacs-lisp
The Lady and the Ten Who Were Taken are unearthed
#+end_src

[fn:2]
#+begin_src emacs-lisp
Tenth return of the Great Comet
#+end_src

[fn:1]
#+begin_src emacs-lisp
Temple of Traveller's Repose is founded
#+end_src
"))
  (it "should fall back to a a value from :defaults"
    (annalist-test-tome-setup
     :view '(:defaults (:max-width 40
                        :extractp (lambda (_item) t)
                        :src-block-p (lambda (_item) t))))
    (annalist-describe-expect 'annalist 'annalist-test
      "
|  Year | Event                                   | Location           |
|-------+-----------------------------------------+--------------------|
|   559 | The Siege of Dejagore begins            | Southern Continent |
| -1442 | Temple of Traveller's Repose is founded | Southern Continent |
|     0 | Beginning of the Domination             | Northern Continent |
|   470 | Tenth return of the Great Comet         | Outer Space        |
|   470 | [fn:1]                                  | Northern Continent |

[fn:1]
#+begin_src emacs-lisp
The Lady and the Ten Who Were Taken are unearthed
#+end_src
"))
  (it "should take precedence over a value from defaults"
    (annalist-test-tome-setup
     :view '(:defaults (:use-src-block-p (lambda (_item) nil))
             (event :max-width 40
                    :extractp (lambda (_item) t)
                    :src-block-p (lambda (_item) t))))
    (annalist-describe-expect 'annalist 'annalist-test
      "
|  Year | Event                                   | Location           |
|-------+-----------------------------------------+--------------------|
|   559 | The Siege of Dejagore begins            | Southern Continent |
| -1442 | Temple of Traveller's Repose is founded | Southern Continent |
|     0 | Beginning of the Domination             | Northern Continent |
|   470 | Tenth return of the Great Comet         | Outer Space        |
|   470 | [fn:1]                                  | Northern Continent |

[fn:1]
#+begin_src emacs-lisp
The Lady and the Ten Who Were Taken are unearthed
#+end_src
")))

;; * View Definition
(describe "annalist-define-view"
  (it "should support inheriting settings from another view"
    (annalist-test-tome-setup
     :view '(:defaults (:title "All their days are numbered")
             (location :format annalist-verbatim)))
    (annalist-define-view 'annalist-test 'alternate
      '(:defaults (:format annalist-code :title nil)
        :predicate (lambda (record)
                     (string= (nth 2 record) "Northern Continent"))
        (location :title "Place"))
      :inherit 'default)
    (annalist-describe-expect 'annalist 'annalist-test
      "
| Year  | Event                                               | Place                |
|-------+-----------------------------------------------------+----------------------|
| ~0~   | ~Beginning of the Domination~                       | =Northern Continent= |
| ~470~ | ~The Lady and the Ten Who Were Taken are unearthed~ | =Northern Continent= |
"
      'alternate)))

;; * Annalist Record
;; TODO test recording with plist instead of list
;; TODO test that records keep order they are recorded in (and how updating a
;;      record affects the order)
;; TODO test local recording and describing

;; * Builtin Types
;; ** Keybindings Type
;; TODO test formatting and titles
;; TODO test extraction
;; TODO test that record update works (and different values for
;;      annalist-update-previous-key-definition)
;; TODO test preprocess
;; TODO test keymap handling
;; TODO test that valid/active keybinding checks works

;;; test-annalist.el ends here
