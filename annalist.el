;;; annalist.el --- Record and display information such as keybindings  -*- lexical-binding: t; -*-

;; Author: Fox Kiester <noct@posteo.net>
;; URL: https://github.com/noctuid/annalist.el
;; Keywords: convenience, tools, keybindings
;; Package-Requires: ((emacs "24.4") (cl-lib "0.5") (org "0"))
;; Version: 0.1

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
;; This library supports recording information (such as keybindings) and later
;; displaying that information in Org tables.

;; For more information see the repository README.

;;; Code:
(require 'cl-lib)
(eval-when-compile (require 'subr-x))

(defgroup annalist nil
  "Record and display information such as keybindings."
  :group 'convenience
  :prefix "annalist-")

(defcustom annalist-describe-hook nil
  "Hook run in the description buffer after it has been populated.
The buffer is editable when this hook is run."
  :type 'hook)

;; (defcustom annalist-storage-type 'builtin
;;   "The method for storage.
;; The builtin method uses builtin data structures for storing information but does
;; not support full functionality. The emacsql method requires using an external
;; database but supports for functionality."
;;   :type '(choice
;;           (const :tag "Use builin data structures" builtin)
;;           (const :tag "Use an sql database" emacsql)))

;; * Storage Variables
(defvar annalist--tomes nil
  "Stores recorded information for defined tomes.")

(defvar-local annalist--local-tomes nil
  "Stores buffer-local recorded information for defined tomes.")

(defvar annalist--tomes-settings nil
  "Stores settings for defined tomes.")

(defvar annalist--tomes-views (make-hash-table :test #'equal)
  "Stores possible views for defined tomes.")

;; * List Helpers
(defun annalist--merge-lists (a b &optional test)
  "Return the result of merging the lists A in B.
Merging is done by adding items in B that are not in A (as tested by TEST) to
the end of A."
  (append a (cl-set-difference b a :test (or test #'equal))))

(defun annalist--merge-plists (a b)
  "Return the result of merging the plists A and B.
When the same property exists in both A and B, prefer A's value."
  (let ((res (cl-copy-list a)))
    (cl-loop for (prop val) on b by #'cddr
             unless (plist-get res prop)
             do (setq res (plist-put res prop val)))
    res))

(defun annalist--merge-nested-plists (a b)
  "Return the result of merging the settings plists A and B.
When the same property exists in both A and B, prefer A's value unless the value
is a nested plist. In that case, merge the nested plists of the two. This
function only handles one level of nesting."
  (let ((res (cl-copy-list a)))
    (cl-loop for (prop val) on b by #'cddr
             if (not (plist-get res prop))
             do (setf (cl-getf res prop) val)
             else
             if (listp val)
             do (setf (cl-getf res prop)
                      (annalist--merge-plists (plist-get res prop)
                                              val)))
    res))

(defun annalist--get (plist fallback-plist keyword)
  "From PLIST or FALLBACK-PLIST get the corresponding value for KEYWORD.
FALLBACK-PLIST will be checked when KEYWORD does not exist in PLIST (but not in
cases where it is explicitly specified in PLIST as nil)."
  (cl-getf plist keyword
           (plist-get fallback-plist keyword)))

(defun annalist--item-get (settings item prop)
  "Extract an item-specific setting from SETTINGS.
SETTINGS is a settings plist of the form (ITEM1 (PROP1 value1) :defaults (PROP1
defaultvalue)). ITEM is the item to check for PROP for. PROP is the setting to
check for (e.g. :format). If PROP does not appear in the ITEM's plist, return
the value from the :defaults plist (or nil if the property is not specified in
either)."
  (let ((default-item-settings (plist-get settings :defaults))
        (item-settings (plist-get settings item)))
    (annalist--get item-settings default-item-settings prop)))

(defun annalist--test (settings item)
  "Return the test specified by :test in SETTINGS for ITEM's plist.
SETTINGS is a plist in the form (ITEM1 (:test 'eq) ITEM2 (:test 'my-test)
:defaults (:test 'some-test)). If :test is not in ITEM's plist, check for :test
in the :defaults plist. If :test is in neither plist, return #'equal."
  (or (annalist--item-get settings item :test)
      #'equal))

(defun annalist--plistify-settings (definition-settings type &optional no-extras)
  "Convert DEFINITION-SETTINGS to an internally useable plist.
DEFINITION-SETTINGS is a list of arguments for `annalist-define-tome'.
For example:
'(:test my-equal
  :primary-key (keymap key)
  keymap
  key
  definition)

would become (ignoring order):
'(:type TYPE
  :test my-equal
  :primary-key (keymap key)
  :key-indices (0 1)
  :final-index 2
  :metadata-index 3
  0 (:name keymap)
  1 (:name key)
  2 (:name definition)
  keymap (:name keymap)
  key (:name key)
  definition (:name definition))

If NO-EXTRAS is non-nil, exclude the extra generated information (e.g.
:final-index)."
  (let ((counter 0)
        primary-key
        key-indices
        plist)
    (cl-loop for (key val) on definition-settings by #'cddr
             if (eq key :primary-key)
             do (setq primary-key (if (listp val)
                                      val
                                    (list val))))
    (while definition-settings
      (let ((entry (pop definition-settings)))
        (if (keywordp entry)
            (setq plist (plist-put plist entry (pop definition-settings)))
          (let* ((item (if (listp entry)
                           (car entry)
                         entry))
                 (item-definition-settings (append (list :name item)
                                                   (and (listp entry)
                                                        (cdr entry)))))
            (setq plist (plist-put plist item item-definition-settings))
            (setq plist (plist-put plist counter item-definition-settings))
            (when (memq item primary-key)
              (push counter key-indices))
            (cl-incf counter)))))
    (if no-extras
        plist
      (append (list :type type
                    :key-indices key-indices
                    :final-index (1- counter)
                    :metadata-index counter)
              plist))))

(defun annalist-plistify-record (record type)
  "Convert the ordered RECORD list of TYPE to a plist."
  (let* ((settings (annalist--get-tome-settings type))
         (metadata (nth (plist-get settings :metadata-index) record)))
    (cl-loop for i from 0 to (plist-get settings :final-index)
             collect (plist-get (plist-get settings i) :name) into plist
             and collect (nth i record) into plist
             finally (cl-return (nconc plist (list t metadata))))))

(defun annalist-listify-record (record type)
  "Convert the RECORD plist of TYPE to an ordered list."
  (let* ((settings (annalist--get-tome-settings type)))
    (cl-loop for i from 0 to (plist-get settings :final-index)
             collect (plist-get record (plist-get (plist-get settings i) :name))
             into list
             finally (cl-return (nconc list (list (plist-get record t)))))))

(defun annalist--tome (type &optional local)
  "Return the tome for TYPE and LOCAL, creating it if necessary."
  (let ((tome (if local
                  (plist-get annalist--local-tomes type)
                (plist-get annalist--tomes type))))
    (or tome
        (progn
          (if local
              (setq annalist--local-tomes
                    (plist-put annalist--local-tomes type
                               (make-hash-table :test #'equal)))
            (setq annalist--tomes
                  (plist-put annalist--tomes type
                             (make-hash-table :test #'equal))))
          (annalist--tome type local)))))

;; * Type Definition
(defun annalist--get-tome-settings (type)
  "Return the settings plist for TYPE."
  (plist-get annalist--tomes-settings type))

(defun annalist-define-tome (type settings)
  "Create a new type of thing that can be recorded called TYPE.
SETTINGS be a list of items and any settings necessary for recording them."
  (declare (indent 1))
  (setq annalist--tomes-settings
        (plist-put annalist--tomes-settings type
                   (annalist--plistify-settings settings type))))

;; * View Definition
(defun annalist--get-view-settings (type view)
  "Return the settings plist corresponding to TYPE and VIEW."
  (unless view
    (setq view 'default))
  (gethash (cons type view) annalist--tomes-views))

(gv-define-setter annalist--get-view-settings (val type view)
  `(puthash (cons ,type (or ,view 'default)) ,val annalist--tomes-views))

(cl-defun annalist-define-view (type name settings &key inherit)
  "Define a display method for TYPE called NAME.
To define the default view SETTINGS, NAME should be 'default. If INHERIT is
non-nil, inherit SETTINGS from that view."
  (declare (indent 2))
  ;; exclude extra settings because may not have specified every item and don't
  ;; want to change :final-index
  (setq settings (annalist--plistify-settings settings type t))
  (when inherit
    (setq settings (annalist--merge-nested-plists
                    settings
                    (annalist--get-view-settings type inherit))))
  (setf (annalist--get-view-settings type name) settings))

;; * Recording
(defun annalist--record-record (new-record existing-records settings)
  "Non-destructively add NEW-RECORD to EXISTING-RECORDS and return it.
SETTINGS is the plist of settings for the type of thing/tome the record
corresponds to (e.g. keybindings).

When the primary key in NEW-RECORD matches that in an old record exactly (as
determined by :test in SETTINGS or `equal'), remove the old record and add
NEW-RECORD to the front of EXISTING-RECORDS.

When :record-update is present in SETTINGS, use its value to update the
NEW-RECORD (e.g. to update a \"previous definition\" item). An update function
is passed the old record (nil if none), NEW-RECORD, and SETTINGS. It should
return an updated recording to store."
  (let* ((key-indices (plist-get settings :key-indices))
         (update (plist-get settings :record-update))
         old-record)
    (setq existing-records
          (cl-loop for record in existing-records
                   if (and key-indices
                           (cl-dolist (index key-indices t)
                             (let ((test (annalist--test settings index)))
                               (unless (funcall test
                                                (nth index new-record)
                                                (nth index record))
                                 (cl-return)))))
                   ;; remove old record and put new record at front
                   do (setq old-record record)
                   else
                   ;; keep record as-is
                   collect record))
    (when update
      (setq new-record (funcall update old-record new-record settings)))
    (cons new-record existing-records)))

(defun annalist--record-headings (record store depth settings)
  "Non-destructively record RECORD into STORE, returning STORE.
RECORD is a list of the headings and column entries for a row to be recorded.
DEPTH is the depth of the current item being recorded. SETTINGS is the plist of
settings for the type of thing/tome being recorded (e.g. keybindings). If DEPTH
exceeds the max heading depth in SETTINGS (i.e. it is the depth at which the
table starts as specified by :table-start-index), insert RECORD into the current
STORE and return it. Otherwise, record the current item as a heading in STORE
and recurse with an incremented DEPTH."
  (if (>= depth
          (plist-get settings :table-start-index))
      (annalist--record-record record store settings)
    (let* ((test (annalist--test settings depth))
           (store (if (hash-table-p store)
                      store
                    (make-hash-table :test test)))
           (next-store (gethash (nth depth record) store)))
      (puthash (nth depth record)
               (annalist--record-headings record next-store (1+ depth) settings)
               store)
      store)))

;;;###autoload
(cl-defun annalist-record (annalist type record &key local plist)
  "In the store for ANNALIST, TYPE, and LOCAL, record RECORD.
ANNALIST should correspond to the package/user recording this information (e.g.
'general, 'me, etc.). TYPE is the type of information being recorded (e.g.
'keybindings). LOCAL corresponds to whether to store RECORD only for the current
buffer. This information together is used to select where RECORD should be
stored in and later retrieved from with `annalist-describe'. RECORD should be a
list of items to record and later print as org headings and column entries in a
single row. If PLIST is non-nil, RECORD should be a plist instead of an ordered
list (e.g. '(keymap org-mode-map key \"C-c a\" ...)). The plist keys should be
the symbols used for the definition of TYPE."
  (let* ((tome (annalist--tome type local))
         (settings (annalist--get-tome-settings type))
         (preprocess (plist-get settings :preprocess))
         (store (gethash annalist tome)))
    (when plist
      (setq record (annalist-listify-record record type)))
    (when preprocess
      (setq record (funcall preprocess record)))
    (puthash annalist
             (annalist--record-headings record store 0 settings)
             tome)))

;; * Printing
(defun annalist--safe-pipe (item)
  "Format ITEM to replace all \"|\" occurrences with \"¦\"."
  (replace-regexp-in-string "|" "¦" item))

(defvar annalist--fn-counter nil
  "Counter for the current footnote.")

(defun annalist--print-table-header (settings)
  "Print an org table header using the titles from SETTINGS."
  (let ((i (plist-get settings :table-start-index))
        item-settings
        title)
    (while (setq item-settings (plist-get settings i))
      (setq title (or (plist-get item-settings :title)
                      (capitalize (symbol-name
                                   (plist-get item-settings :name)))))
      (princ "|")
      (princ (annalist--safe-pipe (format "%s" title)))
      (cl-incf i)))
  (princ "|\n|-+-|\n"))

;; TODO split this up
(defun annalist--print-table (records settings)
  "Print an org table for RECORDS using SETTINGS."
  ;; printed oldest to newest
  ;; TODO could add option to do newest to oldest instead
  (setq records (reverse records))
  (let* ((predicate (plist-get settings :predicate))
         (sorter (plist-get settings :sort))
         (sorted-records (if sorter
                             (sort records sorter)
                           records))
         (start-index (plist-get settings :table-start-index))
         footnotes)
    ;; print header
    (annalist--print-table-header settings)
    ;; print rows
    (dolist (record sorted-records)
      (when (or (null predicate)
                (funcall predicate record))
        (cl-loop
         for i from start-index to (plist-get settings :final-index)
         do
         (let* ((item (nth i record))
                (formatter (annalist--item-get settings i :format))
                (max-width (annalist--item-get settings i :max-width))
                (extractp (annalist--item-get settings i :extractp))
                (src-block-p (annalist--item-get settings i :src-block-p))
                (too-long (and max-width
                               (> (length (format "%s" item))
                                  max-width))))
           ;; space ensures that a negative number, for example, doesn't expand
           ;; into a |---+---+---| line later
           (princ "| ")
           (cond ((and too-long
                       extractp
                       (funcall extractp item))
                  (princ (format "[fn:%s]" annalist--fn-counter))
                  (push (list annalist--fn-counter item formatter
                              (funcall src-block-p item))
                        footnotes)
                  (cl-incf annalist--fn-counter))
                 (t
                  (when too-long
                    (setq item (format (format "%%.%ss" max-width) item)))
                  (when formatter
                    (setq item (funcall formatter item)))
                  (princ (annalist--safe-pipe (format "%s" item))))))))
      (princ "|\n"))
    (princ "\n")
    ;; print footnotes
    (dolist (footnote footnotes)
      (let ((num (car footnote))
            (item (cadr footnote))
            (use-src-block (cl-cadddr footnote))
            (formatter (cl-caddr footnote)))
        (princ (format "[fn:%s]\n" num))
        (cond (use-src-block
               (princ "#+begin_src emacs-lisp\n")
               (princ (format "%s\n" item))
               (princ "#+end_src\n"))
              (t
               (princ (format "%s\n" (if formatter
                                         (funcall formatter item)
                                       item)))))
        (princ "\n")))))

(defun annalist--print-headings (store depth settings &optional
                                       increase-print-depth)
  "Print information from STORE as `org-mode' headings.
DEPTH is the depth of the current heading. SETTINGS contains information about
which entries in STORE are headings and how to print them. If
INCREASE-PRINT-DEPTH is non-nil, increase the level of all printed headings by
one."
  (if (>= depth (plist-get settings :table-start-index))
      (annalist--print-table store settings)
    (let* ((formatter (annalist--item-get settings depth :format))
           (priority-keys (annalist--item-get settings depth :prioritize))
           (keys (hash-table-keys store))
           (sorter (annalist--item-get settings depth :sort))
           (sorted-keys (annalist--merge-lists priority-keys
                                               (if sorter
                                                   (sort keys sorter)
                                                 keys)))
           (predicate (annalist--item-get settings depth :predicate))
           (asterisk-num (if increase-print-depth
                             (+ 2 depth)
                           (1+ depth))))
      (dolist (key sorted-keys)
        (let ((next-store (gethash key store)))
          (when (and next-store
                     (or (null predicate)
                         (funcall predicate key)))
            ;; print heading
            (unless (null key)
              (princ (format "%s %s\n" (make-string asterisk-num ?*)
                             (if formatter
                                 (funcall formatter key)
                               key))))
            (annalist--print-headings next-store (1+ depth) settings
                                      increase-print-depth)))))))

(declare-function org-at-heading-p "org")
(declare-function org-table-align "org-table")
(declare-function outline-next-heading "outline")
(defvar org-startup-folded)
;;;###autoload
(defun annalist-describe (annalist type &optional view)
  "Describe information recorded by ANNALIST for TYPE.
For example: (annalist-describe 'general 'keybindings) If VIEW is non-nil, use
those settings for displaying recorded information instead of the defaults."
  (let* ((settings (annalist--merge-nested-plists
                    (annalist--get-view-settings type view)
                    (annalist--get-tome-settings type)))
         (tome (annalist--tome type))
         (local-tome (annalist--tome tome t))
         (name-store (when tome
                       (gethash annalist tome)))
         (local-name-store (when local-tome
                             (gethash annalist local-tome)))
         (output-buffer-name (format "*%s %s*" annalist type))
         (annalist--fn-counter 1))
    ;; NOTE `with-output-to-temp-buffer' does not change the current buffer (so
    ;; it is possible to check active keymaps in a predicate function)
    (when local-name-store
      (with-output-to-temp-buffer output-buffer-name
        (princ "* Local")
        (annalist--print-headings local-name-store 0 settings t)))
    (when name-store
      (with-output-to-temp-buffer output-buffer-name
        (when local-name-store
          (princ "* Global"))
        (annalist--print-headings name-store 0 settings local-name-store)))
    (when (or local-name-store name-store)
      (with-current-buffer output-buffer-name
        (let ((org-startup-folded 'showall))
          (org-mode))
        (read-only-mode -1)
        ;; TODO delete empty tables then headings (e.g. if predicate for row
        ;; fails but predicate for headings didn't)
        (while (progn
                 (while (progn
                          (forward-line)
                          (org-at-heading-p)))
                 (org-table-align)
                 (outline-next-heading)))
        (goto-char (point-min))
        (run-hooks 'annalist-describe-hook)
        (read-only-mode)))))

;; * Type Creation Helpers
;; ** Formatting
(defun annalist-verbatim (item)
  "Format ITEM to be surrounded by equal signs."
  (format "=%s=" item))

(defun annalist-code (item)
  "Format ITEM to be surrounded by tildes."
  (format "~%s~" item))

(defun annalist-capitalize (item)
  "Convert ITEM to a string and capitalize it."
  (capitalize (format "%s" item)))

(defun annalist-compose (&rest fns)
  "Return a function composed of FNS.
FNS will be called right to left."
  (let ((fn1 (car (last fns)))
        (fns (butlast fns)))
    (lambda (&rest args)
      (cl-reduce #'funcall fns
                 :from-end t
                 :initial-value (apply fn1 args)))))

;; ** Sorting
(defun annalist-string-< (x y)
  "Return whether X is lexicographiclly less than Y.
The string forms of X and Y as obtained with `format' are compared."
  (string< (format "%s" x) (format "%s" y)))

(defun annalist-key-< (x y)
  "Return whether X is lexicographically less than Y.
Both are considered to be keys in their bindable forms. Compare their
descriptive forms as obtained with `key-description'"
  (string< (key-description x) (key-description y)))

;; * Keybindings Type
(defun annalist--preprocess-keybinding (record)
  "Preprocess RECORD by normalizing the keymap.
If the keymap is 'global and the state is non-nil, set the keymap to be the
actual evil global keymap (e.g. 'evil-normal-state-map)."
  (let* ((record (annalist-plistify-record record 'keybindings))
         (keymap (plist-get record 'keymap))
         (state (plist-get record 'state)))
    (when (and state (eq keymap 'global))
      (plist-put record 'state nil)
      (plist-put record
                 'keymap
                 (evil-state-property state :keymap)))
    (annalist-listify-record record 'keybindings)))

(declare-function evil-state-property "evil-common")
(declare-function evil-get-minor-mode-keymap "evil-core")
(declare-function evil-get-auxiliary-keymap "evil-core")
(cl-defun annalist--get-keymap (state keymap-sym &optional minor-mode)
  "Get the keymap corresponding to STATE and KEYMAP-SYM.
If MINOR-MODE is non-nil, KEYMAP-SYM is considered to be a minor mode name.
Return nil if the STATE and KEYMAP-SYM combination is invalid (e.g. the keymap
does not yet exist)."
  (let ((keymap (when (boundp keymap-sym)
                  (symbol-value keymap-sym))))
    (if state
        (cond ((eq keymap-sym 'global)
               (evil-state-property state :keymap t))
              (minor-mode
               (evil-get-minor-mode-keymap state keymap-sym))
              ((eq keymap-sym 'local)
               (evil-state-property state :local-keymap t))
              (t
               (when keymap
                 (evil-get-auxiliary-keymap keymap state t t))))
      (if (eq keymap-sym 'global)
          (current-global-map)
        keymap))))

(defun annalist--lookup-key (keymap key)
  "Return the current definition for KEYMAP and KEY.
When a sub-sequence of KEY is bound, return nil instead of 1."
  (when (and keymap key)
    (let ((def (lookup-key keymap key)))
      (if (equal def 1)
          nil
        def))))

(defcustom annalist-update-previous-key-definition 'on-change
  "When to update the stored previous key definition.
When set to 'on-change, update the previous definition only when the old
definition is different from the current one (e.g. evaluating a `define-key'
call twice will not affect the stored previous definition the second time). When
set to nil, only update the previous definition when the key was previously
unbound/nil."
  ;; can't think of a use case, but add 'always if requested
  ;; t is equivalent of on-change
  :type '(choice
          (const :tag "When definition has changed" on-change)
          (const :tag "When the key was previously unbound" nil)))

(defun annalist--previous-value (old-val current-val new-val &optional test)
  "Update a \"previous\" value for something.
OLD-VAL is the currently stored \"previous\" value. CURRENT-VAL is the actual
current value for the thing (which could potentially be different from the
stored current value if not all functions that change the thing call
`annalist-record'). NEW-VAL is the new value that the thing will be changed to.
TEST is the test to used to compare values (or `equal'). If there is no OlD-VAL
or if NEW-VAL is still equal to CURRENT-VAL and
`annalist-update-previous-key-definition' is non-nil, return CURRENT-VAL.
Otherwise return OLD-VAL."
  (if (or (null old-val)
          (and annalist-update-previous-key-definition
               (not (funcall (or test #'equal)
                             current-val new-val))))
      current-val
    old-val))

(defun annalist--update-keybinding (old-record new-record settings)
  "Using the information in OLD-RECORD update NEW-RECORD.
The previous definition item in NEW-RECORD will updated based on the old
recorded previous definition (which may not exist), the actual/current
definition, and the new definition. SETTINGS is used to check for a test
function for comparing key definitions."
  (let* ((type (plist-get settings :type))
         (old-record (annalist-plistify-record old-record type))
         (new-record (annalist-plistify-record new-record type))
         (new-metadata (plist-get new-record t))
         (old-def (plist-get old-record 'definition))
         (keymap-sym (plist-get new-record 'keymap))
         (state (plist-get new-record 'state))
         (key (plist-get new-record 'key))
         (keymap (or (plist-get new-metadata :keymap)
                     (annalist--get-keymap
                      state
                      keymap-sym
                      (plist-get new-metadata :minor-mode))))
         (current-def (when keymap
                        (annalist--lookup-key keymap key)))
         (new-def (plist-get new-record 'definition))
         (test (annalist--test settings 'definition)))
    ;; keybinding may still be deferred
    (when current-def
      (plist-put new-record
                 'previous-definition
                 (annalist--previous-value old-def current-def new-def test)))
    (annalist-listify-record new-record type)))

(defun annalist--valid-keymap-p (keymap-sym)
  "Return whether KEYMAP-SYM is bound.
This is necessary since it is possible to record keybindings before they are
actually defined (e.g. keybindings may be deferred until the keymap exists).
'local and 'global are handled specially (return non-nil)."
  (or (memq keymap-sym '(local global))
      (boundp keymap-sym)))

(defun annalist--active-keymap-p (keymap-sym)
  "Return whether KEYMAP-SYM's value is an active keymap."
  (or (memq keymap-sym '(local global))
      (and (boundp keymap-sym)
           ;; TODO handling case where keymap-sym is a minor mode
           (memq (symbol-value keymap-sym) (current-active-maps)))))

(declare-function evil-state-p "evil-core")
(defun annalist--valid-state-p (state)
  "Return whether STATE is valid."
  (or (null state)
      (and (featurep 'evil)
           (evil-state-p state))))

(defvar evil-local-mode)
(defun annalist--valid-state-and-evil-on-p (state)
  "Return whether STATE is valid `evil-local-mode' is on."
  (and (annalist--valid-state-p state) evil-local-mode))

(annalist-define-tome 'keybindings
  (list :primary-key '(keymap state key)
        :table-start-index 2
        :preprocess #'annalist--preprocess-keybinding
        :record-update #'annalist--update-keybinding
        'keymap
        'state
        'key
        'definition
        'previous-definition))

(annalist-define-view 'keybindings 'default
  (list (list 'keymap :format #'annalist-code)
        (list 'state :format #'annalist-capitalize)
        (list 'key :format (annalist-compose #'annalist-verbatim
                                             #'key-description))
        (list 'definition :format #'annalist-code)
        (list 'previous-definition :title "Previous" :format #'annalist-code)
        :extractp #'listp
        :src-block-p #'listp))

;; * Settings Type
;; TODO

;; * Hooks Type
;; TODO

;; * Advice Type
;; TODO

(provide 'annalist)
;;; annalist.el ends here
