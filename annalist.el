;; annalist.el --- Record and display information such as keybindings  -*- lexical-binding: t; -*-

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

;; For more information see the README in the online repository.

;;; Code:
(require 'cl-lib)
(require 'subr-x)

(defgroup annalist nil
  "Record and display information such as keybindings."
  :group 'convenience
  :prefix "annalist-")

(defcustom annalist-describe-hook nil
  "Hook run in the description buffer after it has been populated.
The buffer is editable when this hook is run."
  :group 'annalist
  :type 'hook)

;; * General Helpers
(defun annalist--symbol (name)
  "Preface NAME with \"annalist-\", returning an interned symbol."
  (intern (format "annalist-%s" name)))

(defun annalist--local-symbol (name)
  "Preface NAME with \"annalist-local-\", returning an interned symbol."
  (intern (format "annalist-local-%s" name)))

(defun annalist--override-settings-symbol (name)
  "Return \"annalist-NAME-override-settings\" as an interned symbol."
  (intern (format "annalist-%s-override-settings" name)))

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

(defun annalist--merge-settings-plists (a b)
  "Return the result of merging the settings plists A and B.
When the same property exists in both A and B, prefer A's value unless the value
is a nested plist. In that case, merge the nested plists of the two. The values
for :items and for numeric properties are assumed to be plists themselves. The
values for other properties are not considered to be plists."
  (let ((res (cl-copy-list a)))
    (cl-loop for (prop val) on b by #'cddr
             if (not (plist-get res prop))
             do (setf (cl-getf res prop) val)
             else
             if (eq prop :items)
             do (setf (cl-getf res prop)
                      (annalist--merge-settings-plists (plist-get res prop)
                                                       val))
             else
             if (numberp prop)
             do (setf (cl-getf res prop)
                      (annalist--merge-plists (plist-get res prop) val)))
    res))

(defun annalist--plist-inherit (plist)
  "Return PLIST merged with its inherited plist, if any.
The :inherit keyword specifies the type (e.g. \"keybindings\") to inherit
values from. Values in PLIST take precedence."
  (let ((inherit-plist (let* ((it (plist-get plist :inherit))
                              (sym (annalist--symbol it)))
                         (when it
                           (symbol-plist sym)))))
    (if inherit-plist
        (annalist--merge-settings-plists plist inherit-plist)
      plist)))

(defvar annalist-override-settings nil
  "Property list of settings that will override settings for any type.
Note that an :inherit property in this plist will be ignored.")

(defun annalist--settings (type)
  "Return the settings that should be used for TYPE.
Settings in TYPE's override plist take precedence over TYPE's default settings,
and settings in `annalist-override-settings' have the highest precedence."
  (let ((type-settings (annalist--plist-inherit
                        (symbol-plist (annalist--symbol type))))
        (type-override-settings (symbol-value
                                 (annalist--override-settings-symbol type))))
    (annalist--merge-settings-plists
     annalist-override-settings
     (annalist--merge-settings-plists type-override-settings type-settings))))

(defun annalist--store (type &optional local)
  "Return the store corresponding to TYPE and LOCAL.
If LOCAL is non-nil, return the buffer-local store for TYPE. Otherwise return
the global store for TYPE."
  (let ((global-store (symbol-value (annalist--symbol type)))
        (local-store (symbol-value (annalist--local-symbol type))))
    (if local
        local-store
      global-store)))

(defun annalist--get (plist fallback-plist keyword)
  "From PLIST or FALLBACK-PLIST get the corresponding value for KEYWORD.
FALLBACK-PLIST will be checked when KEYWORD does not exist in PLIST (but not in
cases where it is explicitly specified in PLIST as nil)."
  (cl-getf plist keyword
           (cl-getf fallback-plist keyword)))

(defun annalist--test (settings &optional fallback-settings)
  "Return the test specified by :test in SETTINGS or FALLBACK-SETTINGS.
If :test is in neither plist, return #'equal."
  (or (annalist--get settings fallback-settings :test)
      #'equal))

;; * Type Definition
(defun annalist-define-type (type &rest settings)
  "Create a new type of thing (or \"tome\") that can be recorded called TYPE.
This will automatically create global and local stores variables called
\"annalist-TYPE\" and \"annalist-local-TYPE\" (e.g. `annalist-keybindings' and
`annalist-local-keybindings'). It will also create a variable called
\"annalist-TYPE-override-settings\" that can be changed to override the default
SETTINGS. SETTINGS should be a plist of annalist type settings; see the README
for more information."
  (declare (indent defun))
  (let* ((prefixed-type (annalist--symbol type))
         (store prefixed-type)
         (local-store (annalist--local-symbol type))
         (type-override-plist (annalist--override-settings-symbol type))
         (test (annalist--test settings)))
    (eval `(defvar ,store (make-hash-table :test ',test)
             ,(format "Store (or \"tome\") for recording information about %s."
                      type)))
    (eval `(defvar-local ,local-store (make-hash-table :test ',test)
             ,(format
               "Local store (or \"tome\") for recording information about %s."
               type)))
    (eval `(defvar ,type-override-plist nil
             ,(format
               "Settings that will override the defaults for the %s type."
               type)))
    (cl-loop for (prop value)
             on settings by #'cddr
             do (put prefixed-type prop value))))

;; * Recording
(defun annalist--record-record (data metadata records settings)
  "Non-destructively add DATA and METADATA to RECORDS, returning RECORDS.
SETTINGS is the plist of settings for the type of thing the record corresponds
to (e.g. keybindings).

When DATA matches the data in an old record exactly (as determined by :test in
SETTINGS or `equal'), update the old record's metadata with METADATA but keep
its order in RECORDS. Otherwise, when :key-index is present in SETTINGS and
optionally :test in that index's local or default settings, use their values to
check for a matching old record. When one is found, remove it and add the new
record (DATA . METADATA) to the front of RECORDS. Only compare data, not
metadata.

When :record-update is present, use its value to update the new record (e.g. to
update a \"previous definition\" item). An update function is passed the old
record (nil if none), (DATA . METADATA), and SETTINGS. It should return the
 (data . metadata) to store."
  (let* ((test (annalist--test settings))
         (key-index (plist-get settings :key-index))
         (items-settings (plist-get settings :items))
         (key-settings (plist-get items-settings key-index))
         (key-test (annalist--test key-settings items-settings))
         (update (plist-get settings :record-update))
         (new-record (cons data metadata))
         old-record)
    (setq records
          (cl-loop for record in records
                   if (funcall test data (car record))
                   ;; keep record order, update metadata
                   collect new-record
                   else
                   if (and key-index
                           (funcall key-test
                                    (nth key-index data)
                                    (nth key-index (car record))))
                   ;; remove old record and put new record at front
                   do (setq old-record record)
                   else
                   ;; keep record as-is
                   collect record))
    (when update
      (setq new-record (funcall update old-record new-record settings)))
    (cons new-record records)))

(defun annalist--record-headings (data metadata store depth settings)
  "Non-destructively record DATA and METADATA into STORE, returning STORE.
DATA is a list of the headings and column entries for a row to be recorded.
DEPTH is the depth of the current item being recorded. SETTINGS is the plist of
settings for the type of thing being recorded (e.g. keybindings). If DEPTH
exceeds the max heading depth in SETTINGS (i.e. it is the depth at which the
table starts as specified by :table-start-index), insert DATA and METADATA into
the current STORE and return it. Otherwise, record the current item as a heading
in STORE and recurse with an incremented DEPTH.()"
  (if (>= depth
          (plist-get settings :table-start-index))
      (annalist--record-record data metadata store settings)
    (let* ((items-settings (plist-get settings :items))
           (item-settings (plist-get items-settings depth))
           (test (annalist--test item-settings items-settings))
           (store (if (hash-table-p store)
                      store
                    (make-hash-table :test test)))
           (next-store (gethash (nth depth data) store)))
      (puthash (nth depth data)
               (annalist--record-headings data metadata next-store (1+ depth)
                                          settings)
               store)
      store)))

;;;###autoload
(cl-defun annalist-record (name type data &optional metadata &key local)
  "In the store for NAME, TYPE, and LOCAL, record DATA and METADATA.
NAME should correspond to the package/user (the \"annalist\") recording this
information (e.g. 'general, 'me, etc.). TYPE is the type of information being
recorded (e.g. 'keybindings). LOCAL corresponds to whether to store DATA only
for the current buffer. This information together is used to select the
store (or \"tome\") to DATA and METADATA (together a \"record\") should be
stored in and later retrieved from with `annalist-describe'. DATA should be a
list of items to record and later print as org headings and column entries in a
single row. METADATA should be a plist of additional information; this
information will not be printed but will later be usable for filtering, for
example."
  (let* ((settings (annalist--settings type))
         (store (annalist--store type local))
         (name-store (gethash name store)))
    (puthash name
             (annalist--record-headings data metadata name-store 0 settings)
             store)))

;; * Printing
(defun annalist--safe-pipe (item)
  "Format ITEM to replace all \"|\" occurrences with \"¦\"."
  (replace-regexp-in-string "|" "¦" item))

(defvar annalist--fn-counter nil
  "Counter for the current footnote.")

(defun annalist--print-table-header (settings)
  "Print an org table header using the titles from SETTINGS."
  (let ((i (plist-get settings :table-start-index))
        (items-settings (plist-get settings :items))
        item-settings)
    (while (setq item-settings (plist-get items-settings i))
      (princ "|")
      (princ (annalist--safe-pipe
              (format "%s" (plist-get item-settings :title))))
      (cl-incf i)))
  (princ "|\n|-+-|\n"))

(defun annalist--print-table (records settings)
  "Print an org table for RECORDS using SETTINGS."
  (setq records (nreverse records))
  (let* ((items-settings (plist-get settings :items))
         (predicate (plist-get settings :predicate))
         (sorter (plist-get settings :sort))
         (key-index (plist-get settings :key-index))
         (key-sorter (annalist--get (plist-get items-settings key-index)
                                    items-settings
                                    :sort))
         (sorted-records (cond (sorter
                                (sort records sorter))
                               (key-sorter
                                (sort records
                                      (lambda (x y)
                                        (funcall key-sorter
                                                 (nth key-index (car x))
                                                 (nth key-index (car y))))))
                               (t
                                records)))
         (start-index (plist-get settings :table-start-index))
         footnotes)
    ;; print header
    (annalist--print-table-header settings)
    ;; print rows
    (dolist (record sorted-records)
      (when (or (null predicate)
                (funcall predicate record))
        (cl-loop
         for i from start-index below (length (car record))
         do
         (let* ((item (nth i (car record)))
                (item-settings (plist-get items-settings i))
                (formatter (annalist--get item-settings items-settings
                                          :format))
                (max-width (annalist--get item-settings items-settings
                                          :max-width))
                (extractp (annalist--get item-settings items-settings
                                         :extractp))
                (src-block-p (annalist--get item-settings items-settings
                                            :src-block-p))
                (too-long (and max-width
                               (> (length (format "%s" item))
                                  max-width))))
           (princ "|")
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
  (if (>= depth
          (plist-get settings :table-start-index))
      (annalist--print-table store settings)
    (let* ((items-settings (plist-get settings :items))
           (item-settings (plist-get items-settings depth))
           (formatter (annalist--get item-settings items-settings :format))
           (priority-keys (annalist--get item-settings items-settings
                                         :prioritize))
           (keys (hash-table-keys store))
           (sorter (annalist--get item-settings items-settings :sort))
           (sorted-keys (annalist--merge-lists priority-keys
                                               (if sorter
                                                   (sort keys sorter)
                                                 keys)))
           (predicate (annalist--get item-settings items-settings :predicate))
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
(defun annalist-describe (name type)
  "Describe information recorded by NAME about TYPE.
For example: (annalist-describe 'general 'keybindings)"
  (let* ((settings (annalist--settings type))
         (store (annalist--store type))
         (local-store (annalist--store type t))
         (name-store (when store
                       (gethash name store)))
         (local-name-store (when local-store
                             (gethash name local-store)))
         (output-buffer-name (format "*%s %s*" name type))
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
  :group 'annalist
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
  (let* ((new-data (car new-record))
         (new-metadata (cdr new-record))
         (old-data (car old-record))
         (old-def (nth 4 old-data))
         (keymap-sym (nth 0 new-data))
         (state (nth 1 new-data))
         (key (nth 2 new-data))
         (keymap (or (plist-get new-metadata :keymap)
                     (annalist--get-keymap
                      state
                      keymap-sym
                      (plist-get new-metadata :minor-mode))))
         (current-def (when keymap
                        (annalist--lookup-key keymap key)))
         (new-def (nth 3 new-data))
         (items-settings (plist-get settings :items))
         (def-settings (plist-get settings 3))
         (test (annalist--test def-settings items-settings)))
    ;; keybinding may still be deferred
    (when current-def
      (setf (nth 4 new-data)
            (annalist--previous-value old-def current-def new-def test)))
    (cons new-data new-metadata)))

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

(annalist-define-type 'keybindings
  :key-index 2
  :table-start-index 2
  :record-update #'annalist--update-keybinding
  :items
  (list 0 (list :title "Keymap" :format #'annalist-code
                :predicate #'annalist--valid-keymap-p)
        1 (list :title "State" :format #'annalist-capitalize
                :predicate #'annalist--valid-state-p)
        2 (list :title "Key" :format (annalist-compose #'annalist-verbatim
                                                       #'key-description))
        3 (list :title "Definition" :format #'annalist-code)
        4 (list :title "Previous" :format #'annalist-code)
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
