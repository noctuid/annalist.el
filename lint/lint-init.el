(require 'elisp-lint)

(defun elisp-lint--custom-fill-column ()
  "Confirm buffer has no lines exceeding `fill-column' in length.
Ignore the first line of the file. In test files (those called \"test-*.el\"),
also ignore strings."
  (save-excursion
    (let (too-long-lines
          (no-strings (string-match-p (rx (and "test-" (1+ any) ".el"))
                                      buffer-file-name)))
      (goto-char (point-min))
      (forward-line)
      (while (not (eobp))
        (when no-strings
          ;; skip past string
          (let* ((ps (syntax-ppss))
                 (comment (nth 4 ps))
                 (string-beg (nth 8 ps)))
            (when (and (not comment) string-beg)
              (goto-char string-beg)
              (forward-sexp))))
        (goto-char (point-at-eol))
        (when (and (> (current-column) fill-column)
                   (or (not no-strings)
                       (save-excursion
                         (backward-char)
                         (not (nth 3 (syntax-ppss))))))
          (push (line-number-at-pos) too-long-lines))
        (forward-line))
      (if too-long-lines
          (error "Lines longer than %d characters: %s"

                 fill-column (elisp-lint--join-lines too-long-lines))
        t))))

(defun elisp-lint--fill-column-no-strings ()
  "Confirm buffer has no lines exceeding `fill-column' in length.
Strings are ignored (used for populating buffers for tests)."
  (elisp-lint--fill-column-no-first t))


;; using package-lint so don't need package-format
(setq elisp-lint-ignored-validators '("fill-column" "package-format"))

(cl-pushnew "custom-fill-column" elisp-lint-buffer-validators
            :test #'equal)

;; TODO should be a package for this... or builtin should be better
;; https://emacs.stackexchange.com/questions/10230/how-to-indent-keywords-aligned
;; https://github.com/Fuco1/.emacs.d/blob/af82072196564fa57726bdbabf97f1d35c43b7f7/site-lisp/redef.el#L20-L94
(defun fuco1/lisp-indent-function (indent-point state)
  "This function is the normal value of the variable `lisp-indent-function'.
The function `calculate-lisp-indent' calls this to determine
if the arguments of a Lisp function call should be indented specially.

INDENT-POINT is the position at which the line being indented begins.
Point is located at the point to indent under (for default indentation);
STATE is the `parse-partial-sexp' state for that position.

If the current line is in a call to a Lisp function that has a non-nil
property `lisp-indent-function' (or the deprecated `lisp-indent-hook'),
it specifies how to indent.  The property value can be:

- `defun', meaning indent `defun'-style
  \(this is also the case if there is no property and the function
  has a name that begins with \"def\", and three or more arguments);

- an integer N, meaning indent the first N arguments specially
  (like ordinary function arguments), and then indent any further
  arguments like a body;

- a function to call that returns the indentation (or nil).
  `lisp-indent-function' calls this function with the same two arguments
  that it itself received.

This function returns either the indentation to use, or nil if the
Lisp function does not specify a special indentation."
  (let ((normal-indent (current-column))
        (orig-point (point)))
    (goto-char (1+ (elt state 1)))
    (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
    (cond
     ;; car of form doesn't seem to be a symbol, or is a keyword
     ((and (elt state 2)
           (or (not (looking-at "\\sw\\|\\s_"))
               (looking-at ":")))
      (if (not (> (save-excursion (forward-line 1) (point))
                  calculate-lisp-indent-last-sexp))
          (progn (goto-char calculate-lisp-indent-last-sexp)
                 (beginning-of-line)
                 (parse-partial-sexp (point)
                                     calculate-lisp-indent-last-sexp 0 t)))
      ;; Indent under the list or under the first sexp on the same
      ;; line as calculate-lisp-indent-last-sexp.  Note that first
      ;; thing on that line has to be complete sexp since we are
      ;; inside the innermost containing sexp.
      (backward-prefix-chars)
      (current-column))
     ((and (save-excursion
             (goto-char indent-point)
             (skip-syntax-forward " ")
             (not (looking-at ":")))
           (save-excursion
             (goto-char orig-point)
             (looking-at ":")))
      (save-excursion
        (goto-char (+ 2 (elt state 1)))
        (current-column)))
     (t
      (let ((function (buffer-substring (point)
                                        (progn (forward-sexp 1) (point))))
            method)
        (setq method (or (function-get (intern-soft function)
                                       'lisp-indent-function)
                         (get (intern-soft function) 'lisp-indent-hook)))
        (cond ((or (eq method 'defun)
                   (and (null method)
                        (> (length function) 3)
                        (string-match "\\`def" function)))
               (lisp-indent-defform state indent-point))
              ((integerp method)
               (lisp-indent-specform method state
                                     indent-point normal-indent))
              (method
               (funcall method indent-point state))))))))

(setq lisp-indent-function #'fuco1/lisp-indent-function)
