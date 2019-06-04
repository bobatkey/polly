;; based on: http://ergoemacs.org/emacs/elisp_syntax_coloring.html

;; define several class of keywords
(setq polly-keywords  '("define" "as" "external" "table" "end"))
; (setq polly-equality  '("coerce" "refl" "subst"))
; (setq polly-operators '("->" "=" "/"))
(setq polly-types     '("string" "integer" "json" "decision"
                        "boolean" "json-field"))

;; create the regex string for each class of keywords
(setq polly-keywords-regexp  (regexp-opt polly-keywords  'symbols))
; (setq polly-equality-regexp  (regexp-opt polly-equality 'words))
; (setq polly-operators-regexp (regexp-opt polly-operators))
(setq polly-types-regexp     (regexp-opt polly-types     'symbols))

;; clear memory
(setq polly-keywords  nil)
(setq polly-equality  nil)
(setq polly-operators nil)
(setq polly-types     nil)

;; create the list for font-lock.
;; each class of keyword is given a particular face
(setq polly-font-lock-keywords
  `(
    (,polly-types-regexp     . font-lock-type-face)
  ;   (,polly-equality-regexp  . font-lock-keyword-face)
  ;   (,polly-operators-regexp . font-lock-builtin-face)
    (,polly-keywords-regexp  . font-lock-keyword-face)
))

;; syntax table
(defvar polly-syntax-table nil "Syntax table for `polly-mode'.")
(setq polly-syntax-table
  (let ((synTable (make-syntax-table)))

    (modify-syntax-entry ?/ ". 12b" synTable)
    (modify-syntax-entry ?\n "> b" synTable)

    synTable))

;; define the mode
(define-derived-mode polly-mode fundamental-mode
  "POLLY mode"
  ;; handling comments
  :syntax-table polly-syntax-table
  ;; code for syntax highlighting
  (setq font-lock-defaults '((polly-font-lock-keywords)))
  (setq mode-name "polly")
  ;; clear memory
  (setq polly-keywords-regexp nil)
  (setq polly-types-regexp nil)
)

(provide 'polly-mode)
