;;; timu-line.el --- Custom and simple mode line -*- lexical-binding: t; -*-

;; Author: Aimé Bertrand <aime.bertrand@macowners.club>
;; Version: 0.4
;; Package-Requires: ((emacs "28.1"))
;; Created: 2023-07-31
;; Keywords: modeline frames ui
;; Homepage: https://gitlab.com/aimebertrand/timu-line
;; This file is NOT part of GNU Emacs.

;; The MIT License (MIT)
;;
;; Copyright (c) 2023 Aimé Bertrand
;;
;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation files (the
;; "Software"), to deal in the Software without restriction, including
;; without limitation the rights to use, copy, modify, merge, publish,
;; distribute, sublicense, and/or sell copies of the Software, and to
;; permit persons to whom the Software is furnished to do so, subject to
;; the following conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;; IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
;; CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
;; TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
;; SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

;;; Commentary:
;; A custom and simple mode line for Emacs.

;;; Code:


;;; DEFAULTS
(defgroup timu-line ()
  "Customise group for the `timu-line' Library."
  :group 'timu)


;;; CUSTOM VARIABLES
(defcustom timu-line-path
  (expand-file-name "libraries/timu-line.el" user-emacs-directory)
  "Variable for the path of the module `timu-line'."
  :type 'file
  :group 'timu-line)

(defcustom timu-line-elfeed-modes
  '(elfeed-show-mode
    elfeed-search-mode)
  "Elfeed modes for which to display a \"*elfeed*\" in the mode line."
  :type '(repeat symbol)
  :group 'timu-line)

(defcustom timu-line-mu4e-context-modes
  '(mu4e-main-mode
    mu4e-view-mode
    mu4e-loading-mode
    mu4e-compose-mode
    mu4e-headers-mode)
  "Mu4e modes for which to display the `mu4e-contexts' in the mode line."
  :type '(repeat symbol)
  :group 'timu-line)

(defcustom timu-line-show-vc-branch t
  "Control weather to show the vc branch in the mode line.
By default set to true."
  :type 'boolean
  :group 'timu-line)

(defcustom timu-line-show-lsp-indicator nil
  "Control weather to show an lsp indicator in the mode line.
By default set to nil."
  :type 'boolean
  :group 'timu-line)

(defcustom timu-line-show-eglot-indicator nil
  "Control weather to show an eglot indicator in the mode line.
By default set to nil."
  :type 'boolean
  :group 'timu-line)

(defcustom timu-line-show-python-virtual-env t
  "Control weather to show the python Venv in the mode line.
By default set to true."
  :type 'boolean
  :group 'timu-line)

(defcustom timu-line-show-org-capture-hints t
  "Control weather to show the org capture hints in the mode line.
By default set to true."
  :type 'boolean
  :group 'timu-line)

(defcustom timu-line-show-mu4e-context t
  "Control weather to show the mu4e context in the mode line.
By default set to true."
  :type 'boolean
  :group 'timu-line)

(defcustom timu-line-show-elfeed-counts t
  "Control weather to show elfeed counts in the mode line.
By default set to true."
  :type 'boolean
  :group 'timu-line)

(defcustom timu-line-show-evil-state nil
  "Control weather to show the evil state in the mode line.
By default set to true."
  :type 'boolean
  :group 'timu-line)


;;; BUFFER-LOCAL VARIABLES
(defvar-local timu-line-spacer-one
    (propertize "  " 'face 'timu-line-active-face 'display `(raise +0.20)))

(defvar-local timu-line-spacer-two
    (propertize "  " 'face 'timu-line-active-face 'display `(raise -0.20)))

(defvar-local timu-line-spacer-three
    (propertize "  " 'face 'timu-line-active-face))

(dolist (vars '(timu-line-spacer-one
                timu-line-spacer-two
                timu-line-spacer-three))
  (put vars 'risky-local-variable t))


;;; FACES
(defface timu-line-background-active-face
  '((t nil))
  "Default background face for the active mode line."
  :group 'timu-line)

(defface timu-line-background-inactive-face
  '((t nil))
  "Default background face for the inactive mode line."
  :group 'timu-line)

(defface timu-line-active-face
  '((t :weight bold))
  "Default foreground face for the active mode line."
  :group 'timu-line)

(defface timu-line-inactive-face
  '((t nil))
  "Default foreground face for the inactive mode line."
  :group 'timu-line)

(defface timu-line-special-face
  '((t :inherit link :underline nil :weight bold))
  "Special foreground face for the mode line."
  :group 'timu-line)

(defface timu-line-fancy-face
  '((t :inherit success :weight bold))
  "Fancy foreground face for the mode line."
  :group 'timu-line)

(defface timu-line-status-face
  '((t :inherit mode-line-highlight :underline nil :weight bold))
  "Foreground face for status indicators in the mode line."
  :group 'timu-line)

(defface timu-line-modified-face
  '((t :inherit error :weight bold))
  "Face for the buffer name, when the buffer is modified."
  :group 'timu-line)

(defface timu-line-read-only-face
  '((t :inherit font-lock-comment-face :weight bold))
  "Face for the buffer name, when the buffer is read only."
  :group 'timu-line)

(defvar timu-line-selected-window nil
  "Variable to store the currently selected window.")


;;; FUNCTIONS
(defmacro timu-line-face-switcher (active-face inactive-face &rest body)
  "Set face according to the window state.
ACTIVE-FACE for the active window and INACTIVE-FACE for the inactive window.
The optional argument BODY is the string/code to propertize."
  `(let ((face (if (eq timu-line-selected-window
                       (get-buffer-window (current-buffer)))
                   ,active-face
                 ,inactive-face)))
     ,@body))

(defun timu-line-update-selected-window ()
  "Update selected window into `timu-line-selected-window'."
  (setq timu-line-selected-window (selected-window)))

(defun timu-line-get-buffer-name ()
  "Return the buffer name as a string."
  (timu-line-face-switcher
   'timu-line-special-face 'timu-line-inactive-face
   (propertize
    (cond
     (buffer-file-name
      (format " %s " (f-join (f-filename (f-dirname buffer-file-name))
                             (f-filename buffer-file-name))))
     ((memq major-mode timu-line-elfeed-modes)
      " *elfeed* ")
     (t
      (format " %s " (buffer-name))))
    'face face)))

(defun timu-line-get-buffer-name-status ()
  "Return buffer status (ro, rw or modified) and name."
  (with-current-buffer (or (buffer-base-buffer) (current-buffer))
    (if (and buffer-file-name (buffer-modified-p))
        (timu-line-face-switcher
         'timu-line-modified-face 'timu-line-inactive-face
         (propertize (timu-line-get-buffer-name) 'face face))
      (if buffer-read-only
          (timu-line-face-switcher
           'timu-line-read-only-face 'timu-line-inactive-face
           (propertize (timu-line-get-buffer-name) 'face face))
        (timu-line-face-switcher
         'timu-line-active-face 'timu-line-inactive-face
         (propertize (timu-line-get-buffer-name) 'face face))))))

(defun timu-line-get-org-capture-hints ()
  "Return a string with instruction for org capture."
  (timu-line-face-switcher
   'timu-line-fancy-face 'timu-line-inactive-face
   (propertize
    (if (bound-and-true-p org-capture-mode)
        "  | Finish: M-s | Refile: M-r | Cancel: M-w | "
      "")
    'face face)))

(defun timu-line-get-evil-state ()
  "Return the evil state as a propertized string."
  (let ((state "!")) ;; default value
    (when (bound-and-true-p evil-emacs-state-minor-mode) (setq state " e"))
    (when (bound-and-true-p evil-insert-state-minor-mode) (setq state " i"))
    (when (bound-and-true-p evil-motion-state-minor-mode) (setq state " m"))
    (when (bound-and-true-p evil-normal-state-minor-mode) (setq state " n"))
    (when (bound-and-true-p evil-operator-state-minor-mode) (setq state " o"))
    (when (bound-and-true-p evil-replace-state-minor-mode) (setq state " r"))
    (when (bound-and-true-p evil-visual-state-minor-mode) (setq state " v"))
    (timu-line-face-switcher
     'timu-line-special-face 'timu-line-inactive-face
     (propertize state 'face face))))

(defun timu-line-get-major-mode ()
  "Return current major mode name."
  (timu-line-face-switcher
   'timu-line-fancy-face 'timu-line-inactive-face
   (propertize
    (concat
     "  m:"
     (substring (symbol-name major-mode) 0 -5))
    'face face)))

(defun timu-line-get-vc-branch ()
  "Return current vc branch if in a repo."
  (timu-line-face-switcher
   'timu-line-special-face 'timu-line-inactive-face
   (propertize
    (if vc-mode
        (let ((backend (vc-backend buffer-file-name)))
          (concat "  b:"
                  (substring-no-properties vc-mode
                                           (+ (if (eq backend 'Hg) 2 3) 2))))
      "")
    'face face)))

(defun timu-line-lsp-string ()
  "Return and indicator for lsp mode as a propertized string."
  (timu-line-face-switcher
   'timu-line-fancy-face 'timu-line-inactive-face
   (propertize
    (if (bound-and-true-p lsp-mode)
        " l:lsp"
      "")
    'face face)))

(defun timu-line-eglot-string ()
  "Return and indicator for eglot mode as a propertized string."
  (timu-line-face-switcher
   'timu-line-fancy-face 'timu-line-inactive-face
   (propertize
    (if (bound-and-true-p eglot--managed-mode)
        " l:eglot"
      "")
    'face face)))

(defun timu-line-kbd-macro-p ()
  "String to display in the mode line when a Keyboard Macro is being recorded."
  (timu-line-face-switcher
   'timu-line-status-face 'timu-line-inactive-face
   (propertize
    (if defining-kbd-macro
        "k "
      "")
    'face face)))

(defun timu-line-get-python-virtual-env ()
  "Return python virtual env.
Information:
- Python Virtual Environment using `pyvenv-virtual-env-name'.
  The Venv is only shown when active."
  (timu-line-face-switcher
   'timu-line-special-face 'timu-line-inactive-face
   (propertize
    (if (eq major-mode 'python-mode)
        (let ((venv (if pyvenv-virtual-env-name
                        (format "  v:%s" pyvenv-virtual-env-name)
                      "")))
          venv)
      "")
    'face face)))

(defun timu-line-unread-email-count ()
  "Return the count of unread emails and a mail icon."
  (timu-line-face-switcher
   'timu-line-active-face 'timu-line-inactive-face
   (propertize
    (if (string-equal mu4e-alert-mode-line "")
        (format "")
      (concat
       " e:"
       (progn (string-match "[0-9]+" mu4e-alert-mode-line)
              (match-string 0 mu4e-alert-mode-line))
       ""))
    'face face)))

(defun timu-line-get-tab-number ()
  "Return the number of the current tab and the total numbers of tabs."
  (timu-line-face-switcher
   'timu-line-active-face 'timu-line-inactive-face
   (propertize
    (let* ((current-tab (tab-bar--current-tab))
           (tab-index (tab-bar--current-tab-index))
           (explicit-name (alist-get 'explicit-name current-tab))
           (tab-name (alist-get 'name current-tab)))
      (if explicit-name tab-name
        (format "  t:%s:%s" (+ 1 tab-index)
                (number-to-string
                 (length (frame-parameter nil 'tabs))))))
    'face face)))

(defun timu-line-get-position ()
  "Return column position of the point as a propertized string."
  (timu-line-face-switcher
   'timu-line-active-face 'timu-line-inactive-face
   (propertize
    (format "  c:%d" (current-column))
    'face face)))

(defun timu-line-mu4e-context-string ()
  "Extract context from `mu4e--search-last-query'."
  (if mu4e--search-last-query
      (if (string-match "/\\(.+?\\)/.*" mu4e--search-last-query)
          (match-string 1 mu4e--search-last-query) "")
    ""))

(defun timu-line-mu4e-context ()
  "Return the current mu4e context as a propertized string."
  (timu-line-face-switcher
   'timu-line-special-face 'timu-line-inactive-face
   (propertize
    (if (memq major-mode timu-line-mu4e-context-modes)
        (if (> (length (timu-line-mu4e-context-string)) 0)
            (concat " c:" (substring-no-properties
                           (timu-line-mu4e-context-string) 0 nil))
          "")
      "")
    'face face)))

(defun timu-line-elfeed-search-filter ()
  "Return the current elfeed search filter as a propertized string."
  (timu-line-face-switcher
   'timu-line-active-face 'timu-line-inactive-face
   (propertize
    (if (memq major-mode timu-line-elfeed-modes)
        (concat " " elfeed-search-filter)
      "")
    'face face)))

(defun timu-line-elfeed-article-counts ()
  "Return number of articles and feeds as propertized string.
Example: \"feeds:7 unread:42 total:42\"."
  (timu-line-face-switcher
   'timu-line-special-face 'timu-line-inactive-face
   (propertize
    (if (memq major-mode '(elfeed-search-mode))
        (let ((unread
               (car (split-string (elfeed-search--count-unread) "/")))
              (total
               (car (split-string
                     (cadr (split-string (elfeed-search--count-unread) "/")) ":")))
              (feeds
               (cadr (split-string
                      (cadr (split-string (elfeed-search--count-unread) "/")) ":"))))
          (concat "  feeds:" feeds " unread:" unread " total:" total))
      "")
    'face face)))

(defun timu-line-popper-indicator ()
  "Return the current mu4e context as a propertized string."
  (customize-set-variable 'popper-mode-line "")
  (timu-line-face-switcher
   'timu-line-status-face 'timu-line-inactive-face
   (if (bound-and-true-p popper-popup-status)
       (propertize " p" 'face face)
     "")))

(defun timu-line-front-space ()
  "Space to add to the front of the mode line content.
This is the same as the default value of the `mode-line-format'."
  (if
      (display-graphic-p)
      " " "-"))

(defun timu-line-end-space ()
  "Space to add to the end of the mode line content.
This is the same as the default value of the `mode-line-format'."
  (unless
      (display-graphic-p)
    "-%-"))

(defun timu-line-default-mode-line ()
  "Set the `mode-line-format' to the default Emacs value."
  (setq mode-line-format nil)
  (kill-local-variable 'mode-line-format)
  (force-mode-line-update)
  (setq-default mode-line-format
                '("%e" mode-line-front-space
                  mode-line-mule-info
                  mode-line-client
                  mode-line-modified
                  mode-line-remote
                  mode-line-frame-identification
                  mode-line-buffer-identification
                  "   "
                  mode-line-position
                  (vc-mode vc-mode)
                  "  "
                  mode-line-modes
                  mode-line-misc-info
                  mode-line-end-spaces)))

(defun timu-line-render (left right)
  "Correct spacing for the mode line.
Return a string of `window-width' length containing LEFT, and RIGHT
 aligned respectively."
  (let* ((available-width (- (window-width) (length left) 2)))
    (format (format " %%s %%%ds " available-width) left right)))

(defun timu-line-activate-mode-line ()
  "Set the `mode-line-format' to the custom value of the `timu-line-mode'."
  (add-hook 'post-command-hook #'timu-line-update-selected-window)
  (customize-set-variable 'mode-line-position-column-line-format '(" %c "))
  (customize-set-variable 'mode-line-percent-position nil)
  (customize-set-variable 'evil-mode-line-format nil)
  (setq mode-line-format nil)
  (kill-local-variable 'mode-line-format)
  (force-mode-line-update)
  (setq-default mode-line-format
                '((:eval (timu-line-render
                          ;; left
                          (format-mode-line
                           (concat
                            (timu-line-front-space)
                            (timu-line-kbd-macro-p)
                            (if timu-line-show-evil-state
                                (timu-line-get-evil-state)
                              "")
                            (timu-line-get-buffer-name-status)
                            (if timu-line-show-vc-branch
                                (timu-line-get-vc-branch)
                              "")
                            (if timu-line-show-python-virtual-env
                                (timu-line-get-python-virtual-env)
                              "")
                            (if timu-line-show-org-capture-hints
                                (timu-line-get-org-capture-hints)
                              "")
                            (if timu-line-show-mu4e-context
                                (timu-line-mu4e-context)
                              "")
                            (timu-line-elfeed-search-filter)
                            (if timu-line-show-elfeed-counts
                                (timu-line-elfeed-article-counts)
                              "")
                            timu-line-spacer-one))
                          ;; right
                          (format-mode-line
                           (concat
                            (if timu-line-show-lsp-indicator
                                (timu-line-lsp-string)
                              "")
                            (if timu-line-show-eglot-indicator
                                (timu-line-eglot-string)
                              "")
                            (timu-line-get-major-mode)
                            timu-line-spacer-two
                            (timu-line-unread-email-count)
                            (timu-line-get-tab-number)
                            (timu-line-get-position)
                            (timu-line-popper-indicator)
                            (timu-line-end-space))))))))

;;;###autoload
(define-minor-mode timu-line-mode
  "Toggle `timu-line-mode' minor mode."
  :group 'timu-line
  :global t
  :init-value nil
  (if timu-line-mode
      (timu-line-activate-mode-line)
    (timu-line-default-mode-line)))


(provide 'timu-line)


;;; timu-line.el ends here
