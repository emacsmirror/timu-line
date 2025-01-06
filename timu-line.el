;;; timu-line.el --- Custom and simple mode line -*- lexical-binding: t; -*-

;; Author: Aimé Bertrand <aime.bertrand@macowners.club>
;; Version: 1.0
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

;; I.  Installation
;;   A. Manual installation
;;     1. Add `timu-line.el' (https://gitlab.com/aimebertrand/timu-line)
;;        to your `custom-load-path'.
;;     2. In your `~/.emacs.d/init.el' or `~/.emacs':
;;         (timu-line-mode 1)
;;
;;   B. From Melpa
;;     1. M-x package-install RET timu-line RET.
;;     2. In your `~/.emacs.d/init.el' or `~/.emacs':
;;         (timu-line-mode 1)
;;
;;   C. With use-package
;;     In your `~/.emacs.d/init.el' or `~/.emacs':
;;       (use-package timu-line
;;         :ensure t
;;         :config
;;         (timu-line-mode 1))
;;
;; II.  Features
;;     The following is displayed in appropriate buffers only:
;;
;;   A. Left side
;;      - Display hint when a kbd macro is running
;;      - Display evil state
;;      - Display Tramp host if applicable
;;      - Display buffer/file name
;;      - Display keybindings for org capture in the capture buffer
;;      - Display the vc branch
;;      - Display the python venv
;;      - Display Mu4e context
;;      - Display Elfeed search filter
;;      - Display Elfeed article counts
;;      - Display Monkeytype Stats
;;
;;   B. Right side
;;      - Display the major mode
;;      - Display Mu4e mail count (works only with `mu' installed)
;;      - Display tab number `current:total'
;;      - Display column number of the point
;;
;; III.  Options
;;   A. Faces
;;      The following faces can be set to ones liking.
;;      Either by the theme or with `set-face-attribute'.
;;
;;      - `timu-line-bg-active-face'
;;      - `timu-line-bg-inactive-face'
;;      - `timu-line-active-face'
;;      - `timu-line-inactive-face'
;;      - `timu-line-special-face'
;;      - `timu-line-fancy-face'
;;      - `timu-line-status-face'
;;      - `timu-line-modified-face'
;;      - `timu-line-read-only-face'
;;
;;   B. Control section display
;;      You can elect to display some sections or not by using the
;;      following variables:
;;
;;      - `timu-line-show-vc-branch' - default value is t
;;      - `timu-line-show-lsp-indicator' - default value is nil
;;      - `timu-line-show-eglot-indicator' - default value is nil
;;      - `timu-line-show-python-virtual-env' - default value is t
;;      - `timu-line-show-org-capture-keys' - default value is t
;;      - `timu-line-show-mu4e-context' - default value is t
;;      - `timu-line-show-elfeed-counts' - default value is t
;;      - `timu-line-show-monkeytype-stats' - default value is nil
;;      - `timu-line-show-evil-state' - default value is nil
;;      - `timu-line-show-tramp-host' - default value is nil
;;
;;   C. Org capture hints for keybindings
;;      The variable `timu-line-org-capture-keys-string' contains the string to
;;      show in the mode line as keybindings hint in the org capture buffer.
;;
;;   D. Modes for mu4e context
;;      `timu-line-mu4e-context-modes' is a custom variable containing a list of
;;      major modes in which to display the Mu4e context in the mode line.
;;
;;   E. Modes for mu4e context
;;      `timu-line-elfeed-modes' controls in which modes the custom
;;      Elfeed string is displayed.


;;; Code:


;;; DEFAULTS
(defgroup timu-line ()
  "Customise group for the `timu-line' Library."
  :group 'environment)


;;; VARIABLES
(defvar timu-line-selected-window nil
  "Variable to store the currently selected window.")


;;; CUSTOMIZABLE VARIABLE
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
  "Control whether to show the vc branch in the mode line.
This is set to \"t\" by default."
  :type 'boolean
  :group 'timu-line)

(defcustom timu-line-show-lsp-indicator nil
  "Control whether to show an lsp indicator in the mode line.
This is set to \"nil\" by default."
  :type 'boolean
  :group 'timu-line)

(defcustom timu-line-show-eglot-indicator nil
  "Control whether to show an eglot indicator in the mode line.
This is set to \"nil\" by default."
  :type 'boolean
  :group 'timu-line)

(defcustom timu-line-show-python-virtual-env t
  "Control whether to show the python venv in the mode line.
This is set to \"t\" by default."
  :type 'boolean
  :group 'timu-line)

(defcustom timu-line-show-org-capture-keys t
  "Control whether to show the org capture keybindings in the mode line.
This is set to \"t\" by default."
  :type 'boolean
  :group 'timu-line)

(defcustom timu-line-org-capture-keys-string
  "| Finish: M-s | Refile: M-r | Cancel: M-w |"
  "The string to show as org capture keybindings."
  :type 'boolean
  :group 'timu-line)

(defcustom timu-line-show-mu4e-context t
  "Control whether to show the mu4e context in the mode line.
This is set to \"t\" by default."
  :type 'boolean
  :group 'timu-line)

(defcustom timu-line-show-elfeed-counts t
  "Control whether to show elfeed counts in the mode line.
This is set to \"t\" by default."
  :type 'boolean
  :group 'timu-line)

(defcustom timu-line-show-monkeytype-stats nil
  "Control whether to show `monkeytype' stats in the mode line.
This is set to \"nil\" by default."
  :type 'boolean
  :group 'timu-line)

(defcustom timu-line-show-evil-state nil
  "Control whether to show the evil state in the mode line.
This is set to \"nil\" by default."
  :type 'boolean
  :group 'timu-line)

(defcustom timu-line-show-tramp-host nil
  "Control whether to show the tramp host in the mode line.
This is set to \"nil\" by default."
  :type 'boolean
  :group 'timu-line)


;;; BUFFER-LOCAL VARIABLES
(defvar-local timu-line-spacer-top
    (propertize "  " 'face 'timu-line-active-face 'display `(raise +0.20)))

(defvar-local timu-line-spacer-bottom
    (propertize "  " 'face 'timu-line-active-face 'display `(raise -0.20)))

(dolist (vars '(timu-line-spacer-top
                timu-line-spacer-bottom))
  (put vars 'risky-local-variable t))


;;; FACES
(defface timu-line-bg-active-face
  '((t nil))
  "Default background face for the active mode line."
  :group 'timu-line)

(defface timu-line-bg-inactive-face
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


;;; FUNCTIONS
;;;; for byte-comp
(declare-function elfeed-search--count-unread "ext:elfeed-search" t t )
(declare-function evil-visual-state-p "ext:evil-states" t t)
(declare-function evil-replace-state-p "ext:evil-states" t t)
(declare-function evil-operator-state-p "ext:evil-states" t t)
(declare-function evil-normal-state-p "ext:evil-states" t t)
(declare-function evil-motion-state-p "ext:evil-states" t t)
(declare-function evil-insert-state-p "ext:evil-states" t t)
(declare-function evil-emacs-state-p "ext:evil-states" t t)
(declare-function tramp-tramp-file-p "tramp" t t)

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

(defun timu-line-get-tramp-host (tramp-default-directory)
  "Extract host from TRAMP-DEFAULT-DIRECTORY."
  (when (string-match "/[^:]+:\\([^:]+\\):" tramp-default-directory)
    (match-string 1 tramp-default-directory)))

(defun timu-line-tramp-root-directory-p (path)
  "Check if PATH is at the root of a tramp filesystem."
  (let ((tramp-patterns '("^/\\([^:]+\\):\\([^@:]+\\):/$"
                          "^/\\([^:]+\\):\\([^@]+\\)@\\([^:]+\\):/$")))
    (catch 'found
      (dolist (pattern tramp-patterns)
        (when (string-match-p pattern path)
          (throw 'found t)))
      nil)))

(defun timu-line-tramp-host-string ()
  "Return the tramp host as propertized string."
  (timu-line-face-switcher
   'timu-line-special-face 'timu-line-inactive-face
   (propertize
    (if timu-line-show-tramp-host
        (if (tramp-tramp-file-p default-directory)
            (format " t:%s " (timu-line-get-tramp-host default-directory))
          "")
      "")
    'face face)))

(defun timu-line-get-short-file-path ()
  "Return the file's \"parent-directory/filename\" as a string."
  (concat
   (file-name-nondirectory
    (directory-file-name
     (file-name-parent-directory buffer-file-name)))
   "/"
   (file-name-nondirectory
    (directory-file-name buffer-file-name))))

(defun timu-line-get-short-dired-path ()
  "Return the Dired's \"parent-directory/directory-name\" or \"/\" as a string.
The value is \"/\" when `dired-directory' is at the root of the files system."
  (cond
   ((or (equal "/" dired-directory) (equal "/../" dired-directory))
    "/")
   ((timu-line-tramp-root-directory-p default-directory)
    "/")
   (t
    (concat
     (file-name-nondirectory
      (directory-file-name
       (file-name-parent-directory dired-directory)))
     "/"
     (if (string-suffix-p "~/" dired-directory)
         (buffer-name)
       (file-name-nondirectory
        (directory-file-name dired-directory)))))))

(defun timu-line-get-buffer-name ()
  "Return the buffer name as a string."
  (timu-line-face-switcher
   'timu-line-special-face 'timu-line-inactive-face
   (propertize
    (cond
     ((memq major-mode timu-line-elfeed-modes)
      " *elfeed* ")
     ((derived-mode-p 'helpful-mode)
      " *helpful* ")
     ((string-prefix-p "*Embark Export:" (buffer-name))
      (format " %s " (buffer-name)))
     (buffer-file-name
      (format " %s " (timu-line-get-short-file-path)))
     ((derived-mode-p 'dired-mode)
      (format " %s " (timu-line-get-short-dired-path)))
     (t
      (format " %s " (buffer-name))))
    'face face)))

(defun timu-line-buffer-name ()
  "Return buffer name as a propertized string."
  (with-current-buffer (or (buffer-base-buffer) (current-buffer))
    (if (and buffer-file-name (buffer-modified-p))
        (timu-line-face-switcher
         'timu-line-modified-face 'timu-line-inactive-face
         (propertize (timu-line-get-buffer-name) 'face face))
      (timu-line-face-switcher
       'timu-line-active-face 'timu-line-inactive-face
       (propertize (timu-line-get-buffer-name) 'face face)))))

(defun timu-line-org-capture-keys ()
  "Return keybindings instruction for org capture as a propertized string."
  (timu-line-face-switcher
   'timu-line-fancy-face 'timu-line-inactive-face
   (propertize
    (if timu-line-show-org-capture-keys
        (if (bound-and-true-p org-capture-mode)
            (concat "  " timu-line-org-capture-keys-string " ")
          "")
      "")
    'face face)))

(defun timu-line-evil-state ()
  "Return the evil state as a propertized string."
  (if (featurep 'evil)
      (let ((state "!")) ;; default value
        (when (evil-emacs-state-p) (setq state " e"))
        (when (evil-insert-state-p) (setq state " i"))
        (when (evil-motion-state-p) (setq state " m"))
        (when (evil-normal-state-p) (setq state " n"))
        (when (evil-operator-state-p) (setq state " o"))
        (when (evil-replace-state-p) (setq state " r"))
        (when (evil-visual-state-p) (setq state " v"))
        (timu-line-face-switcher
         'timu-line-fancy-face 'timu-line-inactive-face
         (if timu-line-show-evil-state
             (propertize state 'face face)
           "")))
    ""))

(defun timu-line-major-mode ()
  "Return current major mode name as a propertized string."
  (timu-line-face-switcher
   'timu-line-fancy-face 'timu-line-inactive-face
   (propertize
    (concat
     "  m:"
     (substring (symbol-name major-mode) 0 -5))
    'face face)))

(defun timu-line-vc-branch ()
  "Return current vc branch if in a repo as a propertized string."
  (timu-line-face-switcher
   'timu-line-special-face 'timu-line-inactive-face
   (propertize
    (if timu-line-show-vc-branch
        (if vc-mode
            (let ((backend (vc-backend buffer-file-name)))
              (concat "  b:"
                      (substring-no-properties
                       vc-mode
                       (+ (if (eq backend 'Hg) 2 3) 2))))
          "")
      "")
    'face face)))

(defun timu-line-read-only-state ()
  "Return a string if the buffer is read only."
  (with-current-buffer (or (buffer-base-buffer) (current-buffer))
    (if buffer-read-only
        (timu-line-face-switcher
         'timu-line-special-face 'timu-line-inactive-face
         (propertize " ro  " 'face face)))))

(defun timu-line-lsp-string ()
  "Return and indicator for lsp mode as a propertized string."
  (timu-line-face-switcher
   'timu-line-fancy-face 'timu-line-inactive-face
   (propertize
    (if timu-line-show-lsp-indicator
        (if (bound-and-true-p lsp-mode)
            " l:lsp"
          "")
      "")
    'face face)))

(defun timu-line-eglot-string ()
  "Return and indicator for eglot mode as a propertized string."
  (timu-line-face-switcher
   'timu-line-fancy-face 'timu-line-inactive-face
   (propertize
    (if timu-line-show-eglot-indicator
        (if (bound-and-true-p eglot--managed-mode)
            " l:eglot"
          "")
      "")
    'face face)))

(defun timu-line-kbd-macro-p ()
  "Propertized String to display when a Keyboard Macro is being recorded."
  (timu-line-face-switcher
   'timu-line-status-face 'timu-line-inactive-face
   (propertize
    (if defining-kbd-macro
        "k "
      "")
    'face face)))

(defun timu-line-python-virtual-env ()
  "Return python virtual env.
Information:
- Python Virtual Environment using `pyvenv-virtual-env-name'.
  The Venv is only shown when active."
  (timu-line-face-switcher
   'timu-line-special-face 'timu-line-inactive-face
   (propertize
    (if timu-line-show-python-virtual-env
        (if (eq major-mode 'python-mode)
            (let ((venv (if (bound-and-true-p pyvenv-virtual-env-name)
                            (format "  v:%s" pyvenv-virtual-env-name)
                          "")))
              venv)
          "")
      "")
    'face face)))

(defun timu-line-unread-mu4e-count ()
  "Return the count of unread emails as a propertized string.
This works only with \"mu\" so far."
  (timu-line-face-switcher
   'timu-line-active-face 'timu-line-inactive-face
   (propertize
    (if (executable-find "mu")
        (concat " e:"
                (shell-command-to-string
                 "printf $(mu find flag:unread 2> /dev/null | wc -l)"))
      "")
    'face face)))

(defun timu-line-tab-number ()
  "Return current tab number & total numbers of tabs as a propertized string."
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

(defun timu-line-position ()
  "Return column position of the point as a propertized string."
  (timu-line-face-switcher
   'timu-line-active-face 'timu-line-inactive-face
   (propertize
    (format "  c:%d" (current-column))
    'face face)))

(defun timu-line-mu4e-context-string ()
  "Extract context from `mu4e--search-last-query'."
  (if (bound-and-true-p mu4e--search-last-query)
      (if (string-match "/\\(.+?\\)/.*" mu4e--search-last-query)
          (match-string 1 mu4e--search-last-query) "")
    ""))

(defun timu-line-mu4e-context ()
  "Return the current mu4e context as a propertized string."
  (timu-line-face-switcher
   'timu-line-special-face 'timu-line-inactive-face
   (propertize
    (if timu-line-show-mu4e-context
        (if (memq major-mode timu-line-mu4e-context-modes)
            (if (> (length (timu-line-mu4e-context-string)) 0)
                (concat " c:" (substring-no-properties
                               (timu-line-mu4e-context-string) 0 nil))
              "")
          "")
      "")
    'face face)))

(defun timu-line-elfeed-search-filter ()
  "Return the current elfeed search filter as a propertized string."
  (timu-line-face-switcher
   'timu-line-active-face 'timu-line-inactive-face
   (propertize
    (if (bound-and-true-p elfeed-search-filter)
        (if (memq major-mode timu-line-elfeed-modes)
            (concat " " elfeed-search-filter)
          "")
      "")
    'face face)))

(defun timu-line-get-elfeed-article-counts ()
  "Extract number of articles and feeds.
Example: \"feeds:7 unread:42 total:42\"."
  (if (featurep 'elfeed)
      (if (memq major-mode '(elfeed-search-mode))
          (let ((unread
                 (car
                  (split-string (elfeed-search--count-unread) "/")))
                (total
                 (car
                  (split-string
                   (cadr
                    (split-string (elfeed-search--count-unread) "/")) ":")))
                (feeds
                 (cadr
                  (split-string
                   (cadr
                    (split-string (elfeed-search--count-unread) "/")) ":"))))
            (concat "  feeds:" feeds " unread:" unread " total:" total))
        "")
    ""))

(defun timu-line-elfeed-article-counts ()
  "Return number of articles and feeds as propertized string."
  (timu-line-face-switcher
   'timu-line-special-face 'timu-line-inactive-face
   (propertize
    (if timu-line-show-elfeed-counts
        (timu-line-get-elfeed-article-counts)
      "")
    'face face)))

(defun timu-line-popper-indicator ()
  "Return an popper indicator as a propertized string."
  (setq popper-mode-line "")
  (timu-line-face-switcher
   'timu-line-fancy-face 'timu-line-inactive-face
   (if (bound-and-true-p popper-popup-status)
       (propertize " p" 'face face)
     "")))

(defun timu-line-monkeytype-stats ()
  "Return `monkeytype' stats as a propertized string."
  (timu-line-face-switcher
   'timu-line-special-face 'timu-line-inactive-face
   (if (member 'monkeytype-mode local-minor-modes)
       (if timu-line-show-monkeytype-stats
           (propertize
            (let ((monkeytype-colored-mode-line nil))
              (monkeytype--mode-line-text))
            'face face)
         "")
     "")))

(defun timu-line-front-space ()
  "Space to add to the front of the mode line content.
This is the same as the default value of the `mode-line-format'."
  (if (display-graphic-p)
      " " "-"))

(defun timu-line-end-space ()
  "Space to add to the end of the mode line content.
This is the same as the default value of the `mode-line-format'."
  (unless (display-graphic-p)
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
                            (timu-line-evil-state)
                            (timu-line-tramp-host-string)
                            (timu-line-buffer-name)
                            (timu-line-read-only-state)
                            (timu-line-vc-branch)
                            (timu-line-python-virtual-env)
                            (timu-line-org-capture-keys)
                            (timu-line-mu4e-context)
                            (timu-line-elfeed-search-filter)
                            (timu-line-elfeed-article-counts)
                            (timu-line-monkeytype-stats)
                            timu-line-spacer-top))
                          ;; right
                          (format-mode-line
                           (concat
                            (timu-line-lsp-string)
                            (timu-line-eglot-string)
                            (timu-line-major-mode)
                            timu-line-spacer-bottom
                            (timu-line-unread-mu4e-count)
                            (timu-line-tab-number)
                            (timu-line-position)
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
