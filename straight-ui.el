;;; straight-ui.el --- package UI for straight.el  -*- lexical-binding: t; -*-
;; -*- lexical-binding: t; -*-

;;; Commentary:

(require 'straight)
(require 'url)
(require 'tabulated-list)
(require 'button)

;;; Code:
(defface straight-ui-package
  '((default :inherit default))
  "Default face for packages."
  :group 'straight-faces)

(defface straight-ui-marked-package
  '((default :inherit default :weight bold :foreground "pink"))
  "Face for marked packages."
  :group 'straight-faces)

(defface straight-ui-marked-install
  '((default :inherit default :weight bold :foreground "#89cff0"))
  "Face for marked packages."
  :group 'straight-faces)

(defface straight-ui-package-installed
  '((default :weight bold :foreground "orange"))
  "Default face for packages."
  :group 'straight-faces)

(defvar straight-ui-buffer "* straight-ui *")
(defvar straight-ui-melpa-metadata-cache nil)

(defvar url-http-end-of-headers)
(defun straight-ui-melpa-recipes (&optional refresh)
  "Return a list of all MELPA packages of form:.
Optional argument REFRESH bypasses `straight-ui-melpa-cache'."
  (if (and straight-ui-melpa-metadata-cache
           (not refresh))
      straight-ui-melpa-metadata-cache
    (setq straight-ui-melpa-metadata-cache
          (with-current-buffer (url-retrieve-synchronously "https://melpa.org/archive.json")
            (goto-char url-http-end-of-headers)
            ;;@COMPAT: Emacs<28
            (json-read)))))

;;(straight-ui-melpa-recipes 'refresh)

;;@INCOMPLETE: what do we consider "installed?"
;; We could be more fine grained and offer info based on whether a package has
;; been cloned, built, etc. is explicitly declared in init file
(defun straight-ui-package-installed-p (package)
  "Return t if PACKAGE is installed, else nil."
  (cl-some (lambda (installed) (string= package installed))
           (hash-table-keys straight--build-cache)))

(defun straight-ui-package-link (package)
  "Return propertized PACKAGE string."
  (let ((installed (straight-ui-package-installed-p package)))
    (apply #'propertize
           (delq nil
                 `(,package
                   ,@(when installed '(mouse-face highlight))
                   help-echo "testing"
                   face ,(if installed
                             'straight-ui-package-installed
                           'straight-ui-package))))))

(defun straight-ui-package-info (package)
  "Return string for PACKAGE info."
  package)

(defvar straight-ui-melpa-list-cache nil
  "Cache of MELPA package entries.")
(defun straight-ui-melpa-list (&optional refresh)
  "Return a list of melpa packages for UI.
If REFRESH is non-nil, bypass `straight-ui-melpa-list-cache' cache."
  (if (and straight-ui-melpa-list-cache (not refresh))
      straight-ui-melpa-list-cache
    (setq straight-ui-melpa-list-cache
          (mapcar (lambda (recipe)
                    (let* ((package (car recipe))
                           (metadata (cdr recipe))
                           (description (alist-get 'desc metadata ""))
                           (properties (alist-get 'props metadata))
                           (url (alist-get 'url properties "")))
                      `(,package ;;ID
                        [,(straight-ui-package-link (symbol-name package))
                         ,description
                         ,url])))
                  (straight-ui-melpa-recipes)))))

(defun straight--ui-init ()
  "Initialize format of the UI."
  (setq tabulated-list-format
        [("Package" 20 t)
         ("Description" 60 t)
         ("URL" 40 t)]
        tabulated-list-entries
        #'straight-ui-melpa-list))

(defun straight--ui-installed ()
  "Return a list of installed packages."
  (cl-remove-if-not (lambda (entry)
                      (straight-ui-package-installed-p (aref (cadr entry) 0)))
                    (straight-ui-melpa-list)))

(defvar straight-ui-show-installed nil
  "When non-nil only show installed packages.")

(defun straight-ui-show-installed ()
  "Show only installed packages.
Toggle all if already filtered."
  (interactive)
  (message "Showing %S packages"
           (if straight-ui-show-installed "all" "installed"))
  (setq tabulated-list-entries (if straight-ui-show-installed
                                   #'straight-ui-melpa-list
                                 #'straight--ui-installed)
        straight-ui-show-installed (not straight-ui-show-installed))
  (tabulated-list-print 'remember-pos 'update))

(defvar straight-ui-mode-map (make-sparse-keymap)
  "Keymap for `straight-ui-mode'.")
;;@REMOVE: This is just for my own convenience while testing.
(evil-make-intercept-map straight-ui-mode-map)
(define-derived-mode straight-ui-mode tabulated-list-mode "straight-ui"
  "Major mode to manage packages."
  (straight--ui-init)
  (tabulated-list-init-header)
  (tabulated-list-print))

;;;###autoload
(defun straight-ui ()
  "Display the UI."
  (interactive)
  (with-current-buffer (get-buffer-create straight-ui-buffer)
    (unless (derived-mode-p 'straight-ui-mode)
      (straight-ui-mode))
    (switch-to-buffer straight-ui-buffer)))

(defvar straight-ui-search-filter nil
  "Filter for package seraches.")
(defvar straight-ui-search-filter-active nil
  "Whether or not a search filter is in progress.")

(defun straight-ui--minibuffer-setup ()
  "Set up the minibuffer for live filtering."
  (when straight-ui-search-filter-active
    ;;@INCOMPLETE: implement search operator syntax
    ;;(set-syntax-table straight-ui-search-filter-syntax-table)
    (when straight-ui-search-filter-active
      (add-hook 'post-command-hook 'straight-ui--update-search-filter nil :local))))

(add-hook 'minibuffer-setup-hook 'straight-ui--minibuffer-setup)


(defcustom straight-ui-search-debounce-interval 0.5
  "Length of time to wait before updating the search UI.
See `run-at-time' for acceptable values."
  :group 'straight
  :type (or 'string 'int 'float))

(defvar straight--ui-search-timer nil
  "Timer to debounce search input.")

(defun straight-ui--update-search-filter ()
  "Update the UI to reflect search input."
  (when-let ((search-active straight-ui-search-filter-active)
             (buffer straight-ui-buffer)
             (query (minibuffer-contents-no-properties)))
    (unless (string-empty-p query)
      (with-current-buffer buffer
        (setq tabulated-list-entries
              (cl-remove-if-not
               (lambda (entry)
                 (cl-some (lambda (el) (ignore-errors (string-match-p query el)))
                          (butlast (cl-coerce (cadr entry) 'list))))
               (straight-ui-melpa-list)))
        (tabulated-list-print 'remember-pos 'update)))))

(defun straight-ui--debounce-search ()
  "Update filter from minibuffer."
  (if straight--ui-search-timer
      (cancel-timer straight--ui-search-timer))
  (setq straight--ui-search-timer
        (run-at-time straight-ui-search-debounce-interval
                     nil
                     #'straight-ui--update-search-filter)))

(defun straight-ui-search (&optional edit)
  "Filter current buffer by string.
If EDIT is non-nil, edit the last search."
  (interactive)
  (unwind-protect
      (setq straight-ui-search-filter-active t
            straight-ui-search-filter
            (read-from-minibuffer "Search: " (when edit straight-ui-search-filter)))
    (setq straight-ui-search-filter-active nil)
    (tabulated-list-print 'remember-pos 'update)))

(defun straight-ui-search-edit ()
  "Edit last search."
  (interactive)
  (straight-ui-search 'edit))

(defun straight-ui-package-info-page ()
  "Display general info for package on current line."
  (interactive)
  (save-excursion
    (end-of-line)
    (browse-url-at-point)))

(defvar straight-ui--marked-packages nil
  "List of marked packages.
Each element is a cons of (PACKAGE . ACTION)")

(defun straight-ui-package-marked-p (package)
  "Return t if PACKAGE is marked."
  (and (member package (mapcar #'car straight-ui--marked-packages)) t))

(defun straight-ui-unmark (package)
  "Unmark PACKAGE."
  (interactive)
  (setq straight-ui--marked-packages
        (cl-remove-if (lambda (cell) (string= (car cell) package))
                      straight-ui--marked-packages))
  (with-silent-modifications
    (put-text-property (line-beginning-position) (line-end-position)
                       'display nil))
  (forward-line))

(defun straight-ui-mark (package &optional action face)
  "Mark PACKAGE for ACTION.
ACTION is a function which will be called.
It is passed the name of the package as its sole argument.
If FACE is non-nil, use that instead of `straight-ui-marked-package'."
  (interactive)
  (cl-pushnew (cons package action) straight-ui--marked-packages
              :test (lambda (a b) (string= (car a) (car b))))
  (with-silent-modifications
    (put-text-property (line-beginning-position) (line-end-position)
                       'display
                       (propertize (concat "* " (string-trim (thing-at-point 'line)))
                                   'face
                                   (or face 'straight-ui-marked-package)))
    (forward-line)))

(defun straight-ui-current-package ()
  "Return current package of UI line as a string."
  (or (symbol-name (get-text-property (point) 'tabulated-list-id))
      (user-error "No package found at point")))

(defun straight-ui-toggle-mark (&optional action face)
  "Toggle ACTION mark for current package."
  (interactive)
  (let ((package (straight-ui-current-package)))
    (if (straight-ui-package-marked-p package)
        (straight-ui-unmark package)
      (straight-ui-mark package action face))))

(defun straight-ui-mark-install ()
  "Mark package for installation."
  (interactive)
  (straight-ui-toggle-mark
   (lambda (p) (straight-use-package (intern p)))
   'straight-ui-marked-install))

(defun straight-ui-execute-marks ()
  "Execute each action in `straight-ui-marked-packages'."
  (interactive)
  (mapc (lambda (marked)
          (if-let ((action (cdr marked)))
              ;;@INCOMPLETE:
              ;; We need to clean up (visually unmark, remove package from marked list)
              ;; What should we do if this fails?
              (funcall action (car marked))
            (message "@TODO: implement generic mark prompt.")))
        straight-ui--marked-packages))

;;;; Key bindings
            (define-key straight-ui-mode-map (kbd "I") 'straight-ui-show-installed)
            (define-key straight-ui-mode-map (kbd "i") 'straight-ui-mark-install)
            (define-key straight-ui-mode-map (kbd "s") 'straight-ui-search)
            (define-key straight-ui-mode-map (kbd "S") 'straight-ui-search-edit)
            (define-key straight-ui-mode-map (kbd "x") 'straight-ui-execute-marks)
            (define-key straight-ui-mode-map (kbd "*") 'straight-ui-toggle-mark)
            (define-key straight-ui-mode-map (kbd "RET") 'straight-ui-package-info-page)

;;;; TESTS
            ;;(straight-ui)
            ;;(straight-ui-melpa-list)

            (provide 'straight-ui)

;;; straight-ui.el ends here
