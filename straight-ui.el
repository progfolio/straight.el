;;; straight-ui.el --- package UI for straight.el  -*- lexical-binding: t; -*-
;; -*- lexical-binding: t; -*-

;;; Commentary:

(require 'straight)
(require 'url)
(require 'tabulated-list)
(require 'magit)
(require 'button)

;;; Code:
;;;;Faces:
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

(defgroup straight-ui nil
  "Straight's UI options."
  :group 'striaght-ui
  :prefix "straight-ui-")

;;;; Customizations:
(defcustom straight-ui-search-debounce-interval 0.5
  "Length of time to wait before updating the search UI.
See `run-at-time' for acceptable values."
  :group 'straight-ui
  :type (or 'string 'int 'float))

(defcustom straight-ui-install-prefix "⚙ "
  "Prefix for packages marked for deferred installation."
  :type 'string
  :group 'straight-ui)

;;;; Variables:
(defvar straight-ui--search-timer nil "Timer to debounce search input.")
(defvar straight-ui--current-package nil "Package currently being viewed in package info buffer.")
(defvar straight-ui--marked-packages nil "List of marked packages. Each element is a cons of (PACKAGE . ACTION).")
(defvar straight-ui-buffer "* straight-ui *")
(defvar straight-ui-melpa-list-cache nil "Cache of MELPA package entries.")
(defvar straight-ui-melpa-metadata-cache nil)
(defvar straight-ui-mode-map (make-sparse-keymap) "Keymap for `straight-ui-mode'.")
(defvar straight-ui-package-info-buffer "*straight-package-info*" "Buffer name for package info.")
(defvar straight-ui-package-info-mode-map (make-sparse-keymap))
(defvar straight-ui-search-filter nil "Filter for package seraches.")
(defvar straight-ui-search-filter-active nil "Whether or not a search filter is in progress.")
(defvar straight-ui-show-installed nil "When non-nil only show installed packages.")
(defvar straight-ui--package-info-history nil "List of visited packages.")
(defvar url-http-end-of-headers)

;;;; Functions:
(defun straight-ui-melpa-recipes (&optional refresh)
  "Return a list of all MELPA packages of form:.
Optional argument REFRESH bypasses `straight-ui-melpa-cache'."
  (if (and straight-ui-melpa-metadata-cache
           (not refresh))
      straight-ui-melpa-metadata-cache
    (setq straight-ui-melpa-metadata-cache
          (with-current-buffer (url-retrieve-synchronously
                                "https://melpa.org/archive.json")
            (goto-char url-http-end-of-headers)
            ;;@COMPAT: Emacs<28
            (json-read)))))

;;(straight-ui-melpa-recipes 'refresh)
;;(straight-ui--package-name "ace-window")

(defun straight-ui--package-name (package)
  "Return propertized PACKAGE string."
  (let ((installed (straight--installed-p (gethash package straight--recipe-cache))))
    (apply #'propertize
           (delq nil
                 `(,package
                   ,@(when installed '(mouse-face highlight))
                   help-echo "click to see package info"
                   face ,(if installed
                             'straight-ui-package-installed
                           'straight-ui-package))))))

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
                        [,(straight-ui--package-name (symbol-name package))
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
                      (straight--installed-p
                       (gethash (aref (cadr entry) 0) straight--recipe-cache)))
                    (straight-ui-melpa-list)))


(defun straight-ui-show-installed ()
  "Show only installed packages.
Toggle all if already filtered."
  (interactive)
  (setq straight-ui-search-filter nil)
  (message "Showing %S packages"
           (if straight-ui-show-installed "all" "installed"))
  (setq tabulated-list-entries (if straight-ui-show-installed
                                   #'straight-ui-melpa-list
                                 #'straight--ui-installed)
        straight-ui-show-installed (not straight-ui-show-installed))
  (tabulated-list-print 'remember-pos 'update))


;;@REMOVE: This is just for my own convenience while testing.
(declare-function evil-make-intercept-map "evil")
(evil-make-intercept-map straight-ui-mode-map)

(define-derived-mode straight-ui-mode tabulated-list-mode "straight-ui"
  "Major mode to manage packages."
  (straight--ui-init)
  (tabulated-list-init-header)
  (tabulated-list-print 'remember-pos 'update))

;;;###autoload
(defun straight-ui ()
  "Display the UI."
  (interactive)
  (with-current-buffer (get-buffer-create straight-ui-buffer)
    (unless (derived-mode-p 'straight-ui-mode)
      (straight-ui-mode))
    (switch-to-buffer straight-ui-buffer)))

(defun straight-ui--minibuffer-setup ()
  "Set up the minibuffer for live filtering."
  (when straight-ui-search-filter-active
    ;;@INCOMPLETE: implement search operator syntax
    ;;(set-syntax-table straight-ui-search-filter-syntax-table)
    (when straight-ui-search-filter-active
      (add-hook 'post-command-hook
                'straight-ui--update-search-filter nil :local))))

(add-hook 'minibuffer-setup-hook 'straight-ui--minibuffer-setup)

(defun straight-ui--parse-search-filter (filter)
  "Return a list of form (COLUMN TEST...) for FILTER string."
  (let (results seq)
    (dolist (char (split-string filter "" 'omit-nulls))
      (if (string= char "|")
          (setq results (push (nreverse seq) results)
                seq nil)
        (setq seq (push char seq))))
    (setq results (push (nreverse seq) results))
    (mapcar (lambda (s) (split-string (string-join s "") "\s" 'omit-nulls))
            (nreverse results))))

;;(straight-ui--parse-search-filter "test ok | ok | ")
;;len 1 = apply to everything
;;len >1 = loop columns

(defun straight-ui--query-matches-p (query subject)
  "Return t if QUERY (negated or otherwise) agrees with SUBJECT."
  (let* ((negated (and (string-prefix-p "!" query)
                       (setq query (substring query 1))))
         (match (ignore-errors (string-match-p query subject))))
    (if negated (not match) match)))

(defun straight-ui--update-search-filter (&optional query)
  "Update the UI to reflect search input.
If QUERY is non-nil, use that instead of the minibuffer."
  (when-let ((buffer straight-ui-buffer)
             (query (or query (minibuffer-contents-no-properties))))
    (unless (string-empty-p query)
      (with-current-buffer buffer
        (let ((queries (straight-ui--parse-search-filter query))
              (packages (straight-ui-melpa-list)))
          (setq tabulated-list-entries
                (if (eq (length queries) 1)
                    (cl-remove-if-not
                     (lambda (package)
                       (cl-some
                        (lambda (column)
                          (cl-every (lambda (subquery)
                                      (straight-ui--query-matches-p
                                       subquery column))
                                    (car queries)))
                        (cl-coerce (cadr package) 'list)))
                     packages)
                  ;;good

                  (cl-remove-if-not
                   (lambda (package)
                     (let ((colqueries (cl-remove-if-not
                                        (lambda (colquery)
                                          (delq nil colquery))
                                        queries))
                           (columns (let ((i 0))
                                      (cl-remove-if-not
                                       (lambda (_) (prog1
                                                       (delq nil (nth i queries))
                                                     (cl-incf i)))
                                       (cl-coerce (cadr package) 'list)))))
                       (cl-every
                        (let ((i 0))
                          (lambda (column)
                            (prog1
                                (cl-every (lambda (subquery)
                                            (straight-ui--query-matches-p
                                             subquery column))
                                          (nth i colqueries))
                              (cl-incf i))))
                        columns)))
                   packages)))
          (tabulated-list-print 'remember-pos 'update))))))

(defun straight-ui--debounce-search ()
  "Update filter from minibuffer."
  (if straight-ui--search-timer
      (cancel-timer straight-ui--search-timer))
  (setq straight-ui--search-timer
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
            (condition-case nil
                (read-from-minibuffer "Search (empty to clear): "
                                      (when edit straight-ui-search-filter))
              (quit straight-ui-search-filter)))
    (setq straight-ui-search-filter-active nil)
    (when (string-empty-p straight-ui-search-filter)
      ;;reset to default view
      (straight-ui--update-search-filter ".*"))))

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

(defun straight-ui-mark (package &optional action face prefix)
  "Mark PACKAGE for ACTION with PREFIX.
ACTION is a function which will be called.
It is passed the name of the package as its sole argument.
If FACE is non-nil, use that instead of `straight-ui-marked-package'.
PREFIX is displayed before the PACKAGE name."
  (interactive)
  (cl-pushnew (cons package action) straight-ui--marked-packages
              :test (lambda (a b) (string= (car a) (car b))))
  (with-silent-modifications
    (put-text-property (line-beginning-position) (line-end-position)
                       'display
                       (propertize
                        (concat (or prefix "* ") (string-trim (thing-at-point 'line)))
                        'face
                        (or face 'straight-ui-marked-package)))
    (forward-line)))

(defun straight-ui-current-package ()
  "Return current package of UI line as a string."
  (if-let ((prop (get-text-property (point) 'tabulated-list-id)))
      (symbol-name prop)
    (user-error "No package found at point")))

(defun straight-ui-toggle-mark (&optional action face prefix)
  "Toggle ACTION mark for current package.
FACE is used as line's display.
PREFIX is displayed before package name."
  (interactive)
  (let ((package (straight-ui-current-package)))
    (if (straight-ui-package-marked-p package)
        (straight-ui-unmark package)
      (straight-ui-mark package action face prefix))))

(defun straight-ui-mark-install ()
  "Mark package for installation."
  (interactive)
  (unless (string= (buffer-name) straight-ui-buffer)
    (user-error "Cannot mark outside of %S" straight-ui-buffer))
  (let ((package (straight-ui-current-package)))
    (when (straight--installed-p (gethash package straight--recipe-cache))
      (user-error "Package %S already installed" package)))
  (straight-ui-toggle-mark
   (lambda (p) (straight-use-package (intern p) nil nil nil 'interactive))
   'straight-ui-marked-install straight-ui-install-prefix))

(defun straight-ui-execute-marks ()
  "Execute each action in `straight-ui-marked-packages'."
  (interactive)
  (mapc (lambda (marked)
          (if-let ((action (cdr marked)))
              ;;@INCOMPLETE:
              ;; We need to clean up (visually unmark, remove package
              ;; from marked list) What should we do if this fails?
              (progn
                (funcall action (car marked))
                (straight-ui-melpa-list 'refresh)
                (straight-ui--update-search-filter straight-ui-search-filter)
                (tabulated-list-print 'remember-pos 'update))
            (message "@TODO: implement generic mark prompt.")))
        (nreverse straight-ui--marked-packages)))

;;;; Key bindings
(define-key straight-ui-mode-map (kbd "*") 'straight-ui-toggle-mark)
(define-key straight-ui-mode-map (kbd "F") 'straight-ui-toggle-follow-mode)
(define-key straight-ui-mode-map (kbd "I") 'straight-ui-show-installed)
(define-key straight-ui-mode-map (kbd "RET") 'straight-ui-show-package-info)
(define-key straight-ui-mode-map (kbd "S") 'straight-ui-search-edit)
(define-key straight-ui-mode-map (kbd "j") 'straight-ui-next-line)
(define-key straight-ui-mode-map (kbd "k") 'straight-ui-previous-line)
(define-key straight-ui-mode-map (kbd "i") 'straight-ui-mark-install)
(define-key straight-ui-mode-map (kbd "s") 'straight-ui-search)
(define-key straight-ui-mode-map (kbd "x") 'straight-ui-execute-marks)

;;;; INCOMPLETE

(defclass straight-ui-info (magit-section)
  ((keymap :initform 'straight-ui-package-info-mode-map)))

(define-key straight-ui-package-info-mode-map (kbd "RET") 'push-button)

(defun straight-ui--button-install (&rest _args)
  "Install package via the button."
  (straight-use-package (intern straight-ui--current-package))
  (with-current-buffer straight-ui-buffer
    (straight-ui-melpa-list 'refresh)
    (straight-ui--update-search-filter straight-ui-search-filter)
    (tabulated-list-print 'remember-pos 'update)
    (straight-ui--package-info-print straight-ui--current-package)))

(defun straight-ui-insert-uninstalled-buttons ()
  "Insert buttons for uninstalled packages."
  (insert-button "Install" 'action #'straight-ui--button-install))

;;@INCOMPLETE: Should be a tree and use magit-sections
;; As it stands this does not show transitive deps
(defun straight-ui--package-info-related (packages filter)
  "FILTER related PACKAGES."
  (mapconcat (lambda (package)
               (make-text-button
                package nil
                'button-data package
                'action (lambda (p) (straight-ui-show-package-info p))))
             (sort (cl-remove-duplicates
                    (mapcar filter packages)
                    :test #'string=)
                   #'string<)
             "\n"))

(defun straight-ui--package-info-dependents (package)
  "Print dependencies for installed PACKAGE."
  (magit-insert-section (straight-ui-info)
    (magit-insert-heading "Dependents")
    (if-let ((dependents (straight-dependents package)))
        (insert (straight-ui--package-info-related
                 dependents
                 (lambda (p) (if (listp p) (car (last p)) p)))
                "\n")
      ;;@RESERACH: I don't know how magit-cancel-section is supposed to work...
      (magit-cancel-section))))

(defun straight-ui--package-info-dependencies (package)
  "Print dependencies for installed PACKAGE."
  (magit-insert-section (straight-ui-info)
    (magit-insert-heading "Dependencies")
    (if-let ((dependencies (straight-dependencies package)))
        (insert (straight-ui--package-info-related
                 dependencies
                 (lambda (p) (if (listp p) (car p) p)))
                "\n")
      (magit-cancel-section))))

(defun straight-ui--package-info-branch-status (package)
  "Is PACKAGE on branch specified by the recipe?
If not, offer to normalize."
  (let* ((local-repo (straight--repos-dir
                      (plist-get (gethash package straight--recipe-cache) :local-repo)))
         (default-directory local-repo)
         (default (straight-vc-git--default-remote-branch straight-vc-git-default-remote-name local-repo))
         (status (straight-vc-git--compare-and-canonicalize default "HEAD")))
    (unless (plist-get status :same)
      (magit-insert-section (straight-ui-info)
        (magit-insert-heading (propertize "DIVERGED" 'face 'diff-error))
        (insert (format "Local Branch: %s Recipe Branch: %s\n"
                        (propertize (plist-get status :right-ref)
                                    'face 'magit-branch-local)
                        (propertize (plist-get status :left-ref)
                                    'face 'magit-branch-remote)))))))

(defun straight-ui--package-info-print (package)
  "Print info for PACKAGE."
  (with-current-buffer (get-buffer-create straight-ui-package-info-buffer)
    (if-let* ((inhibit-read-only t)
              ;;@DECOUPLE
              ;;This needs to work generically for the recipe repo
              ;;from which package was installed
              (infos (straight-ui-melpa-list))
              (info (car (alist-get (intern package) infos)))
              (description (string-trim (aref info 1))))
        (straight--with-plist (straight--convert-recipe
                               (straight-recipes-retrieve (intern package)))
            (local-repo included-by)
          (setq default-directory (straight--repos-dir (or local-repo
                                                           included-by
                                                           package)))
          (erase-buffer)
          (goto-char (point-min))
          (let ((installed-p (file-exists-p default-directory)))
            (when installed-p (straight-fetch-package package 'from-upstream))
            (magit-insert-section (straight-ui-info)
              (magit-insert-heading package)
              (insert description "\n"))
            (if installed-p
                (let (magit-section-show-child-count)
                  (straight-ui--package-info-dependencies package)
                  (straight-ui--package-info-dependents package)
                  (straight-ui--package-info-branch-status package)
                  (let ((_p (point)))
                    (magit-insert-unpulled-from-upstream)
                    ;; no updates
                    ;;@INCOMPLETE: need to bound this so we don't
                    ;;find any "(empty)" section
                    ;; (when (re-search-backward "(empty)" p t)
                    ;;   (save-excursion
                    ;;     (goto-char (point-max))
                    ;;     (dotimes (_ 2)
                    ;;       (forward-line -1)
                    ;;       (delete-region (point) (line-end-position))))
                    ;;   (insert (propertize "package up to date\n"
                    ;;                       'face '(:foreground "green"))))
                    ))
              (magit-insert-section
                (straight-ui-info)
                (straight-ui-insert-uninstalled-buttons)))
            (magit-mode)
            (goto-char (point-min))
            (setq straight-ui--current-package package)))
      (user-error "No package info for %S available" package))))

;;@INCOMPLETE: Optional numeric arg
;;;###autoload
(defun straight-ui-package-info-history-prev ()
  "Step back through package info history."
  (straight-ui-show-package-info the-last-one))

;;;###autoload
(defun straight-ui-show-package-info (package)
  "Show info for PACKAGE."
  (interactive (list (if (string= (buffer-name) straight-ui-buffer)
                         (straight-ui-current-package)
                       (straight--select-package "package info"))))
  (straight-ui--package-info-print package)
  (unless (string= (buffer-name) straight-ui-package-info-buffer)
    (view-buffer-other-window straight-ui-package-info-buffer)))

(defun straight-ui-follow-maybe (package)
  "Follow the PACKAGE info."
  (save-selected-window
    (funcall (if (get-buffer-window straight-ui-package-info-buffer)
                 #'straight-ui--package-info-print
               #'straight-ui-show-package-info)
             package)))

;;@PERFORMANCE: too slow in use. Maybe cache git results?
(define-minor-mode straight-ui-follow-mode
  "Update package info buffer while navigating UI buffer."
  :lighter " stui-follow"
  (message "follow mode %sabled"
           (if straight-ui-follow-mode "en" "dis")))

(defun straight-ui-toggle-follow-mode ()
  "Toggle command `straight-ui-follow-mode'."
  (interactive)
  (straight-ui-follow-mode 'toggle))

(defun straight-ui-previous-line (&optional n)
  "Move N lines back."
  (interactive "p")
  (forward-line (- (or n 1)))
  (when straight-ui-follow-mode (straight-ui-follow-maybe
                                 (straight-ui-current-package))))

(defun straight-ui-next-line (&optional n)
  "Move N lines back."
  (interactive "p")
  (forward-line (or n 1))
  (when straight-ui-follow-mode (straight-ui-follow-maybe
                                 (straight-ui-current-package))))


(provide 'straight-ui)

;;; straight-ui.el ends here
