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
(defcustom straight-ui-search-debounce-interval 0.15
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
(defvar straight-ui-package-info-mode-map (make-sparse-keymap))
(defvar straight-ui-search-filter nil "Filter for package searches.")
(defvar straight-ui-search-active nil "Whether or not a search is in progress.")
(defvar straight-ui-show-installed nil "When non-nil only show installed packages.")
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
                    (let ((package (symbol-name (car recipe)))
                          (metadata (cdr recipe)))
                      (append (list :id package :package package :source "melpa")
                              (when-let ((description (alist-get 'desc metadata)))
                                (list :description description))
                              (when-let ((properties (alist-get 'props metadata)))
                                (when-let ((url (alist-get 'url properties)))
                                  (list :url url))))))
                  (straight-ui-melpa-recipes)))))

(defun straight--ui-init ()
  "Initialize format of the UI."
  (setq tabulated-list-format
        [("Package" 20 t)
         ("Description" 80 t)
         ("Source" 20 t)]
        tabulated-list-entries
        #'straight-ui-list-packages))

(defun straight-ui-format-entries (entries)
  "Format ENTRIES for use in table."
  (mapcar (lambda (entry)
            (list (plist-get entry :id)
                  (vector (plist-get entry :package)
                          (or (plist-get entry :description) "n/a")
                          (or (plist-get entry :source) "n/a"))))
          entries))

(defun straight-ui-list-packages (&optional sources refresh raw)
  "Return a formatted list of packages from SOURCES.
If SOURCES is nil, each recipe repository in `straight-recipe-repositories'
is checked if it implements metadata.
If REFRESH is non-nil caches are bypassed.
If RAW is non-nil, do not format for `tabulated-list-entries'."
  (let (packages)
    (dolist (source (or sources straight-recipe-repositories))
      (setq packages
            (append packages
                    (straight-recipes 'metadata source "metadata" refresh))))
    (cl-sort packages :key (lambda (it) (plist-get it :id)))
  (if raw packages
    (straight-ui-format-entries
     (cl-remove-duplicates
      packages :test #'string= :key (lambda (x) (plist-get x :id)))))))

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
  (setq straight-ui-search-filter
        (if (setq straight-ui-show-installed (not straight-ui-show-installed))
            ".*" "#installed"))
  (straight-ui--update-search-filter straight-ui-search-filter))

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
  (when straight-ui-search-active
    (add-hook 'post-command-hook
              'straight-ui--debounce-search nil :local)))

(add-hook 'minibuffer-setup-hook 'straight-ui--minibuffer-setup)

(defun straight-ui--parse-search-filter (filter)
  "Return a list of form ((TAGS...) ((COLUMN)...)) for FILTER string."
  (let* ((tags (cl-remove-if-not (lambda (s) (string-prefix-p "#" s))
                                 (split-string filter " " 'omit-nulls)))
         (columns (mapcar (lambda (col)
                            (cl-remove-if
                             (lambda (word) (member word tags))
                             (split-string (string-trim col) " " 'omit-nulls)))
                          (split-string filter "|"))))
    (list (mapcar (lambda (s) (substring s 1)) tags) columns)))

(defun straight-ui--query-matches-p (query subject)
  "Return t if QUERY (negated or otherwise) agrees with SUBJECT."
  (let* ((negated (and (string-prefix-p "!" query)
                       (setq query (substring query 1))))
         (match (ignore-errors (string-match-p query subject))))
    (cond
     ;;ignore negation operator by itself
     ((string-empty-p query) t)
     (negated (not match))
     (t match))))

(defun straight--package-on-default-branch-p (package)
  "Return t if PACKAGE is on its default branch."
  ;;converted recipe branch = current branch
  (straight--with-plist
      (nth 2 (gethash package straight--build-cache))
      (local-repo branch remote)
    (let* ((default-directory (straight--repos-dir local-repo))
           (default-branch (or branch
                               (straight-vc-git--default-remote-branch
                                (or remote straight-vc-git-default-remote-name)
                                local-repo)))
           (status (straight-vc-git--compare-and-canonicalize
                    default-branch "HEAD")))
      (string= (plist-get status :left-ref) (plist-get status :right-ref)))))

(defun straight-ui--default-branch-tag (row)
  "ROW."
  (let ((package (aref (cadr row) 0)))
    (and (straight--installed-p (nth 2 (gethash package straight--build-cache)))
         (straight--package-on-default-branch-p package))))

(defun straight-ui--worktree-dirty-p (package)
  "Return t if PACKAGE has a dirty worktree."
  (let ((recipe (nth 2 (gethash package straight--build-cache))))
    (when (straight--installed-p recipe)
      (straight--with-plist recipe (local-repo)
        (let ((default-directory (straight--repos-dir local-repo)))
          (not (string-empty-p
                (straight--process-output "git" "-c" "status.branch=false"
                                          "status" "--short"))))))))

(defun straight-ui--local-branch-behind-p (package)
  "Return t if installed PACKAGE has unpulled upstream commits."
  (let ((recipe (nth 2 (gethash package straight--build-cache))))
    (when (straight--installed-p recipe)
      (straight--with-plist
          (nth 2 (gethash package straight--build-cache))
          (local-repo branch remote)
        (straight-fetch-package package 'from-upstream)
        (let* ((default-directory (straight--repos-dir local-repo))
               (remote (or remote straight-vc-git-default-remote-name))
               (default-branch (or branch
                                   (straight-vc-git--default-remote-branch
                                    remote
                                    local-repo)))
               (status (straight-vc-git--compare-and-canonicalize
                        (concat remote "/" default-branch) "HEAD")))
          (and
           (plist-get status :right-is-ancestor)
           (not (plist-get status :same))))))))

;;@MAYBE: allow literal string searches?
;;similar to a macro, a tag expands to a search?
(defcustom straight-ui-search-tags
  '(("default-branch" . straight-ui--default-branch-tag)
    ("dirty" . (lambda (p) (straight-ui--worktree-dirty-p (aref (cadr p) 0))))
    ("behind" . (lambda (p) (straight-ui--local-branch-behind-p (aref (cadr p) 0))))
    ("installed" . (lambda (p) (straight--installed-p
                                (gethash (aref (cadr p) 0)
                                         straight--recipe-cache)))))
  "Alist of search tags.
Each cell is of form (NAME FILTER).
If FILTER is a function it must accept a single package as its sole
argument and return non-nil if the package is to be kept.

Each tag can be inverted in the minibuffer by prepending an
exclamation point to it. e.g. #!installed."
  :group 'straight-ui
  :type 'alist)

(defun straight-ui--update-search-filter (&optional query)
  "Update the UI to reflect search input.
If QUERY is non-nil, use that instead of the minibuffer."
  (when-let ((buffer straight-ui-buffer)
             (query (or query (minibuffer-contents-no-properties))))
    (unless (string-empty-p query)
      (with-current-buffer buffer
        (let* ((parsed (straight-ui--parse-search-filter query))
               (tags (car parsed))
               (queries (cadr parsed))
               ;;@INCOMPLETE: we need to generalize this.
               (unfiltered (straight-ui-list-packages))
               (packages
                (dolist (tag tags unfiltered)
                  (let ((negated (and (string-prefix-p "!" tag)
                                      (setq tag (substring tag 1)))))
                    (when-let ((filter (alist-get tag
                                                  straight-ui-search-tags
                                                  nil nil #'string=)))
                      (setq unfiltered
                            (funcall (if negated #'cl-remove-if #'cl-remove-if-not)
                                     filter unfiltered)))))))
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

(defvar straight-ui--previous-minibuffer-contents ""
  "Keep track of minibuffer contents changes.
Allows for less debouncing than just generally hooking into `post-command-hook'.")

(defun straight-ui--debounce-search ()
  "Update filter from minibuffer."
  (let ((input (string-trim (minibuffer-contents-no-properties))))
    (unless (string= input straight-ui--previous-minibuffer-contents)
      (setq straight-ui--previous-minibuffer-contents input)
      (if straight-ui--search-timer
          (cancel-timer straight-ui--search-timer))
      (setq straight-ui--search-timer
            (run-at-time straight-ui-search-debounce-interval
                         nil
                         #'straight-ui--update-search-filter)))))

(defun straight-ui-search (&optional edit)
  "Filter current buffer by string.
If EDIT is non-nil, edit the last search."
  (interactive)
  (unwind-protect
      (setq straight-ui-search-active t
            straight-ui-search-filter
            (condition-case nil
                (read-from-minibuffer "Search (empty to clear): "
                                      (when edit straight-ui-search-filter))
              (quit straight-ui-search-filter)))
    (setq straight-ui-search-active nil)
    (when (string-empty-p straight-ui-search-filter)
      ;;reset to default view
      (straight-ui--update-search-filter ".*"))))

(defun straight-ui-search-edit ()
  "Edit last search."
  (interactive)
  (straight-ui-search 'edit))

(defun straight-ui-browse-package ()
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
(define-key straight-ui-mode-map (kbd "RET") 'straight-ui-describe-package)
(define-key straight-ui-mode-map (kbd "S") 'straight-ui-search-edit)
(define-key straight-ui-mode-map (kbd "b") 'straight-ui-browse-package)
;; (define-key straight-ui-mode-map (kbd "j") 'straight-ui-next-line)
;; (define-key straight-ui-mode-map (kbd "k") 'straight-ui-previous-line)
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
    (let ((magit-display-buffer-function
           (lambda (buffer)
             (display-buffer buffer '(display-buffer-same-window)))))
      (straight-ui--package-info-print straight-ui--current-package)
      (kill-buffer (format "straight: %s" straight-ui--current-package)))))

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
                'action (lambda (p) (straight-ui-describe-package p))))
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
                 (lambda (p) (if (listp p) (car (flatten-tree (last p))) p)))
                "\n")
      (magit-cancel-section))))

(defun straight-ui--package-info-dependencies (package)
  "Print dependencies for installed PACKAGE."
  (magit-insert-section (straight-ui-info)
    (magit-insert-heading "Dependencies")
    (if-let ((dependencies (straight-dependencies package)))
        (insert (straight-ui--package-info-related
                 dependencies
                 (lambda (p) (if (listp p) (car (flatten-tree p)) p)))
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
  (if-let* ((inhibit-read-only t)
            ;;@DECOUPLE
            ;;This needs to work generically for the recipe repo
            ;;from which package was installed
            (metadata (straight-ui-melpa-list))
            (info (car (alist-get (intern package) metadata)))
            (description (string-trim (aref info 1))))
      (straight--with-plist (straight--convert-recipe
                             (straight-recipes-retrieve (intern package)))
          (local-repo included-by)
        (let* ((default-directory (straight--repos-dir (or local-repo
                                                           included-by
                                                           package)))
               (installed-p (file-exists-p default-directory))
               hooks)
          (push
           (lambda ()
             (magit-insert-section (straight-ui-info)
               (magit-insert-heading package)
               (insert description "\n")))
           hooks)
          (if (not installed-p)
              (progn
                (push (lambda ()
                        (magit-insert-section (straight-ui-info)
                          (straight-ui-insert-uninstalled-buttons))
                        (insert "\n"))
                      hooks)
                (with-current-buffer (get-buffer-create (format "straight: %s" package))
                  (with-silent-modifications (erase-buffer))
                  (dolist (fn (nreverse hooks)) (funcall fn))
                  ;;@MAYBE: get rid of this. We can't really *do* most of what magit
                  ;;offers until we install...
                  (magit-mode)
                  (view-buffer-other-window (current-buffer))))
            (straight-fetch-package package 'from-upstream)
            (push (lambda () (straight-ui--package-info-dependencies package)) hooks)
            (push (lambda () (straight-ui--package-info-dependents package)) hooks)
            (let ((magit-status-sections-hook (append (nreverse hooks) magit-status-sections-hook))
                  (magit-display-buffer-function (if (string= (buffer-name) straight-ui-buffer)
                                                     magit-display-buffer-function
                                                   (lambda (buffer)
                                                     (display-buffer buffer '(display-buffer-same-window))))))
              (magit-status-setup-buffer)))
          (setq straight-ui--current-package package)))
    (user-error "No package info for %S available" package)))

;;;###autoload
(defun straight-ui-describe-package (package)
  "Show info for PACKAGE."
  (interactive (list (if (string= (buffer-name) straight-ui-buffer)
                         (straight-ui-current-package)
                       (straight--select-package "package info"))))
  (straight-ui--package-info-print package))

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

;;@FIX: These don't work with visual selection/region
(defun straight-ui-previous-line (&optional n)
  "Move N lines back."
  (interactive "p")
  (forward-line (- (or n 1)))
  (when straight-ui-follow-mode (straight-ui--package-info-print
                                 straight-ui--current-package)))

(defun straight-ui-next-line (&optional n)
  "Move N lines back."
  (interactive "p")
  (forward-line (or n 1))
  (when straight-ui-follow-mode (straight-ui--package-info-print
                                 straight-ui--current-package)))

(provide 'straight-ui)

;;; straight-ui.el ends here
