;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Emacs 24 configuration file
;;
;; Author:  Gordon Tillman, currently based almost entirely on
;;          configuration from John Eastman's emacs configuration:
;;          https://github.com/jeastman/emacs.d
;;
;; Notes:   Just minimal configuration is done in this file.  The bulk of
;;          configuration is done via init-ext.org.  In addition to that
;;          there is support for per-host and per-user initialization.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Fix up path - make sure usr/local is at top
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'cl)
(defun got-prepend-list (src-list prepend-list)
  "Prepend all of the entries in PREPEND-LIST to SRC-LIST.

If any element in PREPEND-LIST is already in SRC-LIST, it is removed
from its old position.

Returns a list that starts with the elements in PREPEND-LIST (in the
correct order) and are followed by the non-duplicate elements of SRC-LIST"
  (reverse (let ((prep-list-rev (reverse prepend-list)))
    (reduce
     (lambda (acc elem)
       (if (member elem acc)
           acc
         (cons elem acc))) (cons prep-list-rev src-list)))))

(setenv "PATH" (mapconcat
                (lambda (s) s)
                (got-prepend-list
                 (split-string (getenv "PATH") ":")
                 '(
                   "/usr/local/bin"
                   "/usr/local/sbin"
                   "/usr/texbin"
                   )) ":"))

(add-to-list 'exec-path "/usr/local/bin")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; set up package management with cask and pallet
;;
;; Prerequisites:
;;
;; 1. Install cask using instructions from https://github.com/cask/cask:
;;      curl -fsSkL https://raw.github.com/cask/cask/master/go | python
;;    -or-
;;    Install using homebrew:
;;      brew install cask
;; 2. Add the Cask bin directory to your PATH if you installed cask via the
;;    "curl" method: ~/.cask/bin
;; 3. cd to your $HOME/.emacs.d directory and run the "cask install" command.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This is the location for all my Emacs configuration
(setq got/dotfiles-dir (file-name-directory user-init-file))

;; Load cask from the correct location
(cond
 ((file-exists-p "~/.cask/cask.el")
  (require 'cask "~/.cask/cask.el"))  ; location for "curl" install
 ((file-exists-p "/usr/local/share/emacs/site-lisp/cask/cask.el")
  (require 'cask "/usr/local/share/emacs/site-lisp/cask/cask.el"))  ; location for homebrew install
 (t (error "Unable to location a cask installation")))
(cask-initialize)

;; Pallet maintains the "Cask" file in your .emacs.d directory.
(unless (package-installed-p 'pallet)
  (error "Package 'pallet' not available.  Please cd to %s and run 'cask install'" got/dotfiles-dir))
(require 'pallet nil t)

;; Workaround for this bug:
;; https://github.com/jwiegley/use-package/issues/85
(require 'ert nil t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Validate org installation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (not (cl-remove-if-not
            (lambda (p) (equal 'org (car p)))
            package-alist))
  (message "No org-mode package found; installing now...")
  (package-install 'org))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load in the rest of my configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'org)

(when (string-lessp (org-version) "8.2")
  (warn "Org-mode is out of date.  Expected version >= 8.2, found %s" (org-version)))

(let ((main-init-file (expand-file-name "init-ext.org" got/dotfiles-dir)))
  (when (file-exists-p main-init-file)
      (org-babel-load-file main-init-file)))

















(put 'scroll-left 'disabled nil)
