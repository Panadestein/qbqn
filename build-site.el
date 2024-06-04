;;; package --- Build Org website

;;; Commentary:
;; Build website from Org-mode source files

;;; Code:

;; Set a package installation directory to avoid conflicts

(require 'package)
(setq package-user-dir (expand-file-name "./.packages"))
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

;; Initialize the package system

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Install packages needed for HTML export

(package-install 'htmlize)
(package-install 'reformatter)
(package-install 'color-theme-modern)

(require 'htmlize)
(require 'ox-publish)
(require 'font-lock)

;; Using this library is a work-around to get color in HTML exports.
;; Otherwise Emacs in batch mode cannot get the correct faces

(load-theme 'greiner t t)
(enable-theme 'greiner)

;; Set some variables for the export

(global-font-lock-mode t)
(setq org-html-validation-link nil
      org-html-head-include-scripts nil
      org-html-include-default-style nil
      org-confirm-babel-evaluate nil
      org-src-fontify-natively t)

;; Define the project to be published

(setq org-publish-project-alist
      (list
       (list "qbqn"
	     :recursive t
	     :base-directory "./src"
	     :publishing-directory "./public"
	     :publishing-function 'org-html-publish-to-html
	     :with-author nil
	     :with-creator nil
	     :with-toc nil
	     :section-numbers nil
	     :time-stamp-file nil)
       (list "assets"
	     :base-directory "./assets"
	     :base-extension "css\\|ttf"
	     :publishing-directory "./public/assets"
	     :publishing-function 'org-publish-attachment
	     :recursive t)
       (list "assets"
	     :base-directory "./ibm_eagle"
	     :publishing-directory "./public/ibm_eagle"
	     :publishing-function 'org-publish-attachment
	     :recursive t)))

;; Generate site

(org-publish-all t)

(message "Build completed")

(provide 'build-site)
;;; build-site.el ends here
