;;; ci-build.el --- CI Entry Point for Quartz Site

;; 1. Setup Package Manager (MELPA)
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Refresh package list if it's empty (happens in fresh containers)
(unless package-archive-contents
  (package-refresh-contents))

;; 2. Install Dependencies
;; ox-hugo will automatically pull in 'tomelr'
(unless (package-installed-p 'ox-hugo)
  (package-install 'ox-hugo))

(require 'ox-hugo)
(require 'org)

;; 3. Define the Build Function
(defun build-quartz-site ()
  "Export all org files in 'my-org-files' to hugo markdown."
  (message "Starting Quartz Export...")

  ;; Global configs for the export
  (setq org-hugo-auto-set-lastmod t)

  (let ((org-files (directory-files-recursively "my-org-files" "\\.org$")))
    (dolist (file org-files)
      (unless (string-match-p "/\\.github/" file)
        (message "Processing: %s" file)
        (with-current-buffer (find-file-noselect file)
          ;; This exports based on the #+HUGO_BASE_DIR or default export paths
          (org-hugo-export-wim-to-md :all-subtrees)
          (kill-buffer)))))

  (message "Export Complete."))
