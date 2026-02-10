;;; ci-build.el --- CI Entry Point for Quartz Site

;; 1. Initialize Package Manager
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

;; 2. Install Dependencies
(unless (package-installed-p 'ox-hugo)
  (package-install 'ox-hugo))

(require 'ox-hugo)
(require 'org)

;; 3. Define the Build Function
(defun build-quartz-site ()
  "Export org files to hugo markdown, skipping invalid files."
  (message "Starting Quartz Export...")
  
  ;; --- CONFIGURATION START ---
  ;; 1. Force timestamps to update
  (setq org-hugo-auto-set-lastmod t)
  (setq org-hugo-front-matter-format "yaml")
  
  ;; 2. THE FIX: Tell ox-hugo where the "root" of the site is.
  ;; We set it to the directory where the org files are checked out.
  (setq org-hugo-base-dir (file-truename "my-org-files"))
  ;; --- CONFIGURATION END ---
  
  (let ((org-files (directory-files-recursively "my-org-files" "\\.org$")))
    (dolist (file org-files)
      ;; Skip .github folder and any file in a "finance" directory
      (unless (or (string-match-p "/\\.github/" file)
                  (string-match-p "/finance/" file))
        (with-current-buffer (find-file-noselect file)
          (condition-case err
              (if (save-excursion 
                    (goto-char (point-min))
                    (re-search-forward "^#\\+title:" nil t))
                  (progn
                    (message "Exporting: %s" file)
                    ;; Export!
                    (org-hugo-export-wim-to-md :all-subtrees))
                (message "Skipping: %s (No #+title found)" file))
            ;; Error handler
            (error (message "ERROR processing %s: %s" file (error-message-string err))))
          (kill-buffer)))))
  (message "Export Complete."))