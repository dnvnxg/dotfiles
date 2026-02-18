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

;; 3. Helper function to compute hugo_section from file path
(defun get-hugo-section-for-file (file base-dir)
  "Get the hugo_section for a file based on its directory relative to base-dir.
   
   Examples:
   - my-org-files/index.org -> \".\"
   - my-org-files/cue/cue.org -> \"cue\"
   - my-org-files/cue/blog/001.org -> \"cue/blog\""
  (let* ((rel-path (file-relative-name file base-dir))
         (dir-path (file-name-directory rel-path)))
    (if (or (null dir-path) (string= dir-path "./"))
        "."
      (directory-file-name dir-path))))

;; 4. Define the Build Function
(defun build-quartz-site ()
  "Export org files to hugo markdown, preserving directory structure."
  (message "Starting Quartz Export...")
  
  ;; --- CONFIGURATION START ---
  ;; 1. Force timestamps to update
  (setq org-hugo-auto-set-lastmod t)
  (setq org-hugo-front-matter-format "yaml")
  
  ;; 2. Set base directory
  (setq org-hugo-base-dir (file-truename "my-org-files"))
  ;; --- CONFIGURATION END ---
  
  (let ((org-files (directory-files-recursively "my-org-files" "\\.org$"))
        (base-dir (file-truename "my-org-files")))
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
                    ;; Compute and set hugo_section dynamically
                    (let ((hugo-section (get-hugo-section-for-file file base-dir)))
                      (message "Exporting: %s (section: %s)" file hugo-section)
                      ;; Set the hugo_section property at file level
                      (setq-local org-hugo-section hugo-section)
                      ;; Export!
                      (org-hugo-export-wim-to-md :all-subtrees)))
                (message "Skipping: %s (No #+title found)" file))
            ;; Error handler
            (error (message "ERROR processing %s: %s" file (error-message-string err))))
          (kill-buffer)))))
  (message "Export Complete."))
