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

;; 3. Helper function to convert string to title case
(defun title-case (str)
  "Convert string to title case (e.g., 'CUE' -> 'Cue', 'MY-FOLDER' -> 'My-folder')"
  (let ((lower (downcase str)))
    (concat (upcase (substring lower 0 1)) (substring lower 1))))

;; 4. Helper function to compute hugo_section from file path
(defun get-hugo-section-for-file (file base-dir)
  "Get the hugo_section for a file based on its directory.
   Simply extract the directory part of the file path and strip my-org-files/ prefix.
   Converts path components to title case."
  (let* (;; Get directory of file, removing my-org-files/ prefix if present
         (raw-dir (file-name-directory file))
         ;; Strip my-org-files/ from the beginning
         (stripped (if (string-prefix-p "my-org-files/" raw-dir)
                      (substring raw-dir (length "my-org-files/"))
                    raw-dir))
         ;; Remove trailing slash
         (clean-path (directory-file-name stripped)))
    (if (or (null clean-path) (string= clean-path "") (string= clean-path "./"))
        "."
      ;; Convert each path component to title case
      (mapconcat 'title-case (split-string clean-path "/") "/"))))

;; 5. Helper function to check if file should be exported as folder index
(defun should-export-as-index (file)
  "Check if file is named same as its parent folder (e.g., CUE/CUE.org).
   If so, it should be exported as index.md."
  (let* ((dir-parts (split-string (file-name-directory file) "/" t))
         (file-name (file-name-sans-extension (file-name-nondirectory file)))
         (parent-folder (car (last dir-parts))))
    (and parent-folder
         file-name
         (string-equal (downcase file-name) (downcase parent-folder)))))

;; 4. Define the Build Function
(defun build-quartz-site ()
  "Export org files to hugo markdown, auto-detecting section from directory."
  (message "Starting Quartz Export...")
  
  ;; --- CONFIGURATION START ---
  (setq org-hugo-auto-set-lastmod t)
  (setq org-hugo-front-matter-format "yaml")
  ;; IMPORTANT: org-hugo-base-dir must point to the Hugo site ROOT (where content/ folder is)
  ;; During CI, this runs from the donovan.fyi repo root, so "." is correct
  (setq org-hugo-base-dir (file-truename "."))
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
                    ;; Compute hugo_section from file directory
                    (let ((hugo-section (get-hugo-section-for-file file base-dir))
                          (is-folder-index (should-export-as-index file)))
                      (message "Processing: %s -> section: %s (folder-index: %s)" file hugo-section is-folder-index)
                      ;; Set hugo_section as buffer-local variable for this export
                      (setq-local org-hugo-section hugo-section)
                      ;; If this is a folder-level note, add EXPORT_FILE_NAME property
                      (when is-folder-index
                        (goto-char (point-min))
                        (re-search-forward "^#\\+TITLE:" nil t)
                        (end-of-line)
                        (insert "\n#+EXPORT_FILE_NAME: index"))
                      ;; Export!
                      (org-hugo-export-wim-to-md :all-subtrees)))
                (message "Skipping: %s (No #+title found)" file))
            ;; Error handler
            (error (message "ERROR processing %s: %s" file (error-message-string err))))
          (kill-buffer)))))
  (message "Export Complete."))
