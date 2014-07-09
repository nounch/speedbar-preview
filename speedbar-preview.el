;;; speedbar-previewer.el --- Previewer for speedbar buffers

;; Author: nounch <nounch@outlook.com>
;; Keywords: tools

;;; Commentary:

;; ---

;;; Code:


;;=========================================================================
;; Customizable variables
;;=========================================================================

(defcustom prevw-buffer "*Preview*"
  "Name of the speedbar-preview buffer.")

(defvar prevw-previous-dired-buffer "")


;;=========================================================================
;; Functions
;;=========================================================================

(defun prevw-preview ()
  (interactive)
  (let ((speedbar-func (get-text-property (1+ (point))
                                          'speedbar-function)))
    (cond
     ((eq speedbar-func 'speedbar-find-file)
      (speedbar-edit-line))     ; Speedbar would do that anyway on RET
     ((eq speedbar-func 'speedbar-dir-follow)
      (prevw-show-preview-buffer)))))

(defun prevw-generate-preview ()
  ;; Current directory:
  ;;   `speedbar-line-directory'
  ;; File/child directory name:
  ;;   `(get-text-property (1+ (point)) 'speedbar-text)'
  (let ((current-dir (speedbar-line-directory))
        (dir-or-file (get-text-property (1+ (point)) 'speedbar-text)))
    (concat current-dir dir-or-file "/")))

(defun prevw-show-preview-buffer ()
  (let ((preview-content (prevw-generate-preview)))
    (save-excursion
      (display-buffer (get-buffer-create prevw-buffer) t)
      (with-selected-window (get-buffer-window prevw-buffer)
        ;; Make buffer writable
        (toggle-read-only -1)
        (erase-buffer)
        (unless (eq prevw-previous-dired-buffer "")
          (kill-buffer (get-buffer prevw-previous-dired-buffer)))
        ;; (insert preview-content)
        (dired preview-content)
        (setq prevw-previous-dired-buffer (buffer-name (current-buffer)))
        ;; Make buffer read-only
        (toggle-read-only 1)))))

(defun prevw-next (&optional arg)
  (interactive "P")
  (speedbar-next arg)
  (prevw-preview))

(defun prevw-prev (&optional arg)
  (interactive "P")
  (speedbar-prev arg)
  (prevw-preview))


;;=========================================================================
;; External interface
;;=========================================================================

(defalias 'speedbar-preview 'prevw-preview
  "Preview the current item in the current Speedbar buffer.")

(defalias 'speedbar-preview-next 'prevw-next
  "Preview the item below the current item in the current Speedbar \
buffer.")

(defalias 'speedbar-preview-prev 'prevw-prev
  "Preview the item above the current item in the current Speedbar \
buffer.")


;;=========================================================================
;; Key bindings
;;=========================================================================

(define-key speedbar-key-map (kbd "N") 'speedbar-preview-next)
(define-key speedbar-key-map (kbd "P") 'speedbar-preview-prev)
(define-key speedbar-key-map (kbd "K") 'speedbar-preview)
(define-key speedbar-key-map (kbd "k") 'speedbar-preview)


;;=========================================================================
;; Provide
;;=========================================================================

(provide 'speedbar-previewer)
;;; speedbar-previewer.el ends here
