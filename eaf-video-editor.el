;;; eaf-video-editor.el ---                          -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Qiqi Jin

;; Author: Qiqi Jin <ginqi7@gmail.com>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(defcustom eaf-video-editor-keybinding
  '(("SPC" . "toggle_play")
    ("x" . "close_buffer")
    ("h" . "play_backward")
    ("l" . "play_forward")
    ("j" . "decrease_volume")
    ("k" . "increase_volume")
    ("f" . "toggle_fullscreen")
    ("r" . "restart")
    ("c" . "clip_point")
    ("p" . "toggle_play_clips")
    ("e" . "export"))

  "The keybinding of EAF Video Player."
  :type 'cons)


(org-link-set-parameters "eaf-ve-clip"
                         :follow #'eaf-video-editor-link-follow-function)

(org-link-set-parameters "eaf-ve-text")

(defun eaf-video-editor--get-file-name-full-path-no-ext ()
  (interactive)
  (let ((file-name (buffer-file-name)))
    (when file-name
      (file-name-sans-extension file-name))))

(defun eaf-video-editor-buffer-id ()
  (let* ((file-path (file-name-sans-extension (buffer-file-name)))
         (file-name (file-name-nondirectory file-path))
         (eaf-buffer (cl-find-if
                      (lambda(buffer)
                        (string= file-name (buffer-name buffer)))
                      (eaf--get-eaf-buffers))))
    (buffer-local-value 'eaf--buffer-id eaf-buffer)))

(defun eaf-video-editor-link-path-to-clip (path)
  (mapcar #'string-to-number (string-split path "-")))

(defun eaf-video-editor-link-follow-function (path arg)
  (let* ((buffer-id (eaf-video-editor-buffer-id))
         (clip (mapcar #'string-to-number (string-split path "-"))))
    (eaf-call-sync "execute_function_with_args" buffer-id "loop_play_in_clip" (car clip) (nth 1 clip))))

;; (print buffer-id)))

(defvar eaf-video-editor--org-file nil)

(defun eaf-video-editor--add-clip (begin end)
  (with-current-buffer (find-file-noselect eaf-video-editor--org-file)
    (goto-char (point-max))
    (insert (format "\n* [[eaf-ve-clip:%s-%s]]" begin end))
    (save-buffer)))

(add-to-list 'eaf-app-binding-alist '("video-editor" . eaf-video-editor-keybinding))

(setq eaf-video-editor-module-path (concat (file-name-directory (or load-file-name (buffer-file-name)))  "buffer.py"))

(add-to-list 'eaf-app-module-path-alist '("video-editor" . eaf-video-editor-module-path))

(defun eaf-video-editor-open ()
  (interactive)
  (eaf-open (expand-file-name "~/Movies/hello.mkv") "video-editor"))

(defun eaf-video-editor-org-clips ()
  (with-current-buffer (find-file-noselect eaf-video-editor--org-file)
    (let ((headlines (eaf-video-editor--get-all-org-headlines))
          (clips))
      (dolist (headline headlines)
        (with-temp-buffer
          (insert headline)
          (org-element-map (org-element-parse-buffer) 'link
            (lambda (link)
              (when (string= "eaf-ve-clip" (org-element-property :type link))
                (push (org-element-property :path link) clips))))))
      (mapcar #'eaf-video-editor-link-path-to-clip (reverse clips)))))

(defun eaf-video-editor-update-clips ()
  (interactive)
  (let ((clips (eaf-video-editor-org-clips))
        (buffer-id (eaf-video-editor-buffer-id)))
    (eaf-call-sync "execute_function_with_args"
                   buffer-id
                   "update_clips" clips)))

(defun eaf-video-editor-parse-link (link)
  (let ((type (org-element-property :type link))
        (path (org-element-property :path link)))
    (pcase type
      ("eaf-ve-clip" (list type (eaf-video-editor-link-path-to-clip path)))
      ("eaf-ve-text" (list type (eaf-video-editor-link-path-to-text path))))))

(defun eaf-video-editor-link-path-to-text (path)
  (let ((lst (string-split path ":")))
    (list (nth 0 lst) (string-to-number (nth 1 lst)))))


(defun eaf-video-editor-org-export-elements ()
  (with-current-buffer (find-file-noselect eaf-video-editor--org-file)
    (let ((headlines (eaf-video-editor--get-all-org-headlines))
          (export-elements)
          (element))
      (dolist (headline headlines)
        (with-temp-buffer
          (insert headline)
          (org-element-map (org-element-parse-buffer) 'link
            (lambda (link)
              (setq element (eaf-video-editor-parse-link link))
              (when element
                (push element export-elements))))))
      (reverse export-elements))))

(defun eaf-video-editor-update-export-elements ()
  (interactive)
  (let ((elements (eaf-video-editor-org-export-elements))
        (buffer-id (eaf-video-editor-buffer-id)))
    (eaf-call-sync "execute_function_with_args"
                   buffer-id
                   "update_export_elements" elements)))



(defun eaf-video-editor--get-all-org-headlines ()
  "Return a list of all headlines in the current Org-mode buffer."
  (interactive)
  (require 'org-element)
  (let (headlines)
    (org-element-map (org-element-parse-buffer) 'headline
      (lambda (headline)
        (push (org-element-property :raw-value headline) headlines)))
    (if (called-interactively-p 'interactive)
        (message "Headlines: %s" (reverse headlines))
      (reverse headlines))))



(provide 'eaf-video-editor)
;;; eaf-video-editor.el ends here
