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

(defcustom eve-keybinding
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

  "The keybinding of EAF Video Editor."
  :type 'cons)

(defvar eve--org-file nil)

(defvar eve-module-path
  (concat (file-name-directory (or load-file-name (buffer-file-name)))  "buffer.py"))

(defun eve--add-clip (begin end)
  "Add an clip to org mode file."
  (with-current-buffer (find-file-noselect eve--org-file)
    (goto-char (point-max))
    (print begin)
    (insert (format "\n* [[eve-clip:%s-%s]]" begin end))
    (save-buffer)))

(defun eve--find-buffer (url)
  "Find opened buffer by `url'"
  (let ((opened-buffer))
    (catch 'found-match-buffer
      (dolist (buffer (buffer-list))
        (set-buffer buffer)
        (when (equal major-mode 'eaf-mode)
          (when (and (string= eaf--buffer-url url)
                     (string= eaf--buffer-app-name "video-editor"))
            (setq opened-buffer buffer)
            (throw 'found-match-buffer t)))))
    opened-buffer))

(defun eve--get-all-org-headlines ()
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

(defun eve-buffer-id ()
  "Get eaf buffer id from org mode file."
  (let* ((file-path (file-name-sans-extension (buffer-file-name)))
         (eaf-buffer (eve--find-buffer file-path)))
    (buffer-local-value 'eaf--buffer-id eaf-buffer)))

(defun eve-data-org-to-editor (&rest _)
  "Update editor data from org mode file."
  (interactive)
  (with-current-buffer (find-file-noselect eve--org-file)
    (when-let ((elements (eve-org-elements))
               (buffer-id (eve-buffer-id)))
      (eaf-call-async "execute_function_with_args"
                      buffer-id
                      "update_edit_elements" elements))))

(defun eve-image-org-link-complete-file (&optional arg)
  "Create a file link using completion.
With optional ARG \\='(16), abbreviate the file name in the link."
  (let ((file (read-file-name "eve-image: "))
        (pwd (file-name-as-directory (expand-file-name ".")))
        (pwd1 (file-name-as-directory (abbreviate-file-name
                                       (expand-file-name ".")))))
    (cond ((equal arg '(16))
           (concat "eve-image:"
                   (abbreviate-file-name (expand-file-name file))))
          ((string-match
            (concat "^" (regexp-quote pwd1) "\\(.+\\)") file)
           (concat "eve-image:" (match-string 1 file)))
          ((string-match
            (concat "^" (regexp-quote pwd) "\\(.+\\)")
            (expand-file-name file))
           (concat "eve-image:"
                   (match-string 1 (expand-file-name file))))
          (t (concat "eve-image:" file)))))

(defun eve-link-follow-function (path arg)
  "When opening an org mode link, play the clip in the eve buffer."
  (let* ((buffer-id (eve-buffer-id))
         (clip (mapcar #'string-to-number (string-split path "-"))))
    (eaf-call-async "execute_function_with_args"
                    buffer-id
                    "loop_play_in_clip"
                    (car clip)
                    (cadr clip))))

(defun eve-link-path-to-clip (path)
  "Convert org link path to a clip."
  (mapcar #'string-to-number (string-split path "-")))

(defun eve-link-path-to-image (path)
  "Convert link path to edit image."
  (let ((lst (string-split path ":")))
    (list (expand-file-name (nth 0 lst)) (string-to-number (nth 1 lst)))))

(defun eve-link-path-to-text (path)
  "Convert link path to edit text."
  (let ((lst (string-split path ":")))
    (list (nth 0 lst) (string-to-number (nth 1 lst)))))

(defun eve-open ()
  "Open an video file to edit."
  (interactive)
  (eaf-open (expand-file-name (read-file-name "Select a file: ")) "video-editor"))

(defun eve-open-in-org ()
  "Open an video file to edit."
  (interactive)
  (when (equal major-mode 'org-mode)
    (let* ((org-file (buffer-file-name (current-buffer)))
           (video-file (substring org-file 0 (- (length org-file) 4))))
      (when (file-exists-p video-file)
        (eaf-open video-file "video-editor")))))

(defun eve-parse-link (link)
  "Parse org link."
  (let ((type (org-element-property :type link))
        (path (org-element-property :path link)))
    (pcase type
      ("eve-clip" (list type (eve-link-path-to-clip path)))
      ("eve-text" (list type (eve-link-path-to-text path)))
      ("eve-image" (list type (eve-link-path-to-image path))))))

(defun eve-org-elements ()
  "Get all edit elements in org mode file."
  (with-current-buffer (find-file-noselect eve--org-file)
    (let ((headlines (eve--get-all-org-headlines))
          (export-elements)
          (element))
      (dolist (headline headlines)
        (with-temp-buffer
          (insert headline)
          (org-element-map (org-element-parse-buffer) 'link
            (lambda (link)
              (setq element (eve-parse-link link))
              (when element
                (push element export-elements))))))
      (reverse export-elements))))

(add-to-list 'eaf-app-binding-alist '("video-editor" . eve-keybinding))

(add-to-list 'eaf-app-module-path-alist '("video-editor" . eve-module-path))

(define-minor-mode eve-mode
  "A minor mode for EAF Video Editor."
  :lighter " eve"
  :keymap nil
  (if eve-mode
      (progn
        (message "EAF Video Editor Mode enabled")
        (advice-add 'save-buffer :after #'eve-data-org-to-editor))
    (message "EAF Video Editor Mode disabled")
    (advice-remove 'save-buffer #'eve-data-org-to-editor)))

(org-link-set-parameters "eve-clip"
                         :follow #'eve-link-follow-function)

(org-link-set-parameters "eve-image"
                         :complete #'eve-image-org-link-complete-file)

(org-link-set-parameters "eve-text")


(provide 'eaf-video-editor)
;;; eaf-video-editor.el ends here
