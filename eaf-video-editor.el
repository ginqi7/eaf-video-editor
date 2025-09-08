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

  "The keybinding of EAF Video Editor."
  :type 'cons)

(defvar eaf-video-editor--org-file nil)

(defvar eaf-video-editor-module-path
  (concat (file-name-directory (or load-file-name (buffer-file-name)))  "buffer.py"))

(add-to-list 'eaf-app-binding-alist '("video-editor" . eaf-video-editor-keybinding))
(add-to-list 'eaf-app-module-path-alist '("video-editor" . eaf-video-editor-module-path))


(org-link-set-parameters "eaf-ve-clip"
                         :follow #'eaf-video-editor-link-follow-function)

(org-link-set-parameters "eaf-ve-text")
(org-link-set-parameters "eaf-ve-image"
                         :complete #'eaf-ve-image-org-link-complete-file)


(defun eaf-ve-image-org-link-complete-file (&optional arg)
  "Create a file link using completion.
With optional ARG \\='(16), abbreviate the file name in the link."
  (let ((file (read-file-name "eaf-ve-image: "))
        (pwd (file-name-as-directory (expand-file-name ".")))
        (pwd1 (file-name-as-directory (abbreviate-file-name
                                       (expand-file-name ".")))))
    (cond ((equal arg '(16))
           (concat "eaf-ve-image:"
                   (abbreviate-file-name (expand-file-name file))))
          ((string-match
            (concat "^" (regexp-quote pwd1) "\\(.+\\)") file)
           (concat "eaf-ve-image:" (match-string 1 file)))
          ((string-match
            (concat "^" (regexp-quote pwd) "\\(.+\\)")
            (expand-file-name file))
           (concat "eaf-ve-image:"
                   (match-string 1 (expand-file-name file))))
          (t (concat "eaf-ve-image:" file)))))

(defun eaf-video-editor-buffer-id ()
  "Get eaf buffer id from org mode file."
  (let* ((file-path (file-name-sans-extension (buffer-file-name)))
         (eaf-buffer (eaf-video-editor--find-buffer file-path)))
    (buffer-local-value 'eaf--buffer-id eaf-buffer)))

(defun eaf-video-editor-link-path-to-clip (path)
  "Convert org link path to a clip."
  (mapcar #'string-to-number (string-split path "-")))

(defun eaf-video-editor-link-follow-function (path arg)
  "When opening an org mode link, play the clip in the eaf-video-editor buffer."
  (let* ((buffer-id (eaf-video-editor-buffer-id))
         (clip (mapcar #'string-to-number (string-split path "-"))))
    (eaf-call-async "execute_function_with_args"
                    buffer-id
                    "loop_play_in_clip"
                    (car clip)
                    (cadr clip))))

(defun eaf-video-editor--add-clip (begin end)
  "Add an clip to org mode file."
  (with-current-buffer (find-file-noselect eaf-video-editor--org-file)
    (goto-char (point-max))
    (print begin)
    (insert (format "\n* [[eaf-ve-clip:%s-%s]]" begin end))
    (save-buffer)))

(defun eaf-video-editor-open ()
  "Open an video file to edit."
  (interactive)
  (eaf-open (expand-file-name (read-file-name "Select a file: ")) "video-editor"))

(defun eaf-video-editor-open-in-org ()
  "Open an video file to edit."
  (interactive)
  (when (equal major-mode 'org-mode)
    (let* ((org-file (buffer-file-name (current-buffer)))
           (video-file (substring org-file 0 (- (length org-file) 4))))
      (when (file-exists-p video-file)
        (eaf-open video-file "video-editor")))))


(defun eaf-video-editor-parse-link (link)
  "Parse org link."
  (let ((type (org-element-property :type link))
        (path (org-element-property :path link)))
    (pcase type
      ("eaf-ve-clip" (list type (eaf-video-editor-link-path-to-clip path)))
      ("eaf-ve-text" (list type (eaf-video-editor-link-path-to-text path)))
      ("eaf-ve-image" (list type (eaf-video-editor-link-path-to-image path))))))

(defun eaf-video-editor-link-path-to-text (path)
  "Convert link path to edit text."
  (let ((lst (string-split path ":")))
    (list (nth 0 lst) (string-to-number (nth 1 lst)))))

(defun eaf-video-editor-link-path-to-image (path)
  "Convert link path to edit image."
  (let ((lst (string-split path ":")))
    (list (expand-file-name (nth 0 lst)) (string-to-number (nth 1 lst)))))



(defun eaf-video-editor-org-elements ()
  "Get all edit elements in org mode file."
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

(defun eaf-video-editor-data-from-org-to-editor (&rest _)
  "Update editor data from org mode file."
  (interactive)
  (with-current-buffer (find-file-noselect eaf-video-editor--org-file)
    (when-let ((elements (eaf-video-editor-org-elements))
               (buffer-id (eaf-video-editor-buffer-id)))
      (eaf-call-async "execute_function_with_args"
                      buffer-id
                      "update_edit_elements" elements))))

(define-minor-mode eaf-video-editor-mode
  "A minor mode for eaf-video-editor."
  :lighter " EAF-video-editor"
  :keymap nil
  (if eaf-video-editor-mode
      (progn
        (message "EAF Video Editor Mode enabled")
        (advice-add 'save-buffer :after #'eaf-video-editor-data-from-org-to-editor))
    (message "EAF Video Editor Mode disabled")
    (advice-remove 'save-buffer #'eaf-video-editor-data-from-org-to-editor)))


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

(defun eaf-video-editor--find-buffer (url)
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


(provide 'eaf-video-editor)
;;; eaf-video-editor.el ends here
