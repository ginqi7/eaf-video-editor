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
    ("o" . "convert_clips_to_video"))

  "The keybinding of EAF Video Player."
  :type 'cons)

(defvar eaf-video-editor--org-file nil)

(defun eaf-video-editor--add-clip (begin end)
  (print "fuck")
  (with-current-buffer (find-file-noselect eaf-video-editor--org-file)
    (goto-char (point-max))
    (insert (format "\n* [[clip:%s-%s]]" begin end))
    (save-buffer)))

(add-to-list 'eaf-app-binding-alist '("video-editor" . eaf-video-editor-keybinding))

(setq eaf-video-editor-module-path (concat (file-name-directory (or load-file-name (buffer-file-name)))  "buffer.py"))

(add-to-list 'eaf-app-module-path-alist '("video-editor" . eaf-video-editor-module-path))

(defun eaf-video-editor-open ()
  (interactive)
  (eaf-open (expand-file-name "~/Movies/hello.mkv") "video-editor"))

(provide 'eaf-video-editor)
;;; eaf-video-editor.el ends here
