;;; view-lock-mode.el --- View Lock mode -*- lexical-binding: t; -*-

;; Copyright (C) 2022, 2023, 2024 Shiina fubuki

;; Author: Shiina fubuki <fubuki AT frill.org>
;; Keywords: environment
;; Version: @(#)$Revision: 2.11 $

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

;; View Lock minor mode
;; Locks `view-mode' after the specified time.

;; (setq view-read-only t)
;; (autoload 'view-lock-mode "view-lock-mode" nil t)
;; (add-hook 'view-mode-hook #'(lambda () (view-lock-mode (if view-mode 1 -1))))

;; Other options see (customize-group 'view-lock-mode)

;;; Code:
(require 'view)

(defgroup view-lock-mode nil
  "view-mode Lock mode."
  :version "30"
  :prefix  "view-lock-"
  :group   'view)

(defcustom view-lock-start-time (* 5 60)
  "View Lock idle time seconds."
  :type  'integer
  :group 'view-lock-mode)

(defcustom view-lock-lighter " View"
  "view-lock-mode lighter."
  :type '(choice string (const nil))
  :group 'view-lock-mode)

(defcustom view-lock-current-input-method t
  "Disable IME at the same time as Lock."
  :type  'boolean
  :group 'view-lock-mode)

(defcustom view-lock-vc-faces '(vc-up-to-date-state vc-locked-state)
  "VC faces to chnage or disable (nil)."
  :type '(choice (repeat face) (const nil))
  :group 'view-lock-mode)

(defcustom view-lock-shade t
  "Shade the screen."
  :type  'boolean
  :group 'view-lock-mode)

(defcustom view-lock-shade-priority 65536
  "View lock shade overlay priority."
  :type 'integer
  :group 'view-lock-mode)

(defgroup view-lock-mode-faces nil
  "view-mode Lock mode face."
  :group 'view-lock-mode
  :group 'faces)

(defface view-lock-highlight
  '((((background dark))
     (:inherit mode-line-inactive :foreground "plum" :weight bold))
    (t :inherit mode-line-inactive :foreground "gold4" :weight bold))
  "View mode highlight face."
  :group 'view-lock-mode-faces)

(defface view-lock-lock-highlight
  '((((background dark))
     (:inherit mode-line-inactive :foreground "deep sky blue" :weight bold))
    (t :inherit mode-line-inactive :foreground "green4" :weight bold))
  "View mode lock face."
  :group 'view-lock-mode-faces)

(defface view-lock-shade
  '((((background dark))
     (:inherit shadow :foreground "dark slate blue"))
    (t :foreground "grey90"))
  "view lock shade face."
  :group 'view-lock-mode-faces)

;; モードライン VC インジケータの face.
;; RCS 等の Lock系の場合リポジトリが Unlock もしくは他者による Lock、
;; つまり編集「不可」状態 ならこの face になり(-).
;; Git 等マージ系なら未編集ならこの face(-)、編集を始めると(更新されると)元に戻る(:).
(defface view-lock-vc
  '((t :inherit view-lock-highlight :weight normal))
  "VC mode line stat face."
  :group 'view-lock-mode-faces)

(defvar-local view-lock-timer nil "Local work variable")
(defvar-local view-lock-active nil)
(defvar-local view-lock-shade-ov nil)

(when (and view-lock-vc-faces (boundp 'face-remapping-alist))
  (setq face-remapping-alist
        (append 
         (mapcar #'(lambda (face) (cons face 'view-lock-vc))  view-lock-vc-faces)
         face-remapping-alist)))

(defun view-lock-ligher (face)
  "It's a definite decision for now."
  (let ((mode (assq 'view-mode minor-mode-alist)))
    (and mode
         (setq minor-mode-alist
               (cons `(view-mode (" " ,(propertize "View" 'face face)))
                     (remove mode minor-mode-alist))))))

(defun view-lock-cancel-timer ()
  (when view-lock-timer
    (cancel-timer view-lock-timer)))

(defun view-lock-timer-start ()
  (run-with-idle-timer
   view-lock-start-time nil #'view-lock-active (current-buffer)))

(defun view-lock-kill-timer ()
  (view-lock-cancel-timer))

(defun view-lock-active (buff)
  (and (buffer-live-p buff)
       (view-lock-key-lock buff)))

(defun view-lock-message ()
  (interactive)
  (ding)
  (message "%s"
           (substitute-command-keys
            "View mode is Locked. type \\[read-only-mode] to lock release.")))

(defun view-lock-restart (prefix)
  "ロック解除しロックタイマーが再スタートされる.
PREFIX 在りではロック解除のみで再スタートはされない."
  (interactive "P")
  (view-lock-mode -1)
  (or prefix (view-lock-mode 1)))

(defun view-lock-shade (buff &optional open)
  (with-current-buffer buff
    (let ((beg (point-min))
          (end (point-max)))
      (if (or open view-lock-shade-ov)
          (progn
            (and view-lock-shade-ov
                 (delete-overlay view-lock-shade-ov))
            (setq view-lock-shade-ov nil))
        (setq view-lock-shade-ov (make-overlay beg end buff))
        (overlay-put view-lock-shade-ov 'category 'view-lock)
        (overlay-put view-lock-shade-ov 'priority view-lock-shade-priority)
        (overlay-put view-lock-shade-ov 'face 'view-lock-shade)))))

(defvar view-mode-override-menu-map
  (let ((map (make-sparse-keymap "view-lock")))
    ;; (define-key map [view-lock-mode] '("Disable" . view-lock-mode))
    (define-key map [view-lock-restart]
                '(menu-item "Lock Release" view-lock-restart
                            :enable view-lock-active))
    (define-key map [read-only-mode]
                '(menu-item "Read Only Mode" read-only-mode
                            :button (:toggle . buffer-read-only)))
    map))

(fset 'view-mode-override-menu-map view-mode-override-menu-map)

(defvar view-mode-override-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap self-insert-command] #'view-lock-message)
    (define-key map "\C-m" #'view-lock-message)
    (define-key map "\C-j" #'view-lock-message)
    (define-key map "\M-q" #'view-lock-restart)
    (define-key map [menu-bar view-lock] (cons "View-Lock" view-mode-override-menu-map))
    map)
  "View Lock mode override Keymap.")

(defvar view-mode-override-mode-line-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line down-mouse-1] #'view-mode-override-menu-map)
    map))

(defmacro view-lock-lighter-property (face echo)
  `(list pre (list :propertize post
                   'face ,face
                   'keymap view-mode-override-mode-line-map
                   'mouse-face 'mode-line-highlight
                   'help-echo ,echo)))

(defun view-lock-mode-lighter ()
  (let* ((pre (substring view-lock-lighter 0 1))
         (post (substring view-lock-lighter 1))
         (active (view-lock-lighter-property 'view-lock-lock-highlight "View Lock Active"))
         (inactive (view-lock-lighter-property 'view-lock-highlight "View Lock Inactive")))
    (list 'view-lock-active active inactive)))
 
(defvar view-lock-mode-lighter (view-lock-mode-lighter))
(put 'view-lock-mode-lighter 'risky-local-variable t)

(define-minor-mode view-lock-mode
  "Key is locked if no operation for a period of time in `view-mode'.
Triger Time is specified in seconds in `view-lock-start-time'.
Press \\[view-lock-mode] to cancel. For unlocking only, with prefix key.
\\{view-mode-override-map}"
  :lighter view-lock-mode-lighter
  (cond
   (view-lock-mode
    (setq-local view-lock-timer (view-lock-timer-start))
    (and view-lock-current-input-method
         current-input-method (toggle-input-method))
    (or (local-variable-p 'minor-mode-alist)
        (make-local-variable 'minor-mode-alist))
    (setq minor-mode-alist
          (remove (assq 'view-mode minor-mode-alist) minor-mode-alist))
     (add-hook 'kill-buffer-hook #'view-lock-kill-timer))
   (t
    (view-lock-cancel-timer)
    (and view-lock-shade (view-lock-shade (current-buffer) 'open))
    (setq view-lock-active nil)
    (and (local-variable-p 'minor-mode-alist)
         (kill-local-variable 'minor-mode-alist))
    (setq minor-mode-overriding-map-alist nil)
    (remove-hook 'kill-buffer-hook #'view-lock-kill-timer))))

(defun view-lock-key-lock (buff)
  (with-current-buffer buff
    (setq view-lock-active t)
    (and view-lock-shade (view-lock-shade buff))
    (force-mode-line-update)
    (setq minor-mode-overriding-map-alist
          `((view-mode      . ,view-mode-override-map)
            (read-only-mode . ,view-mode-override-map)))))

(provide 'view-lock-mode)
;; fin.
