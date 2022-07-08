;;; view-lock-mode.el --- View Lock mode -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Shiina fubuki

;; Author: Shiina fubuki <fubukiATfrill.org>
;; Keywords: environment
;; Version: $Revision: 1.22 $

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

;; (setq view-lock-start-time 180)
;; (require 'view-lock-mode)

;; Other options see (customize-group 'view-lock-mode)

;;; Code:
(when (require 'view nil 'noerror)
  (setq view-read-only t) ; in simple.el
  (add-hook 'view-mode-hook #'view-lock-timer-setup))

(defgroup view-lock-mode nil
  "view-mode Lock mode."
  :version "29"
  :prefix  "view-lock-"
  :group   'environment)

(defcustom view-lock-start-time (* 5 60)
  "View Lock idle time seconds."
  :type  'integer
  :group 'view-lock-mode)

(defcustom view-lock-lighter " View-Lock"
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

(defface view-lock-highlight
  '((t :inherit mode-line-inactive :foreground "plum" :weight bold))
  "View mode highlight face."
  :group 'view-lock-mode
  :group 'faces)

(defface view-lock-lock-highlight
  '((t :inherit mode-line-inactive :foreground "deep sky blue" :weight bold))
  "View mode lock face."
  :group 'view-lock-mode
  :group 'faces)

;; モードライン VC インジケータの face.
;; RCS 等の Lock系の場合リポジトリが Unlock もしくは他者による Lock、
;; つまり編集「不可」状態 ならこの face になり(-).
;; Git 等マージ系なら未編集ならこの face(-)、編集を始めると(更新されると)元に戻る(:).
(defface view-lock-vc
  '((t :inherit view-lock-highlight :weight normal))
  "VC mode line stat face."
  :group 'view-lock-mode
  :group 'faces)

(defvar view-lock-timer nil "Local work variable")

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
  (and view-lock-timer (cancel-timer view-lock-timer)))

(defun view-lock-idle-timer ()
  (run-with-idle-timer
   view-lock-start-time nil #'view-lock-start (current-buffer)))

(defun view-lock-timer-setup ()
  "For `view-mode-hook' function.
Start `view-lock-idle-timer' and Highlight View mode indicator."
  (cond
   (view-mode
    (view-lock-ligher 'view-lock-highlight)
    (setq-local view-lock-timer (view-lock-idle-timer)))
   (t
    (and view-lock-mode (view-lock-mode -1))
    (view-lock-cancel-timer))))

(defun view-lock-kill-timer ()
  (view-lock-cancel-timer))

(add-hook 'kill-buffer-hook #'view-lock-kill-timer)

(defun view-lock-start (buff)
  (and (buffer-live-p buff) (with-current-buffer buff (view-lock-mode))))

(defun view-lock-message ()
  (interactive)
  (ding)
  (message "%s"
           (substitute-command-keys
            "View mode is Locked. type \\[view-lock-quit] to lock release.")))

(defun view-lock-quit (prefix)
  (interactive "P")
  (view-lock-mode -1)
  (if prefix
      (setq-local view-lock-timer (view-lock-idle-timer))
    (view-mode -1)
    (setq buffer-read-only nil)))

(defun view-lock-release ()
  (interactive)
  (view-lock-quit 'prefix))

(defvar view-lock-mode-map
  (let ((map  (make-sparse-keymap))
        (menu (make-sparse-keymap "view-lock")))
    (define-key map [remap self-insert-command] #'view-lock-message)
    (define-key map "\C-m" #'view-lock-message)
    (define-key map "\C-j" #'view-lock-message)
    (define-key map "\M-q" #'view-lock-quit)
    (define-key map "\C-x\C-q" #'view-lock-quit)
    (define-key map [menu-bar view-lock] (cons "View-Lock" menu))
    (define-key menu [view-lock-release] '("Release Only" . view-lock-release))
    (define-key menu [view-lock-quit]
                '(menu-item "Quit View Mode" view-lock-quit
                            :help "With prefix lock release only"))
    map)
  "View Lock mode Keymap.")

(define-minor-mode view-lock-mode
  "Key is locked if no operation for a period of time in `view-mode'.
Triger Time is specified in seconds in `view-lock-start-time'.
\\<view-lock-mode-map>
Press \\[view-lock-quit] to cancel. For unlocking only, with prefix key.
\\{view-lock-mode-map}"
  :lighter view-lock-lighter
  (cond
   (view-lock-mode
    (view-lock-cancel-timer)
    (and view-lock-current-input-method
         current-input-method (toggle-input-method))
    (make-local-variable 'minor-mode-alist)
    (view-lock-ligher 'view-lock-lock-highlight)
    (setq minor-mode-overriding-map-alist
          (list (cons 'view-mode view-lock-mode-map)
                '(view-lock-mode . nil))))
   (t
    (and (local-variable-p 'minor-mode-alist)
         (kill-local-variable 'minor-mode-alist))
    (setq minor-mode-overriding-map-alist nil))))

(provide 'view-lock-mode)
;; fin.
