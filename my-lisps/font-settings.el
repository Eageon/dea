;; -*- Emacs-Lisp -*-

;; Time-stamp: <2013-01-16 22:38:56 Wednesday by yj>

(when (and window-system is-after-emacs-23)
  (require 'my-fontset-win)
  (if mswin
      ;; (huangq-fontset-courier 17)
      (huangq-fontset-consolas 17)
    ;; (huangq-fontset-dejavu 17)))
    ;; (huangq-fontset-fixedsys 17)
    (set-default-font "Monospace 11")))
(set-frame-font "WenQuanYi Micro Hei Mono 12")

(provide 'font-settings)
