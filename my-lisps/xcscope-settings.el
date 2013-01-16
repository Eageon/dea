(require 'xcscope)

(add-hook 'c-mode-common-hook '(lambda() (require 'xcscope)))

(provide 'xcscope-settings)
