;;; eshell-ring-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads nil "eshell-ring" "eshell-ring.el" (24262 65318
;;;;;;  395388 126000))
;;; Generated autoloads from eshell-ring.el

(autoload 'eshring-mode "eshell-ring" "\
Toggle Esh ring mode in current buffer.
With prefix ARG, enable eshring-mode if ARG is positive;
otherwise, disable it.

\(fn &optional ARG)" t nil)

(defvar global-eshring-mode nil "\
Non-nil if Global Eshring mode is enabled.
See the `global-eshring-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-eshring-mode'.")

(custom-autoload 'global-eshring-mode "eshell-ring" nil)

(autoload 'global-eshring-mode "eshell-ring" "\
Toggle Eshring mode in all buffers.
With prefix ARG, enable Global Eshring mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Eshring mode is enabled in all buffers where
`turn-on-eshring-mode' would do it.
See `eshring-mode' for more information on Eshring mode.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "eshell-ring" '(#("turn-on-eshring-mode" 0 20 (fontified nil)) #("eshring" 0 7 (fontified nil)) #("ibuffer-do-kill-on-deletion-marks" 0 33 (fontified nil)))))

;;;***

(provide 'eshell-ring-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; eshell-ring-autoloads.el ends here
