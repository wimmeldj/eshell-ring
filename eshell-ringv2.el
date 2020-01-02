;;; USAGE
;; 
;; Byte compile the file
;; 
;; (require 'eshring <path>)
;; 
;; Choose an eshring inferior map prefix key (where most of the
;; functionality is) if you don't like the default of C-b. This can
;; be done either through the customize group `eshring' or simply:
;; 
;; (defvar eshring-inferior-map-prefix (kbd "C-b"))
;; 
;; Enable global eshring mode.
;; 
;; (global-eshring-mode 1)


(require 'cl)
(require 'ring)
(require 'eshell)                       ;for its mode map


;;;; =======================================================================
;;;; Custom settings

(defgroup eshring nil
  "Eshell Ring"
  :group 'eshell)

(defcustom eshring-mode-lighter " eshr"
  "Lighter displayed in mode line when `eshring-mode' is
  enabled."
  :group 'eshring
  :type 'string)

(defcustom eshring-inferior-map-prefix (kbd "C-b")
  "The prefix key for `eshring/mode' commands."
  :group 'eshring
  :type 'sexp
  :set #'set-default)



;;;; =======================================================================
;;;; main

;;;; starts at 1 and always increases. Like tmux.
(defvar eshring/session-number
  (lexical-let ((i 0))
    (lambda () (incf i))))

;;;; the eshell ring data structure
(defvar eshring/ring (make-ring 1)
  "Used to store cons cells. `car' of each cell is either an
alias for an eshell buffer or a number assigned to the buffer on
creation. `cdr' is the eshell buffer itself.")


(defun eshring/generate-eshell-buffer ()
  "Returns cons of session-number and the eshell buffer
corresponding to that session number, e.g. (0 . #<buffer
*eshell*<0>>). Has side-effect of switching the current buffer to
the newly created eshell buffer."
  (let ((session-number (funcall eshring/session-number)))
    (cons session-number (eshell session-number))))

(defun eshring/new (&optional alias)
  "Generates a new eshell buffer. If ALIAS is provided, it must
be a string. The generated buffer will be stored on
`eshring/ring' keyed by its alias. If ALIAS is not provided, the
new buffer will be uniquely keyed by `eshring/session-number'"
  (unless (and alias
               (eshring/ring-member alias))
    (assert (or (not alias)
                (eq 'string (type-of alias))))
    (let* ((ret (eshring/generate-eshell-buffer))
           (sess-num (int-to-string (car ret)))
           (shell-buff (cdr ret))
           (item (if alias
                     (cons alias shell-buff)
                   (cons sess-num shell-buff))))
      (with-current-buffer shell-buff
        (rename-buffer (format "*eshell<%s>*" (car item))))
      (ring-insert+extend eshring/ring item t))))

(defun eshring/create-unnamed ()
  "Create an unnamed eshell buffer and store it on `eshring/ring'
with an alias given by `eshring/session-number'"
  (interactive)
  (eshring/new))

(defun eshring/get-tail ()
  "Returns tail item (most recently used) on `eshring/ring'.
 E.g. (1 . #<buffer *eshell*<1>) or
 (\"two\" . #<buffer *eshell*<2>)"
  (when (not (ring-empty-p eshring/ring))
    (ring-ref eshring/ring 0)))

(defun eshring/ring-member (key)
  "Return index of member of `eshring/ring' with an alias `equal'
to KEY"
  (catch 'found
    (dotimes (idx (ring-length eshring/ring))
      (when (equal key (car (ring-ref eshring/ring idx)))
        (throw 'found idx)))))

(defun eshring/get (key)
  "Return member of `eshring/ring' where the member's alias is
`equal' to KEY."
  (let* ((idx (eshring/ring-member key))
         (memb (when idx (ring-ref eshring/ring idx))))
    memb))

(defun eshring/get-by-buffer (buffer)
  "Return member of `eshring/ring' where BUFFER is `eq' to the
member's eshell buffer."
  (let ((idx (catch 'found
             (dotimes (i (ring-length eshring/ring))
               (when (eq buffer (cdr (ring-ref eshring/ring i)))
                 (throw 'found i))))))
       (when idx
         (ring-ref eshring/ring idx))))

(eshring/get-by-buffer "#<buffer *eshell*<17>>")

(defun eshring/goto (key)
  "Switches current buffer to the eshell buffer corresponding to
KEY. Updates ring."
  (let ((memb (eshring/get key)))
    (when memb
      (ring-remove+insert+extend eshring/ring memb t) ;update most recently used
      (pop-to-buffer-same-window (cdr memb)))
    memb))

(defun eshring/find (&optional key)
  "Switches current buffer to the eshell buffer corresponding to
KEY. If one does not exits, creates it and switches just the
same."
  (interactive (list (completing-read "Eshell Buffer: "
                                      (mapcar #'car (ring-elements eshring/ring))
                                      nil nil nil nil (car (eshring/get-tail)) t)))
  (when (eq (type-of key) 'string)
    (setq key (s-trim key)))
  (unless (eshring/goto key)
    (eshring/new key)))

(defun eshring/kill (&optional key)
  "If KEY is non-nil, kills the eshell buffer corresponding to
KEY on `eshring/ring' and removes it from the ring. If KEY is
nil, kills the buffer found at tail of `eshring/ring' (most
recently used eshell buffer)."
  (interactive (list (completing-read "Kill Eshell Buffer: "
                                      (mapcar #'car (ring-elements eshring/ring))
                                      nil nil nil nil (car (eshring/get-tail)) t)))

    (or key (setq key (car (eshring/get-tail)))) ;for non-interactive calls

    (let* ((memb (eshring/get key))
           (buff (cdr memb))
           (idx (eshring/ring-member key)))
      (when (and memb buff idx)
        (message "eshring killing buffer %s with name %s" buff (car memb))
        (ring-remove eshring/ring idx)
        (kill-buffer buff))))

(defun eshring/killall (&optional something)
  "Kills all eshell buffers on `eshring/ring' and resets the
ring."
  (interactive "p")
  (when (not (ring-empty-p eshring/ring))
    (message "eshring killing all buffers on ring.")
    (dotimes (i (ring-length eshring/ring) nil)
      (let* ((memb (ring-ref eshring/ring i))
             (buff (cdr memb)))
        (kill-buffer buff)))
    (setq eshring/ring (make-ring 1))))

(defun eshring/make-most-recent ()
  "Makes current buffer the most recently used one in
`eshring/ring'. Current buffer must be present in the ring."
  (with-current-buffer (current-buffer)
    (when (and (equal major-mode 'eshell-mode)
               (buffer-live-p (current-buffer)))
(ring-remove+insert+extend eshring/ring
                                 (eshring/get-by-buffer (current-buffer)) t))))

(defun eshring/traverse (direction ring-members)
  "Traverses eshell buffers stored in `eshring/ring' by switching
the current buffer to one either preceding or following the
tail. Returns a closure to keep the state of the current buffer
in the traversal.

DIRECTION is a symbol of either 'next or 'prev indicating initial
traversal direction.

RING-MEMBERS is a list of elements in `eshring/ring' that will be
traversed. This is used because the state of `eshring/ring'
changes each time the current buffer is switched."
  (assert (or (eq 'next direction) (eq 'prev direction))) ;no sideways
  (lexical-let* ((ring-members ring-members) ;so it can be referenced by the closure
                 (n (mod (if (eq 'next direction) 1 -1)
                         (length ring-members))))
    (switch-to-buffer (cdr (elt ring-members n))) ;initial switch on first call
    (lambda (direction)
      (assert (or (eq 'next direction) (eq 'prev direction)))
      (setq n (mod (+ n (if (eq 'next direction) 1 -1))
                   (length ring-members)))
      (let* ((memb (elt ring-members n))
             (buff (cdr memb)))
        (switch-to-buffer buff)
        (message "%S" memb)))))

(defun eshring/update-ring-with-selection (ring-members)
  "Called after the transient map returned by `eshring/next-prev' is
deactivated. Used to revert all changes to `eshring/ring' except
the final buffer selected which will now be at the tail. This has
the effect of collecting the buffers being switched between at
the tail of the ring, which is obviously desirable as it means
less switching back and forth between the buffers being used."
  (setq eshring/ring (make-ring 1))
  (dolist (memb (reverse ring-members))
    (ring-insert+extend eshring/ring memb t))
  (eshring/make-most-recent))

(defun eshring/next-prev (inc)
  "Switch between eshell buffers stored in `eshring/ring' by
traversing them as a list."
  (interactive "p")
  (lexical-let* ((base (event-basic-type last-command-event))
                 (direction (cond ((eq ?n base) 'next)
                                  ((eq ?p base) 'prev)))
                 (ring-members (ring-elements eshring/ring))
                 (f (eshring/traverse direction ring-members))) ;make first move and alias closure
    (message "Use C-n, C-p for ring traversal")
    (set-transient-map (let ((map (make-sparse-keymap)))
                         (define-key map (kbd "C-n") #'(lambda (inc) (interactive "p") (funcall f 'next)))
                         (define-key map (kbd "C-p") #'(lambda (inc) (interactive "p") (funcall f 'prev)))
                         map)
                       t
                       #'(lambda () (eshring/update-ring-with-selection ring-members)))))

;; improve or remove this
(defun eshring/visualize-ring ()
  (interactive)
   (princ (mapcar (lambda (x) (if (numberp (car x))
                      (format "%d" (car x))
                    (car x)))
           (ring-elements eshring/ring))))



;;;; =======================================================================
;;;; advice

;; meant to keep `eshring/ring' current regardless of how we get to a particular eshell buffer
(defadvice switch-to-buffer (after check-for-eshell-mode-stb activate)
  "Check if a buffer switched to is an eshell buffer so that
`eshring/ring' stays syncronized"
  (eshring/make-most-recent))
(defadvice switch-to-buffer-other-frame (after check-for-eshell-mode-swbof activate)
  "Check if a buffer switched to is an eshell buffer so that
`eshring/ring' stays syncronized"
  (eshring/make-most-recent))
(defadvice switch-to-buffer-other-window (after check-for-eshell-mode-stbow activate)
  "Check if a buffer switched to is an eshell buffer so that
`eshring/ring' stays syncronized"
  (eshring/make-most-recent))



;;;; =======================================================================
;;;; mode

(defvar eshring-map nil
  "Keymap used in `eshring-mode'")

(unless eshring-map
  (setq eshring-map
        (let ((sup-map (make-sparse-keymap))  ;superior map that inherits from `eshell-mode-map'
              (inf-map (make-sparse-keymap))) ;inferior map accessed with prefix
          ;; link inferior map to superior map by prefix
          (define-key sup-map eshring-inferior-map-prefix inf-map)
          (global-set-key eshring-inferior-map-prefix inf-map)
          (set-keymap-parent sup-map eshell-mode-map)
          ;; (use-local-map sup-map)

          ;; superior map definitions
          (define-key sup-map (kbd "C-x k") #'eshring/kill)

          ;; inferior map definitions
          (define-key inf-map (kbd "C-n") #'eshring/next-prev)
          (define-key inf-map (kbd "C-p") #'eshring/next-prev)
          (define-key inf-map (kbd "C-v") #'eshring/visualize-ring)
          (define-key inf-map (kbd "C-f") #'eshring/find)
          (define-key inf-map (kbd "C-<return>") #'eshring/create-unnamed)
          sup-map)))


;;;###autoload
(define-minor-mode eshring-mode
  "Toggle Esh ring mode in current buffer.
With prefix ARG, enable eshring-mode if ARG is positive;
otherwise, disable it."
  nil
  eshring-mode-lighter 
  eshring-map
  ;; code we want to run each time enabled or disabled?
  )

(defun turn-on-eshring-mode ()
  "Enable `eshring-mode' in the current buffer if the major mode
is `eshell-mode'."
  (when (equal major-mode 'eshell-mode)
    (eshring-mode 1)))

;;;###autoload
(define-globalized-minor-mode global-eshring-mode
  eshring-mode turn-on-eshring-mode)

(provide 'eshring)
