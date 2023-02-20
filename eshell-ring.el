;;; eshell-ring.el --- Organize eshell buffers on a ring -*- lexical-binding: t; -*-

;; this should be re-written. Should work with emacs tabs feature in 27.1
;; also, keymap is bad. Fix the requirement of defining a prefix key.
;;
;; Also, instead of redefining buffer killing functions to also check if the buffer is on the shell
;; ring, you should just switch shell ring buffers lazily. That is, if attempting to switch to a
;; killed, buffer, discard that entry, goto the next one, repeat.

;;; USAGE
;; 
;; Byte compile the file
;; 
;; (require 'eshell-ring <path>)
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


(require 's)
(require 'cl)
(require 'ring)
(require 'ibuffer)

(defvar eshell-mode-map nil) ;because now, requiring eshell does not include the mode-map

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

(defcustom eshring-inferior-map-prefix (kbd "<M-f11>")
  "The prefix key for `eshring/mode' commands."
  :group 'eshring
  :type 'sexp
  :set #'set-default)



;;;; =======================================================================
;;;; main

(defvar eshring/session-number
  (let ((i 0))
    (lambda () (incf i)))
  "Starts at 1 and always increases.")

;;;; the eshell ring data structure
(defvar eshring/ring (make-ring 1)
  "Stores cons cells (string . buffer). `car' of each cell is
either an alias for an eshell buffer or a number assigned to the
buffer on creation. `cdr' is the eshell buffer itself.")


(defun eshring/generate-eshell-buffer ()
  "Returns cons of session-number and the eshell buffer
corresponding to that session number, e.g. (0 . #<buffer
*eshell*<0>>). Has side-effect of switching the current buffer to
the newly created eshell buffer."
  (let ((session-number (funcall eshring/session-number)))
    (cons session-number (eshell session-number))))


(defun eshring/new-eshell (&optional alias)
  "Generates a new eshell buffer. If ALIAS is provided, it must
be a string. The generated buffer will be stored on
`eshring/ring' keyed by its alias. If ALIAS is not provided, the
new buffer will be uniquely keyed by `eshring/session-number'"
  (unless (and alias (eshring/ring-member alias))
    (let* ((ret (eshring/generate-eshell-buffer))
           (sess-num (int-to-string (car ret)))
           (shell-buff (cdr ret))
           (item (if alias
                     (cons alias shell-buff)
                   (cons sess-num shell-buff))))
      (with-current-buffer shell-buff
        (rename-buffer (format "*eshell<%s>*" (car item))))
      (ring-insert+extend eshring/ring item t))))


(defun eshring/new-shell (&optional alias)
  "Same as `eshring/new-eshell', except using a `shell' buffer."
  ;; `shell' requires the buffer name to correspond to a buffer that already exists
  (unless (and alias (eshring/ring-member alias))
    (let* ((sess-num (int-to-string (funcall eshring/session-number)))

           ;; (buff-name (format "*shell <%s>*" (or alias sess-num)))

           ;; TODO: this is what we'd like to do. Need to avoid calling `shell'
           ;; and handle linking a buffer to a new shell process ourselves probably

           ;; (empty-buff (generate-new-buffer buff-name)) 

           ;; create a shell buffer with the `current-prefix-arg' set to
           ;; universal arg in order to force creation of new shell process
           ;; rather than just joining the pre-existing one to empty-buff
           (shell-buff (let ((current-prefix-arg 4))
                         (call-interactively #'shell
                                             ;; empty-buff
                                             )))
           (ring-memb (cons (or alias sess-num)
                            shell-buff)))
      (ring-insert+extend eshring/ring ring-memb t))))


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
  "Evals to member of `eshring/ring' where BUFFER is `eq' to the
member's eshell buffer."
  (let ((idx (catch 'found
               (dotimes (i (ring-length eshring/ring))
                 (when (eq buffer (cdr (ring-ref eshring/ring i)))
                   (throw 'found i))))))
    (when idx
      (ring-ref eshring/ring idx))))


(defun eshring/goto (key)
  "Switches current buffer to the eshell buffer corresponding to
KEY. Updates ring."
  (let ((memb (eshring/get key)))
    (when memb
      (ring-remove+insert+extend eshring/ring memb t) ;update most recently used
      (pop-to-buffer-same-window (cdr memb)))
    memb))


(defun eshring/make-most-recent ()
  "Makes current buffer the most recently used one in
`eshring/ring'. Current buffer must be present in the ring."
  (with-current-buffer (current-buffer)
    (when (and (or (eq major-mode 'eshell-mode)
                   (eq major-mode 'shell-mode))
               (buffer-live-p (current-buffer)))
      (ring-remove+insert+extend eshring/ring
                                 (eshring/get-by-buffer (current-buffer)) t))))


(defun eshring/overwrite-ring (ring-members)
  "Overwrites `eshring/ring' with elements in ring-members. Calls
`eshring/make-most-recent' before exit."
  (setq eshring/ring (make-ring 1))
  (dolist (memb (reverse ring-members))
    (ring-insert+extend eshring/ring memb t))
  (eshring/make-most-recent))


(defun eshring/create-unnamed-eshell ()
  "Create an unnamed eshell buffer and store it on `eshring/ring'
with an alias given by `eshring/session-number'"
  (interactive)
  (eshring/new-eshell))


(defun eshring/create-unnamed-shell ()
  "Same as `eshring/create-unnamed-eshell' except for `shell'
buffers"
  (interactive)
  (eshring/new-shell))


(defun eshring/find (&optional key)
  "Switches current buffer to the eshell buffer corresponding to
KEY. If one does not exist, creates it and switches just the
same."
  (interactive (list (completing-read "Eshell Buffer: "
                                      (mapcar #'car (ring-elements eshring/ring))
                                      nil nil nil nil (car (eshring/get-tail)) t)))
  (when (eq (type-of key) 'string)
    (setq key (s-trim key)))
  (unless (eshring/goto key)
    (eshring/new-eshell key)))


;; not currently used due to issues arrising when using completing read on
;; shell/eshell buffers not on a ring. Such as those spawned by
;; `dired-do-async-shell-command'
(defun eshring/kill-completing (&optional key)
  "If KEY is non-nil, kills the eshell buffer corresponding to
KEY on `eshring/ring' and removes it from the ring. Otherwise,
kills the buffer found at tail of `eshring/ring' (most recently
used buffer)."
  (interactive (list (completing-read "Kill Eshell Buffer: "
                                      (mapcar #'car (ring-elements eshring/ring))
                                      nil nil nil nil (car (eshring/get-tail)) t)))
  (unless key (setq key (car (eshring/get-tail)))) ;for non-interactive calls
  (let* ((memb (eshring/get key))
         (buff (cdr memb))
         (idx (eshring/ring-member key)))
    (if (and memb buff idx)
	(progn
	  (message "eshring killing buffer %s with name %s" buff (car memb))
	  (when (kill-buffer buff)
	    (ring-remove eshring/ring idx)))
      (kill-buffer nil))))

(defun eshring/kill (&optional key)
  (interactive)
  (if key			 ;if called with no key, try kill current buffer
      (let* ((memb (eshring/get key))
	     (buff (cdr memb))
	     (idx (eshring/ring-member key)))
	(when (and memb buff idx)
	  (message "eshring killing buffer %s with name %s" buff (car memb))
	  (when (kill-buffer buff)
	    (ring-remove eshring/ring idx))))
    (let* ((buff (current-buffer))
	   (memb (eshring/get-by-buffer buff))
	   (idx 0)) ;; if memb is nil, the buff is not on the ring. Otherwise, it must be first elt
      (if memb
	  (progn
	    (message "eshring killing buffer %s with name %s" buff (car memb))
	    (when (kill-buffer buff)
	      (ring-remove eshring/ring idx)))
	(kill-buffer nil)))))

(defun eshring/killall ()
  "Kills all eshell buffers on `eshring/ring' and resets the
ring."
  (interactive)
  (unless (ring-empty-p eshring/ring)
    (message "eshring killing all buffers on ring.")
    (mapc #'(lambda (memb) (kill-buffer (cdr memb)))
          (ring-elements eshring/ring))
    (setq eshring/ring (make-ring 1))))


(defun eshring/traverse ()
  "Evals to closure expecting a DIRECTION symbol of form 'next or
'prev and a list of elements to traverse, RING-MEMBERS. The
purpose of RING-MEMBERS is to provide the illusion of
`eshring/ring' being temporarily static while it's traversed."
  (let ((n 0) memb buff old)
    (lambda (direction ring-members)
      (setq n (mod (+ n (if (eq 'next direction) 1 -1))
                   (length ring-members))
            memb (elt ring-members n)
            buff (cdr memb)
            old (current-buffer))
      (switch-to-buffer buff)
      (bury-buffer old)
      (message "%S" memb))))


(defun eshring/next-prev ()
  "Switch between buffers stored in `eshring/ring' by traversing
them as a list. Updates state of ring when done traversing."
  (interactive)
  (unless (eql (ring-length eshring/ring) 0)
    (let* ((base (event-basic-type last-command-event))
           (direction (cond ((eq ?n base) 'next)
                            ((eq ?p base) 'prev)))
           (ring-members (ring-elements eshring/ring))
           (move (eshring/traverse)))       ;alias closure
      (funcall move direction ring-members) ;initial move
      (message "Use C-n, C-p for ring traversal")
      (set-transient-map (let ((map (make-sparse-keymap)))
                           (define-key map (kbd "C-n") #'(lambda () (interactive) (funcall move 'next ring-members)))
                           (define-key map (kbd "C-p") #'(lambda () (interactive) (funcall move 'prev ring-members)))
                           map)
                         t
                         #'(lambda () (eshring/overwrite-ring ring-members))))))


;;;; =======================================================================
;;;; advice

;; meant to keep `eshring/ring' current regardless of how we get to a particular eshell buffer
(defadvice switch-to-buffer (after eshring/stb activate)
  "Check if a buffer switched to is an eshell buffer so that
`eshring/ring' stays syncronized"
  (eshring/make-most-recent))
(defadvice switch-to-buffer-other-frame (after eshring/swbof activate)
  "Check if a buffer switched to is an eshell buffer so that
`eshring/ring' stays syncronized"
  (eshring/make-most-recent))
(defadvice switch-to-buffer-other-window (after eshring/stbow activate)
  "Check if a buffer switched to is an eshell buffer so that
`eshring/ring' stays syncronized"
  (eshring/make-most-recent))
(defadvice eshell-life-is-too-much (before eshring/life-is-too-much activate)
  "If `eshell-kill-on-exit' is non-nil, will remove the current
  buffer from `eshehll/ring'"
  (when (and eshell-kill-on-exit
             (eq (current-buffer)
                 (cdr (eshring/get-tail))))
    (ring-remove eshring/ring 0)))


;;;; =======================================================================
;;;; function redefinitions

(define-ibuffer-op ibuffer-do-kill-on-deletion-marks ()
  "Redefines that found in `ibuffer' to handle popping eshell
  buffers from `eshring/ring' as well as killing normal buffers"
  (:opstring "killed"
             :active-opstring "kill"
             :dangerous t
             :complex t
             :mark :deletion
             :modifier-p t)
  (with-current-buffer buf
    (if (or (eq major-mode 'eshell-mode)
            (eq major-mode 'shell-mode))
        (let* ((memb (eshring/get-by-buffer buf))
               (key (car memb)))
          (eshring/kill key))
      (kill-buffer buf))))


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

          ;; superior map definitions
          (define-key sup-map (kbd "C-x k") #'eshring/kill)

          ;; inferior map definitions
          (define-key inf-map (kbd "n") #'eshring/next-prev)
          (define-key inf-map (kbd "p") #'eshring/next-prev)
          (define-key inf-map (kbd "f") #'eshring/find)
          (define-key inf-map (kbd "<return>") #'eshring/create-unnamed-eshell)
          (define-key inf-map (kbd "<backspace>") #'eshring/create-unnamed-shell)
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
is `eshell-mode' or `shell-mode'."
  (when (or (eq major-mode 'eshell-mode)
            (eq major-mode 'shell-mode))
    (eshring-mode 1)))

;;;###autoload
(define-globalized-minor-mode global-eshring-mode
  eshring-mode turn-on-eshring-mode)

(provide 'eshell-ring)
