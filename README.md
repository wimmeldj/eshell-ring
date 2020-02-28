# eshell-ring
Emacs. Organizes eshell and shell buffers by storing them on a ring. This makes
managing multiple instances of shells/eshells much easier.

- `C-b` prefix key
  - `C-n` next shell buffer
  - `C-p` prev shell buffer
  - `C-f` find a shell buffer. If name supplied doesn't match one currently
    stored on the ring, will create a new _eshell_ buffer with that name.
  - `C-RET` create unnamed (named by incremented number) eshell buffer and store
    it on the ring.
  - `C-DEL` do the same as `C-RET`, but create a _shell_ buffer
- `C-x k` is overwritten by this mode's local map so that it not only kills the
  current buffer, but also removes it from the shell ring.


- `ibuffer-do-kill-on-deletion-marks` is redefined so that buffers killed from
  ibuffer will also be removed from the shell ring if they're eshell or shell
  mode buffers.
