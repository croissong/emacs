;;; my-hydras.el --- All my hydras  -*- lexical-binding: t; -*-



(require 'hydra)

(defvar my-hydras--stack nil)

(defun my-hydras--push (expr)
  (push `(lambda () ,expr) my-hydras--stack))

(defun my-hydras--pop ()
  (interactive)
  (let ((x (pop my-hydras--stack)))
    (when x
      (funcall x))))

(defhydra my-hydras-files (:color blue)
  ""
  ("e" (find-file (expand-file-name "init.org" user-emacs-directory)) "emacs" :column "config")
  ("n" (find-file (substitute-in-file-name "$DOTFILES/dot_config/nyxt/init.org")) "nyxt" :column "config"))

(defhydra my-hydras-misc (:color blue)
  ""
  ("+" (call-interactively 'text-scale-adjust) :column "zoom")
  ("-" (call-interactively 'text-scale-adjust) :column "zoom")
  ("0" (call-interactively 'text-scale-adjust) :column "zoom")

  ("s" (progn
         (my-hydras--straight/body)
         (my-hydras--push '(my-hydras-code/body))) "straight.." :column "more")
  ("m" (progn
         (my-hydras--macro/body)
         (my-hydras--push '(my-hydras-code/body))) "macro.." :column "more"))

(defhydra my-hydras-code (:color blue)
  ""
  ("f" (call-interactively 'format-all-buffer) "format" :column "edit")
  ("r" (call-interactively 'isearch-forward-regexp) "search-replace" :column "edit")

  ("1" (crux-rename-file-and-buffer) "rename" :column "file")
  ("2" (crux-delete-file-and-buffer) "delete" :column "file")
  ("3" (call-interactively 'crux-copy-file-preserve-attributes) "copy" :column "file")
  ("5" (call-interactively 'set-buffer-file-coding-system) "encoding" :column "file")
  ("a" (my-hydras--kill-absolute-buffer-path) "absolute path" :column "file")

  ("l" (progn
         (my-hydras--lsp/body)
         (my-hydras--push '(my-hydras-code/body))) "lsp.." :column "more")
  ("d" (progn
         (dap-hydra/body)
         (my-hydras--push '(my-hydras-code/body))) "dap.." :column "more")
  ("e" (progn
         (my-hydras--ediff/body)
         (my-hydras--push '(my-hydras-code/body))) "ediff.." :column "more"))

(defun my-hydras--kill-absolute-buffer-path ()
  "Copy the current buffer's absolute path"
  (kill-new (if (equal major-mode 'dired-mode)
                      default-directory
                    buffer-file-name)))


;; More at https://github.com/abo-abo/hydra/wiki/Emacs

(defhydra my-hydras--ediff (:color blue)
  ""
  ("b" ediff-buffers :column "buffers")
  ("B" ediff-buffers3 "3way" :column "buffers")

  ("f" ediff-files :column "files")
  ("F" ediff-files3 :column "3way":column "files")
  ("c" ediff-current-file "current" :column "files")

  ("r" ediff-revision "revision" :column "VC")

  ("l" ediff-regions-linewise "linewise" :column "regions")
  ("w" ediff-regions-wordwise "wordwise" :column "regions")
  ("q" my-hydras--pop :column ""))

(defhydra my-hydras--straight (:hint nil)
  "
_c_heck all       |_f_etch all     |_m_erge all      |_n_ormalize all   |p_u_sh all
_C_heck package   |_F_etch package |_M_erge package  |_N_ormlize package|p_U_sh package
----------------^^+--------------^^+---------------^^+----------------^^+------------||_q_uit||
_r_ebuild all     |_p_ull all      |_v_ersions freeze|_w_atcher start   |_g_et recipe
_R_ebuild package |_P_ull package  |_V_ersions thaw  |_W_atcher quit    |prun_e_ build"
  ("c" straight-check-all)
  ("C" straight-check-package)
  ("r" straight-rebuild-all)
  ("R" straight-rebuild-package)
  ("f" straight-fetch-all)
  ("F" straight-fetch-package)
  ("p" straight-pull-all)
  ("P" straight-pull-package)
  ("m" straight-merge-all)
  ("M" straight-merge-package)
  ("n" straight-normalize-all)
  ("N" straight-normalize-package)
  ("u" straight-push-all)
  ("U" straight-push-package)
  ("v" straight-freeze-versions)
  ("V" straight-thaw-versions)
  ("w" straight-watcher-start)
  ("W" straight-watcher-quit)
  ("g" straight-get-recipe)
  ("e" straight-prune-build)
  ("q" my-hydras--pop))

(defhydra my-hydras--lsp (:exit t :hint nil)
  "
 Buffer^^               Server^^                   Symbol
-------------------------------------------------------------------------------------
 [_f_] format           [_M-r_] restart            [_d_] declaration  [_i_] implementation  [_o_] documentation
 [_m_] imenu            [_S_]   shutdown           [_D_] definition   [_t_] type            [_r_] rename
 [_x_] execute action   [_M-s_] describe session   [_R_] references   [_s_] signature"
  ("d" lsp-find-declaration)
  ("D" lsp-ui-peek-find-definitions)
  ("R" lsp-ui-peek-find-references)
  ("i" lsp-ui-peek-find-implementation)
  ("t" lsp-find-type-definition)
  ("s" lsp-signature-help)
  ("o" lsp-describe-thing-at-point)
  ("r" lsp-rename)

  ("f" lsp-format-buffer)
  ("m" lsp-ui-imenu)
  ("x" lsp-execute-code-action)

  ("M-s" lsp-describe-session)
  ("M-r" lsp-restart-workspace)
  ("S" lsp-shutdown-workspace))

(defhydra my-hydras--macro (:hint nil :color pink :pre
                         (when defining-kbd-macro
                           (kmacro-end-macro 1)))
  "
  ^Create-Cycle^   ^Basic^           ^Insert^        ^Save^         ^Edit^
╭─────────────────────────────────────────────────────────────────────────╯
     ^_i_^           [_e_] execute    [_n_] insert    [_b_] name      [_'_] previous
     ^^↑^^           [_d_] delete     [_t_] set       [_K_] key       [_,_] last
 _j_ ←   → _l_       [_o_] edit       [_a_] add       [_x_] register
     ^^↓^^           [_r_] region     [_f_] format    [_B_] defun
     ^_k_^           [_m_] step
    ^^   ^^          [_s_] swap
"
  ("j" kmacro-start-macro :color blue)
  ("l" kmacro-end-or-call-macro-repeat)
  ("i" kmacro-cycle-ring-previous)
  ("k" kmacro-cycle-ring-next)
  ("r" apply-macro-to-region-lines)
  ("d" kmacro-delete-ring-head)
  ("e" kmacro-end-or-call-macro-repeat)
  ("o" kmacro-edit-macro-repeat)
  ("m" kmacro-step-edit-macro)
  ("s" kmacro-swap-ring)
  ("n" kmacro-insert-counter)
  ("t" kmacro-set-counter)
  ("a" kmacro-add-counter)
  ("f" kmacro-set-format)
  ("b" kmacro-name-last-macro)
  ("K" kmacro-bind-to-key)
  ("B" insert-kbd-macro)
  ("x" kmacro-to-register)
  ("'" kmacro-edit-macro)
  ("," edit-kbd-macro)
  ("q" nil :color blue))


(provide 'my-hydras)

;;; my-hydras.el ends here
