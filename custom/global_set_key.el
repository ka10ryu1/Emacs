;; =========================
;; キーボードショートカットの設定
;; =========================
;;
;; Mozcを全角/半角キーに割り当て
(global-set-key [zenkaku-hankaku] #'toggle-input-method)
;; C-\でMozcに切り替わらないようにする
(global-unset-key "\C-\\")
;; C-a、C-eを強化
(global-set-key "\C-a" 'seq-home)
(global-set-key "\C-e" 'seq-end)
(when (require 'org nil t)
  (define-key org-mode-map "\C-a" 'org-seq-home)
  (define-key org-mode-map "\C-e" 'org-seq-end))
;; C-hをBackspaceに変更
(global-set-key (kbd "C-h") 'delete-backward-char)
;; C-zをUndoに変更
(global-set-key "\C-z" 'undo)
;; F12キーでeval bufferでelファイルのエラーチェック
(global-set-key [f12] 'eval-buffer)
;; counsel設定
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file) ;; find-fileもcounsel任せ！
;; swoop設定
(global-set-key (kbd "C-o")   'swoop)
(global-set-key (kbd "C-M-o") 'swoop-multi)
(global-set-key (kbd "M-o")   'swoop-pcre-regexp)
(global-set-key (kbd "C-S-o") 'swoop-back-to-last-position)
(global-set-key (kbd "H-6")   'swoop-migemo) ;; Option for Japanese match
(define-key isearch-mode-map (kbd "C-o") 'swoop-from-isearch);; 検索の移行
;; git-complete
(global-unset-key (kbd "C-c C-c")) ;; 一応unbindしておきましょう
(global-set-key (kbd "C-c C-c") 'git-complete)
;; neotreeの設定
(global-set-key [f8] 'neotree-toggle)
;; find-file-in-project
(global-set-key [(super shift i)] 'find-file-in-project)

;; recentf
(define-key global-map [(super r)] 'counsel-recentf) ;; counselにおまかせ！
;; dump-jump
(define-key global-map [(super d)] 'dumb-jump-go) ;; go-to-definition!
(define-key global-map [(super shift d)] 'dumb-jump-back)
