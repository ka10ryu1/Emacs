
;; Emacsで重要になってくるのがM-xで起動する各種コマンドの扱いです
;; counselはivyというコマンド補完機能を用いて，いわゆる絞込検索を実現しています
;; うろ覚えのコマンドであっても，絞込んでCtrl+n, Ctrl+pで選択できます
(package-install 'counsel)

;; ivy設定
(require 'ivy)
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)
(setq ivy-height 30) ;; minibufferのサイズを拡大！（重要）
(setq ivy-extra-directories nil)
(setq ivy-re-builders-alist
      '((t . ivy--regex-plus)))

;; counsel設定
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file) ;; find-fileもcounsel任せ！
(defvar counsel-find-file-ignore-regexp (regexp-opt '("./" "../")))


;; C-s で検索文字を入力し、C-oを入力することで候補をリスト化できる
(package-install 'swoop)

(require 'swoop)
(global-set-key (kbd "C-o")   'swoop)
(global-set-key (kbd "C-M-o") 'swoop-multi)
(global-set-key (kbd "M-o")   'swoop-pcre-regexp)
(global-set-key (kbd "C-S-o") 'swoop-back-to-last-position)
(global-set-key (kbd "H-6")   'swoop-migemo) ;; Option for Japanese match

;; 検索の移行
;; isearch     > press [C-o] > swoop
;; evil-search > press [C-o] > swoop
;; swoop       > press [C-o] > swoop-multi
(define-key isearch-mode-map (kbd "C-o") 'swoop-from-isearch)
;(define-key evil-motion-state-map (kbd "C-o") 'swoop-from-evil-search)
;(define-key swoop-map (kbd "C-o") 'swoop-multi-from-swoop)



;; 自動補完
;; virtualenvが必要（$ sudo -H pip3 install virtualenv）
;; 一度だけM-x jedi:install-serverを実行する必要あり
(package-install 'auto-complete)
(package-install 'jedi)

(ac-config-default)
(ac-set-trigger-key "TAB")
(setq ac-use-menu-map t);; 補完メニュー表示時にC-n/C-pで補完候補選択
(setq ac-use-fuzzy t);; 曖昧マッチ

(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)
