;; =======
;; counsel
;; =======
;;
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
;(global-set-key (kbd "M-x") 'counsel-M-x)
;(global-set-key (kbd "C-x C-f") 'counsel-find-file) ;; find-fileもcounsel任せ！
(defvar counsel-find-file-ignore-regexp (regexp-opt '("./" "../")))


;; =====
;; swoop
;; =====
;;
;; C-s で検索文字を入力し、C-oを入力することで候補をリスト化できる
(package-install 'swoop)

(require 'swoop)
;; (global-set-key (kbd "C-o")   'swoop)
;; (global-set-key (kbd "C-M-o") 'swoop-multi)
;; (global-set-key (kbd "M-o")   'swoop-pcre-regexp)
;; (global-set-key (kbd "C-S-o") 'swoop-back-to-last-position)
;; (global-set-key (kbd "H-6")   'swoop-migemo) ;; Option for Japanese match

;; ;; 検索の移行
;; ;; isearch     > press [C-o] > swoop
;; ;; evil-search > press [C-o] > swoop
;; ;; swoop       > press [C-o] > swoop-multi
;; (define-key isearch-mode-map (kbd "C-o") 'swoop-from-isearch)
;; ;(define-key evil-motion-state-map (kbd "C-o") 'swoop-from-evil-search)
;; ;(define-key swoop-map (kbd "C-o") 'swoop-multi-from-swoop)


;; =============
;; auto-complete
;; =============
;;
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

;; ============
;; git-complete
;; ============
;;
;; [行補完]
;; 入力途中のテキストでプロジェクト内を git grep して、
;; でてきた結果のなかから出現頻度が高い行
;; （デフォルトでは結果の 2% 以上を占めるもの）を
;; 補完候補としてサジェストします。
;;
;; [オムニ補完]
;; 入力途中のテキストで行補完と同様にプロジェクト内を git grep します
;; これといった行補完の候補がなかった場合、
;; まず、それぞれの検索結果から「クエリ文字列より右の部分」だけを取り出します。
(require 'git-complete)
;; (global-unset-key (kbd "C-c C-c")) ;; 一応unbindしておきましょう
;; (global-set-key (kbd "C-c C-c") 'git-complete)
(add-to-list 'load-path "~/.emacs.d/git-complete") ;; お好きなように
(setq git-complete-enable-autopair t)

;; ====================
;; find-file-in-project
;; ====================
;;
;; project内のfileをfile名で検索してくれます
;; 当然ivyが効いているので絞込検索が可能です！
(package-install 'find-file-in-project)
(require 'find-file-in-project)
(global-set-key [(super shift i)] 'find-file-in-project)

;; =======
;; recentf
;; =======
;;
;; 履歴によるファイル検索packageです
;; 直近に開いたファイルをリスト化してminibufferに表示してくれます
;; counselと組み合わせて使えば絞込によるファイル移動が可能になります
;; 余分なメッセージを削除しておきましょう
(package-install 'recentf)
(package-install 'recentf-ext)
(defmacro with-suppressed-message (&rest body)
  "Suppress new messages temporarily in the echo area and the `*Messages*' buffer while BODY is evaluated."
  (declare (indent 0))
  (let ((message-log-max nil))
    `(with-temp-message (or (current-message) "") ,@body)))

(require 'recentf)
(setq recentf-save-file "~/.emacs.d/.recentf")
(setq recentf-max-saved-items 200)             ;; recentf に保存するファイルの数
(setq recentf-exclude '(".recentf"))           ;; .recentf自体は含まない
(setq recentf-auto-cleanup 'never)             ;; 保存する内容を整理
(run-with-idle-timer 30 t '(lambda () (with-suppressed-message (recentf-save-list))))
(require 'recentf-ext) ;; ちょっとした拡張

(define-key global-map [(super r)] 'counsel-recentf) ;; counselにおまかせ！

;; =========
;; dumb-junp
;; =========
;;
;; Emacsではtagを使って関数の宣言まで移動するpackageはいくつかありますが，tagの生成や設定などが面倒でした．
;; dumb-jumpはそれらを全くすることなく，どの言語でもいい感じに宣言箇所までコマンド１つで移動することが可能になります
(setq dumb-jump-mode t)
(setq dumb-jump-selector 'ivy) ;; 候補選択をivyに任せます
(setq dumb-jump-use-visible-window nil)
(define-key global-map [(super d)] 'dumb-jump-go) ;; go-to-definition!
(define-key global-map [(super shift d)] 'dumb-jump-back)

;; =======
;; neotree
;; =======
;;
;; ディレクトリツリーをサイドバーに表示してくれるpackageです
;; プロジェクトのディレクトリ構成が気になった時などに使いたいですね
(package-install 'neotree)
(require 'neotree)
(setq neo-theme 'ascii)   ;; icon, classic等もあるよ！
(setq neo-persist-show t) ;; delete-other-window で neotree ウィンドウを消さない
(setq neo-smart-open t)   ;; neotree ウィンドウを表示する毎に current file のあるディレクトリを表示する
(setq neo-smart-open t)

;; =========
;; yasnippet
;; =========
;;
;; テンプレート挿入機能を強化するパッケージ
(package-install 'yasnippet)
(require 'yasnippet)
(yas-global-mode t)
(yas/initialize)
(yas/load-directory "~/.emacs.d/snippets")



;; [EOF]
