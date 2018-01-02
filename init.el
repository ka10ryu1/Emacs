;; ===========
;; MELFAの設定
;; ===========
;;
;; パッケージ管理ツールの設定
(require 'package)
;; Emacs JP を始めとして、package-archives の後ろに追加する例が多いので従った。
;; http://emacs-jp.github.io/packages/package-management/package-el.html
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; init.el で package-install() せず、M-x package-list-packages から
;; インストールする場合、これらは不要。package-install() が良しなに
;; 初期化してくれるため。
(package-initialize)
(package-refresh-contents)

;; ==================
;; seauential-command
;; ==================
;;
;; 1. 行頭（行末）に飛ぶ
;; 2. 次にバッファ先頭（行末）に飛ぶ
;; 3. 次に元の場所に戻る
(package-install 'sequential-command)
(require 'sequential-command-config)

;; =======================
;; 補完と検索の機能を強化する
;; =======================
;;
(add-to-list 'load-path "~/.emacs.d/custom")
(load "git-complete.el")
(load "completion_and_search.el")
(load "global_set_key.el")



;; ===================
;; Python開発支援ツール
;; ===================
;;
;; pep8対応($ pip install autopep8)
(package-install 'py-autopep8)
(require 'py-autopep8)
(add-hook 'python-mode-hook 'py-autopep8-enable-on-save)
;; Flymake($ pip install pyflake)
(package-install 'flymake)
(flymake-mode t)
(require 'flymake-python-pyflakes)
(add-hook 'python-mode-hook 'flymake-python-pyflakes-load)
(defun flymake-show-help ()
  (when (get-char-property (point) 'flymake-overlay)
    (let ((help (get-char-property (point) 'help-echo)))
      (if help (message "%s" help)))))
(add-hook 'post-command-hook 'flymake-show-help)

;; ===========
;; その他の設定
;; ===========
;;
;; テーマの設定
(load-theme 'misterioso t)
;; 文字コードの設定
(set-default-coding-systems 'utf-8)
;; タブをスペースで扱う
(setq-default indent-tabs-mode nil)
;; 起動時の画面はいらない
(setq inhibit-startup-message t)
;; 行・列番号を表示する
(line-number-mode t)
(column-number-mode t)
;; スクロールバーを非表示にする
(set-scroll-bar-mode nil)
;; ツールバーの消去
(tool-bar-mode -1)
;; yes or no -> y or n
(fset 'yes-or-no-p 'y-or-n-p)
;; タイトルバーにファイル名を表示する
(setq frame-title-format (format "emacs@%s : %%f" (system-name)))
;; 対応する括弧を表示
(show-paren-mode 1)
;; 現在の行に色を付ける
(global-hl-line-mode t)
;; 行末の空白の自動削除
(package-install 'whitespace)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
;; タブ・行末・空行表示
(require 'whitespace)
(set-face-foreground 'whitespace-space nil);; 空白
(set-face-background 'whitespace-space "gray33")
(set-face-background 'whitespace-empty "gray33");; ファイル先頭と末尾の空行
(set-face-foreground 'whitespace-tab nil);; タブ
(set-face-background 'whitespace-tab "gray33")
(set-face-background 'whitespace-trailing "gray33");; ???
(set-face-background 'whitespace-hspace "gray33")
(setq whitespace-style '(face           ; faceで可視化
                         trailing       ; 行末
                         tabs           ; タブ
                         empty          ; 先頭/末尾の空行
                         spaces         ; 空白
                         ;; space-mark     ; 表示のマッピング
                         tab-mark))

(setq whitespace-space-regexp "\\(\u3000+\\)");; スペースは全角のみを可視化
(setq whitespace-display-mappings;; タブの表示を変更
      '((tab-mark ?\t [?\xBB ?\t])))
(global-whitespace-mode 1)
;; パスを1階層ずつ削除
(defun my-minibuffer-delete-parent-directory ()
  "Delete one level of file path."
  (interactive)
  (let ((current-pt (point)))
    (when (re-search-backward "/[^/]+/?" nil t)
      (forward-char 1)
      (delete-region (point) current-pt))))(define-key minibuffer-local-map (kbd "M-^") 'my-minibuffer-delete-parent-directory)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(avy-migemo-function-names
   (quote
    (swiper--add-overlays-migemo
     (swiper--re-builder :around swiper--re-builder-migemo-around)
     (ivy--regex :around ivy--regex-migemo-around)
     (ivy--regex-or-literal :around ivy--regex-or-literal-migemo-around)
     (ivy--regex-plus :around ivy--regex-plus-migemo-around)
     ivy--highlight-default-migemo ivy-occur-revert-buffer-migemo ivy-occur-press-migemo avy-migemo-goto-char avy-migemo-goto-char-2 avy-migemo-goto-char-in-line avy-migemo-goto-char-timer avy-migemo-goto-subword-1 avy-migemo-goto-word-1 avy-migemo-isearch avy-migemo-org-goto-heading-timer avy-migemo--overlay-at avy-migemo--overlay-at-full)))
 '(package-selected-packages
   (quote
    (whitespace flymake flymake-python-pyflakes flycheck autopep8 py-autopep8 yasnippet neotree dumb-junp recentf-ext recentf find-file-in-project git-complete jedi auto-complete company-jedi swoop counsel sequential-command))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
