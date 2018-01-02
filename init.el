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



;; 1. 行頭（行末）に飛ぶ
;; 2. 次にバッファ先頭（行末）に飛ぶ
;; 3. 次に元の場所に戻る
(package-install 'sequential-command)
(require 'sequential-command-config)
(global-set-key "\C-a" 'seq-home)
(global-set-key "\C-e" 'seq-end)
(when (require 'org nil t)
  (define-key org-mode-map "\C-a" 'org-seq-home)
  (define-key org-mode-map "\C-e" 'org-seq-end))


(add-to-list 'load-path "~/.emacs.d/custom")

;; 補完と検索を強化する
(load "completion_and_search.el")


;; Emacs標準テーマから暗めのテーマを選択
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

;;reload
(global-set-key [f12] 'eval-buffer)

;; タイトルバーにファイル名を表示する
(setq frame-title-format (format "emacs@%s : %%f" (system-name)))

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
 '(package-selected-packages (quote (company-jedi swoop counsel sequential-command))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; C-hをBackspaceに変更
(global-set-key (kbd "C-h") 'delete-backward-char)

;; C-zをUndoに変更
(global-set-key "\C-z" 'undo)
