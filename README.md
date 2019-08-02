# 概要

Emacsの設定ファイル

# 動作環境

- Emacs 25.1.1 ($ emacs --version)

# 導入手順

## 必要なパッケージのインストール

```console
$ pip install autopep8
$ pip install pyflakes
$ pip install jedi
$ sudo apt install p7zip fontforge virtualenv emacs-mozc-bin
```

## .emacs.dフォルダの生成

```console
$ makedir ~/.emacs.d
```

## .emacs.dに以下のファイルとフォルダを追加する
- init.el
- custom/
- sunippets/

```console
$ cp -r ./init.el ./custom/ ./sunippets/ ~/.emacs.d
```

## git-completeのコピー

custom直下にgit-complete.elをコピーする

```console
$ ./git-complete.sh
$ cp ./git-complete/git-complete.el ~/.emacs.d/custom/
```

## 日本語フォント（Cica）のインストール

```console
$ cica.sh
```

あとはCicaのREADMEに従ってフォントの　ビルドをするか、ビルド済みパッケージをダウンロードする

ttfファイルが作成されたら、

- ホームディレクトリに.fontsフォルダを作成する
- ttfファイルを.fontsフォルダに移動する
- $ fc-cache -fv # フォントキャッシュのクリア


# 利用できるキーボードショートカット


[global_set_key.el](https://github.com/ka10ryu1/Emacs/blob/master/custom/global_set_key.el)を参照
