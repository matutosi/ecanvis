#!/bin/bash

### shinyapps.ioでグラフの日本語表示をするためのスクリプト
### Author: Kenta Tanaka (https://mana.bi/)
### 以下の記事を参考 (ほぼ丸パクり) にしました
### https://ziomatrix18.blog.fc2.com/blog-entry-853.html
###
### 使い方
### Shinyアプリ内で以下のように実行してください
### download.file("https://raw.githubusercontent.com/ltl-manabi/shinyapps.io_japanese_font/master/use_ipaex_font.sh", destfile = "use_ipaex_font.sh")
### system("bash ./use_ipaex_font.sh")


mkdir ~/tmp
cd ~/tmp
curl -O -L https://moji.or.jp/wp-content/ipafont/IPAexfont/IPAexfont00401.zip
unzip IPAexfont00401.zip
mkdir -p ~/.fonts/ipa
cp ./IPAexfont00401/*.ttf ~/.fonts/ipa
fc-cache -f ~/.fonts
