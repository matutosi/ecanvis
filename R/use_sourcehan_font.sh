#!/bin/bash

### shinyapps.ioでグラフの日本語表示をするためのスクリプト
### Author: Kenta Tanaka (https://mana.bi/)
### 以下の記事を参考 (ほぼ丸パクり) にしました
### https://ziomatrix18.blog.fc2.com/blog-entry-853.html
###
### 使い方
### Shinyアプリ内で以下のように実行してください
### download.file("https://raw.githubusercontent.com/ltl-manabi/shinyapps.io_japanese_font/master/use_sourcehan_font.sh", destfile = "use_sourcehan_font.sh")
### system("bash ./use_sourcehan_font.sh")


mkdir ~/tmp
cd ~/tmp
curl -O -L https://github.com/adobe-fonts/source-han-sans/releases/download/2.003R/SourceHanSans.ttc
curl -O -L https://github.com/adobe-fonts/source-han-serif/releases/download/1.001R/SourceHanSerif.ttc
mkdir -p ~/.fonts/sourcehan
cp ./SourceHan*.ttc ~/.fonts/sourcehan
fc-cache -f ~/.fonts
