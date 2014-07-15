#!/bin/sh
session="text-network-compiler"
tmux start-server
tmux new-session -d -s $session "vim -c ':NERDTree'"
tmux source-file .tmux.conf
tmux selectp -t $session:0
tmux splitw -t $session:0 -p 20 "sbt console; bash"
tmux attach-session -t $session
