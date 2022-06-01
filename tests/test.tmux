split-window R --interactive
send-keys 'largerscale::locator("localhost", 8999L, verbose=T)' Enter
split-window R --interactive
send-keys 'largerscale::worker("localhost", 9001L, "localhost", 8999L, verbose=T)' Enter
split-window R --interactive
send-keys 'largerscale::worker("localhost", 9002L, "localhost", 8999L, verbose=T)' Enter
select-layout tiled
run-shell 'sleep 1'
select-pane -t0; send-keys 'R && tmux kill-session' Enter 'source("test.R", echo=T)' Enter
