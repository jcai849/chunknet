split-window R --interactive
send-keys 'chunknet::locator_node("localhost", 8999L, verbose=T)' Enter
split-window R --interactive
send-keys 'chunknet::worker_node("localhost", 9001L, "localhost", 8999L, verbose=T)' Enter
split-window R --interactive
send-keys 'chunknet::worker_node("localhost", 9002L, "localhost", 8999L, verbose=T)' Enter
select-layout tiled
select-pane -t0; send-keys 'R && tmux kill-session' Enter 'source("test.R", echo=T)' Enter
