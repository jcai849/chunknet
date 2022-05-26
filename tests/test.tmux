split-window R --interactive
send-keys 'largerscale::locator("localhost", 8999L, verbose=T)' Enter
split-window R --interactive
send-keys 'largerscale::worker("localhost", 9001L, "localhost", 8999L, verbose=T)' Enter
split-window R --interactive
send-keys 'largerscale::worker("localhost", 9002L, "localhost", 8999L, verbose=T)' Enter
select-layout tiled
run-shell 'sleep 2'
select-pane -t0; send-keys 'R' Enter 'source("test.R", echo=T)' Enter
