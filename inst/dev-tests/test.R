library(chunknet)

options(chunknetVerbose = TRUE)

orcv::start()
LOCATOR("localhost", 8999L)
worker1 <- orcv::as.Location("localhost", 9001L)
worker2 <- orcv::as.Location("localhost", 9002L)
Sys.sleep(2)
a <- push(list(1:3))
print(pull(a))
b <- push(list(2:4), "localhost")
print(pull(b))
c <- remote_call(function() {3:1}, list())
d <- remote_call("+", list(b, c))
pull(d)
rm(a, b, c, d)

x <- push(1:10, worker1)
print(pull(x))
y <- push(10:1, worker2)
print(pull(y))
z <- remote_call("+", list(x, y))
print(pull(z))

j <- remote_call(cumsum, list(z))
print(pull(j))

k <- remote_call("-", list(1:10, 1:2))
print(pull(k))

gc()

kill_all_nodes()
q("no")
