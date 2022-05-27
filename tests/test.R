library(largerscale)

options(largerscaleVerbose = TRUE)

LOCATOR("localhost", 8999L)
worker1 <- data.frame(address="localhost", port=9001L)
worker2 <- data.frame(address="localhost", port=9002L)

a <- push(1:3)
print(pull(a))
b <- push(2:4, "localhost")
print(pull(b))
c <- remote_call("+", list(a, b))
print(pull(c))
rm(a, b)

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

#kill_all_nodes()
#q("no")
