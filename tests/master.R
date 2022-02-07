library(largerscale)

LOCATOR("localhost", 8989L)

a <- remote_call("+", list(1:10, 10:1))
print(pull(a$href))

worker1 <- list(address="localhost", port=3434L)
worker2 <- list(address="localhost", port=4343L)

x <- push(1:10, worker1)
y <- push(10:1, worker2)
z <- remote_call("+", list(x, y))
print(pull(z$href))

b <- remote_call(cumsum, list(z))
print(pull(b$href))
