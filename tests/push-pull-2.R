library(largerscale)

PORT <- 8989L
x <- 1:10
data <- push_nonblocking(x, "localhost", PORT)
x <- pull_blocking(data$href, "localhost", PORT)
print(x)
