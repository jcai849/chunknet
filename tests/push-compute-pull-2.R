library(largerscale)

PORT <- 8989L
x <- 1:10
data <- push_nonblocking(x, "localhost", PORT)
computation <- remote_call_nonblocking(procedure=factorial, arguments=list(x=data), "localhost", PORT)
x <- pull_blocking(computation$output, "localhost", PORT)
print(x)
