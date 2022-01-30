library(largerscale)

PORT <- 8989L
x <- 1:10
data <- push(x, "localhost", PORT)
computation <- remote_call(procedure=factorial, arguments=list(x=data), "localhost", PORT)
x <- pull(computation$output, "localhost", PORT)
print(x)
