library(largerscale)

# Showing some of the main actors:
(id <- identifier("data", "<unique-key-391>"))
(loc <- locate(id))

(vcomp <- read.hdfs("/some/file/path"))

# locate where to perform the computation, transfer the computation
# on the remote end receiving the compuation:

(do(vcomp))

# Concurrently on the local end:

(localv <- data(vcomp))
with.comment(value(localv))
(transformation1 <- computation(fun=t.test,
			       input=localv,
			       identifier("computation", "<unique-key-234>")))
(request(transformation1))

# locate where to perform the computation, transfer the computation
# on the remote end receiving the compuation:

(do(transformation1))


# The remote end's data pool:

(datapool())
