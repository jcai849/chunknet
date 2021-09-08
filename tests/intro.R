"LOCAL MACHINE:"

library(largerscale)

(vdata <- read.hdfs("/some/file/path"))

"REMOTE MACHINE:"

(vcomp <- receive())
(do(vcomp))
(datapool())

"LOCAL MACHINE:"

(v <- value(vdata))
tdata <- do(t.test, v)

"REMOTE MACHINE:"

(tcomp <- receive())
(do(tcomp))
(datapool())

"LOCAL MACHINE:"

sdata <- do(summary, tdata)

"REMOTE MACHINE:"

(scomp <- receive())
(do(scomp))
(datapool())

"LOCAL MACHINE:"

(s <- value(sdata))

# graph(sdata)
