"LOCAL MACHINE:"

library(largerscale)

(dfdata <- read.hdfs("/some/file/path"))
str(dfdata)

"REMOTE MACHINE:"

(computationpool())
(dfcomp <- receive())

str(dfcomp)
(computationpool())

(do(dfcomp))

(datapool())
str(datapool())

"LOCAL MACHINE:"

str(df <- value(dfdata))
(lmdata <- do(lm, list(y ~ x, data=df)))

"REMOTE MACHINE:"

(lmcomp <- receive())
(do(lmcomp))
str(datapool())

"LOCAL MACHINE:"

(sdata <- do(summary, lmdata))

"REMOTE MACHINE:"

(scomp <- receive())
(do(scomp))
str(datapool())

"LOCAL MACHINE:"

s <- value(sdata)
s[1] <- NULL # get rid of call capture !!
print(s)

(cdata <- do(coef, sdata))

"REMOTE MACHINE:"

(ccomp <- receive())
(do(ccomp))

"LOCAL MACHINE:"

dependencygraph(cdata)
