## Local -----------------------------------------------------------------------
library(largerscale)

dfdata <- read.hdfs("/some/file/path")
str(dfdata)


## Remote ----------------------------------------------------------------------
(computationqueue())
(dfcomp <- receive())

str(dfcomp)
(computationqueue())

str(do(dfcomp))

(datapool())
str(datapool())


## Local -----------------------------------------------------------------------
str(df <- value(dfdata))
(lmdata <- do(lm, list(y ~ x, data=dfdata)))


## Remote ----------------------------------------------------------------------
(lmcomp <- receive())
str(do(lmcomp))
(datapool())


## Local -----------------------------------------------------------------------
(sdata <- do(summary, lmdata))


## Remote ----------------------------------------------------------------------
(scomp <- receive())
(do(scomp))
(datapool())


## Local -----------------------------------------------------------------------
s <- value(sdata)
s[1] <- NULL # get rid of call capture !!
(s)

(cdata <- do(coef, sdata))


## Remote ----------------------------------------------------------------------
(ccomp <- receive())
(do(ccomp))
