source("general.R", local=T, echo=T)

"REMOTE"

computationqueue()
datapool()
unstore(cdata)
datapool()

"LOCAL"

tryCatch(value(cdata), error=identity)

"REMOTE"

str(computationqueue())
do(receive())
computationqueue()
datapool()

"LOCAL"

value(cdata)
