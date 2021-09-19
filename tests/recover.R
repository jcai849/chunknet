source("general.R", local=T, echo=T)

"REMOTE"

computationpool()
datapool()
unstore(cdata)
datapool()

"LOCAL"

tryCatch(value(cdata), error=identity)

"REMOTE"

str(computationpool())
do(receive())
computationpool()
datapool()

"LOCAL"

value(cdata)
