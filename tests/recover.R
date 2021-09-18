source("general.R", local=T, echo=T)

"REMOTE"

computationpool()
datapool()
unstore(cdata)
datapool()

"LOCAL"

"value(cdata) -> ERROR!"

recover(cdata)

"REMOTE"

str(computationpool())
do(receive())
computationpool()
datapool()

"LOCAL"

value(cdata)
