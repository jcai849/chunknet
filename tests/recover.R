source("general.R", local=T)

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

tryCatch(value(cdata), error=identity)

"REMOTE"

unstore(cdata)
unstore(sdata)
unstore(lmdata)
unstore(dfdata)
datapool()

"LOCAL"

tryCatch(value(cdata), error=identity)

"REMOTE"

while(!is.null(r <- receive())) {
	print(computationqueue())
	callCC(function(k) do(r, k))
}

"LOCAL"

tryCatch(value(cdata), error=identity)
