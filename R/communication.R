send.default <- function(x, to) {
	conn <- socketConnection(host=host(to), port=port(to), open="wb")
	serialize(x, conn)
	flush(conn)
	close(conn)
}

receive <- function(port) {
	unserialize(readBin(comms(port), "raw"))
}

comms <- local({
	server <- NULL
	getserver <- function(port) {
		if (is.null(server)) {
			server <<- serverSocket(if (missing(port)) 0 else port)
		} else server
	}
	function(port) socketAccept(getserver(port), blocking=TRUE, open="rb", timeout=Inf)
})

worker <- function(host="localhost", port, init=T) {
	if (init) system2("ssh", host,
			  shQuote(paste("R", "-e",
					paste0("largerscale::work(port=", port, ")"))))
	assign(paste0(host,port), location(host, port), workerpool())
}
