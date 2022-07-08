post_location <- function(href, location) {
	log("Sending location of %s to Locator", href)
	stopifnot(length(location) > 0)
	fd <- orcv::send(LOCATOR(), paste0("POST /data/", href), location, keep_conn=T)
	invisible(orcv::receive(fd))
}

get_location <- function(href) {
	fd <- orcv::send(LOCATOR(), paste0("GET /data/", href), keep_conn=T)
	loc <- orcv::payload(orcv::receive(fd))
	stopifnot(length(loc) > 0)
	loc
}

get_all_locations <- function() {
	fd <- orcv::send(LOCATOR(), "GET /nodes", keep_conn=T)
	locnload <- orcv::payload(orcv::receive(fd))
	stopifnot(NROW(locnload) > 0)
	locnload$location[order(locnload$loading)]
}

remote_call <- function(procedure, arguments, target) {
	chunkref_args <- sapply(arguments, inherits, "ChunkReference")
        location <- if (!missing(target)) {
		stopifnot(inherits(target, "ChunkReference"))
		get_location(target$href)[1]
	} else if (!any(chunkref_args)) {
                get_all_locations()[1]
        } else {
                get_location(arguments[chunkref_args][[1]]$href)[1]
        }
	arguments <- lapply(arguments, function(arg)
                if (inherits(arg, "ChunkReference")) arg else push(arg, location))

	compref <- ComputationReference(procedure, arguments)
	post_location(compref$output_href, location)
	orcv::send(location, paste0("PUT /computation/", compref$href), compref)
	ChunkReference(compref$output_href)
}

push <- function(x, location, ...) UseMethod("push", x)
push.default <- function(x, location, ...) {
	chunkref <- ChunkReference()
	if (missing(location)) {
		log("push missing location. Accessing all locations")
                location <- get_all_locations()[1]
        } else if (is.character(location)) {
		log("push given only hostname. Accessing all locations for address")
		address <- orcv::address(orcv::as.Location(location, 0L))
                all_locs <- get_all_locations()
                location <- all_locs[orcv::address(all_locs) == address][1]
        }
	post_location(chunkref$href, location)
	post_data(chunkref$href, x, location)
	chunkref
}
push.Chunk <- function(x, location, ...) {
	post_data(x$href, x$data, location)
	x
}

post_data <- function(href, value, location) {
	stopifnot(orcv::is.Location(location) || orcv::is.FD(location))
	orcv::send(location, paste0("POST /data/", href), value)
	value
}

pull <- function(x, ...) UseMethod("pull", x)
pull.character <- function(x, ...) {
	location <- get_location(x)[1]
	fd <- orcv::send(location, paste0("GET /data/", x), keep_conn=T)
	orcv::payload(orcv::receive(fd))
}
pull.ChunkReference <- function(x, ...) {
	pull(x$href)
}

async_pull <- function(x, ...) UseMethod("async_pull", x)
async_pull.character <- function(x, ...) {
	location <- get_location(x)[1]
	orcv::send(location, paste0("GET /async/data/", x))
}

kill_all_nodes <- function() {
        all_locations <- get_all_locations()
	orcv::send(all_locations, "EXIT")
	orcv::send(LOCATOR(), "EXIT")
        invisible(NULL)
}
