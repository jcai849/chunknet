post_locations <- function(hrefs, locations) {
	log("Sending location of %s to Locator", href)
	stopifnot(is.character(hrefs), is.Location(locations))
	stopifnot(length(location) > 0,
		  length(hrefs) == length(locations))
	fd <- orcv::send(LOCATOR(), paste0("POST /data/", paste(hrefs, collapse=',')), locations, keep_conn=T)
	invisible(orcv::receive(fd))
}

get_locations <- function(hrefs) {
	stopifnot(is.character(hrefs))
	fd <- orcv::send(LOCATOR(), paste0("GET /data/", paste(hrefs, collapse=','), keep_conn=T)
	locs <- orcv::payload(orcv::receive(fd))
	stopifnot(is.Location(locs),
		  length(locs) == length(hrefs))
	invisible(locs)
}

get_host_locations(hosts) {
	fd <- orcv::send(LOCATOR(), paste0("GET /host/", paste(hosts, collapse=','), keep_conn=T)
	locs <- orcv::payload(orcv::receive(fd))
	stopifnot(is.Location(locs),
		  length(locs) == length(hosts))
	invisible(locs)
}

get_least_loaded_location <- function() {
	fd <- orcv::send(LOCATOR(), "GET /node", keep_conn=T)
	orcv::payload(orcv::receive(fd))
}

remote_call <- function(procedure, arguments, target, post_locs=TRUE) {
	chunkref_args <- sapply(arguments, inherits, "ChunkReference")

        location <- if (!missing(target)) {
		stopifnot(inherits(target, "ChunkReference"))
		get_locations(target$href)[1]
	} else if (!any(chunkref_args)) {
                get_least_loaded_location()
        } else if (any(!is.null(clocs <- sapply(arguments[chunkref_args], '$', "loc")))) {
		clocs
	} else {
                get_locations(arguments[chunkref_args][[1]]$href)[1]
        }

	new_chunkrefs <- push(arguments[!chunkref_args], rep(location, sum(!chunkref_args)), post_locs=FALSE)
	post_locations(sapply(new_chunkrefs, '$', href), rep(location, length(new_chunkrefs)))

	arguments[!chunkref_args] <- new_chunkrefs
	
	compref <- ComputationReference(procedure, arguments)
	if (post_locs) post_locations(compref$output_href, location)
	orcv::send(location, paste0("PUT /computation/", compref$href), compref)
	ChunkReference(compref$output_href, location)
}

push <- function(x, locations, ...) UseMethod("push", x)
push.default <- function(x, locations, post_locs=TRUE, ...) {
	location <- if (missing(location)) {
                get_least_loaded_location()
        } else if (is.character(location)) {
		address <- orcv::address(orcv::as.Location(location, 0L))
		get_host_locations(address)
        } else locations
	chunkrefs <- lapply(x, function(...) ChunkReference(init_loc=location))

	if (post_locs) post_locations(sapply(chunkrefs '$' "href"), rep(location, length(chunkrefs)))

	post_data(sapply(chunkrefs, '$', "href"), x, location)
	chunkrefs
}
push.Chunk <- function(x, location, ...) {
	post_data(x$href, x$data, location)
	x
}

post_data <- function(hrefs, values, location) {
	stopifnot(orcv::is.Location(location) || orcv::is.FD(location))
	stopifnot(is.character(href),
		  length(hrefs) == length(values))
	orcv::send(location, paste0("POST /data/", paste(hrefs, collapse=',')), values)
	values
}

pull <- function(x, ...) UseMethod("pull", x)
pull.character <- function(x, ...) {
	locations <- get_locations(x)
	hrefs_at_locs <- split(x, locations)
	fds <- mapply(function(loc, hrefs) orcv::send(loc, paste0("GET /data/", paste(hrefs, collapse=',')), keep_conn=T),
		      unique(locations), hrefs_at_locs)
	orcv::payload(orcv::receive(fds))
}
pull.ChunkReference <- function(x, ...) {
	pull(x$href)
}

async_pull <- function(hrefs, ...) {
	stopifnot(is.character(hrefs))
	location <- get_locations(hrefs)
	hrefs_at_locs <- split(hrefs, locations)
	mapply(function(loc, hrefs) orcv::send(location, paste0("GET /async/data/", paste(hrefs, collapse=',')))
	       unique(locations), hrefs_at_locs)
}

kill_all_nodes <- function() {
	orcv::send(LOCATOR(), "EXIT")
}
