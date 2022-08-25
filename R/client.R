post_locations <- function(hrefs, locations) {
	stopifnot(is.character(hrefs), orcv::is.Location(locations))
	stopifnot(length(locations) > 0,
		  length(hrefs) == length(locations))
	fd <- orcv::send(LOCATOR(), paste0("POST /data/", paste(hrefs, collapse=',')), locations, keep_conn=T)
	invisible(orcv::receive(fd))
}

get_locs <- function(method_arg) function(ids) {
	request <- paste0("GET ", method_arg, if (missing(ids)) NULL else paste(ids, collapse=','))
	fd <- orcv::send(LOCATOR(), request, keep_conn=T)
	locs <- orcv::payload(orcv::receive(fd)[[1]])
	stopifnot(orcv::is.Location(locs),
		  length(locs) == length(ids))
	invisible(locs)
}
get_locations <- get_locs("/data/") # takes char vector of hrefs
get_host_locations <- get_locs("/host/") #takes char vector of hosts
get_least_loaded_locationss <- get_locs("/node/") # takes integer n locations

remote_call <- function(procedure, arguments, target, post_locs=TRUE) {
	chunkref_args <- sapply(arguments, inherits, "ChunkReference")

        location <- if (!missing(target)) {
		stopifnot(inherits(target, "ChunkReference"))
		get_locations(target$href)[1]
	} else if (!any(chunkref_args)) {
                get_least_loaded_locations(1)
        } else if (!all(sapply(clocs <- lapply(arguments[chunkref_args], function(x) get("init_loc", x)), is.null))) {
		clocs[!sapply(clocs, is.null)][[1]]
	} else {
                get_locations(arguments[chunkref_args][[1]]$href)[1]
        }

	new_chunkrefs <- push(arguments[!chunkref_args], rep(location, sum(!chunkref_args)), post_locs=FALSE)
	post_locations(sapply(new_chunkrefs, function(x) get("href", x)), rep(location, length(new_chunkrefs)))

	arguments[!chunkref_args] <- new_chunkrefs
	
	compref <- ComputationReference(procedure, arguments)
	if (post_locs) post_locations(compref$output_href, location)
	orcv::send(location, paste0("PUT /computation/", compref$href), compref)
	ChunkReference(compref$output_href, location, compref)
}

push <- function(x, locations, ...) UseMethod("push", x)
push.default <- function(x, locations, post_locs=TRUE, ...) { # returns list of chunkrefs
	locations <- if (missing(locations)) { 
                get_least_loaded_locations(1)
        } else if (is.character(locations)) {
		get_host_locations(locations)
        } else locations

	chunkrefs <- lapply(locations, function(loc) ChunkReference(init_loc=loc, gen_comp=NULL))
	if (post_locs) post_locations(sapply(chunkrefs, function(x) get("href", x)), locations)

	post_data(sapply(chunkrefs, function(x) get("href", x)), x, locations)
	chunkrefs
}
push.Chunk <- function(x, locations, ...) {
	post_data(x$href, x$data, locations)
	x
}
push.list <- function(x, locations, ...) {
	if (all(sapply(x, inherits, "Chunk"))) {
		post_data(sapply(x, href), sapply(x, data), locations)
	} else NextMethod()
}

post_data <- function(hrefs, values, locations) {
	stopifnot(orcv::is.Location(locations) || orcv::is.FD(locations))
	stopifnot(is.character(hrefs),
		  length(hrefs) == length(locations))
	hrs <- split(hrefs, as.factor(locations))			# group hrefs by location
	vals <- split(values, as.factor(locations))			# group vals by location
	locs <- lapply(split(locations, as.factor(locations)), '[[', 1)	# get corresponding locs
	mapply(function(l, h, v) orcv::send(l, paste0("POST /data/", paste(h, collapse=',')), v),
		locs, hrs, vals)
	invisible(NULL)
}

pull <- function(x, ...) UseMethod("pull", x)
pull.character <- function(x, ...) { # hrefs
	locations <- get_locations(x)
	hrefs_at_locs <- split(x, as.factor(locations))
	locs <- unique(locations)
	fds <- orcv::as.FD(mapply(function(loc, hrefs)
				  	orcv::send(loc, paste0("GET /data/", paste(hrefs, collapse=',')), keep_conn=T),
		      	          locs, hrefs_at_locs))
	unsplit(lapply(orcv::receive(fds, simplify=FALSE), orcv::payload), as.factor(locations))
}
pull.list <- function(x, ...) {
	stopifnot(all(sapply(x, inherits, "ChunkReference")))
	chunks <- pull(sapply(x, href))
	lapply(x, function(x) x$gen_comp <- NULL)
	chunks
}
pull.ChunkReference <- function(x, ...) pull(list(x))

async_pull <- function(hrefs, ...) {
	stopifnot(is.character(hrefs))
	location <- get_locations(hrefs)
	hrefs_at_locs <- split(hrefs, as.factor(locations))
	mapply(function(loc, hrefs) orcv::send(location, paste0("GET /async/data/", paste(hrefs, collapse=','))),
	       unique(locations), hrefs_at_locs)
}

kill_all_nodes <- function() {
	orcv::send(LOCATOR(), "EXIT")
}
