post_locations <- function(hrefs, locations) {
	stopifnot(is.character(hrefs), orcv::is.Location(locations))
	stopifnot(length(locations) > 0,
		  length(hrefs) == length(locations))
	fd <- orcv::send(LOCATOR(), paste0("POST /data/", paste(hrefs, collapse=',')), locations, keep_conn=T)
	invisible(orcv::receive(fd, simplify=FALSE))
}

get_locs <- function(method_arg) function(ids) {
	if (!length(ids) || identical(ids, 0L)) return(orcv::location(0))
	request <- paste0("GET ", method_arg, if (missing(ids)) NULL else paste(ids, collapse=','))
	fd <- orcv::send(LOCATOR(), request, keep_conn=T)
	locs <- orcv::payload(orcv::receive(fd, simplify=FALSE)[[1]])
	stopifnot(orcv::is.Location(locs),
		  length(locs) == length(ids))
	invisible(locs)
}
get_locations <- get_locs("/data/") # takes char vector of hrefs
get_host_locations <- get_locs("/host/") #takes char vector of hosts
get_least_loaded_locations <- get_locs("/node/") # takes integer n locations

# procedures = list of procs or char vector
# argument_lists = list of lists of args for each proc
# targets = list of targets aligned with other args
do.ccall <- function(procedures, argument_lists, targets, post_locs=TRUE) {
	locations <- determine_locations(argument_lists, targets)
	arguments_by_loc <- disperse_arguments(argument_lists, locations)
	comps_by_loc <- send_computations(procedures, arguments_by_loc, locations)
	comprefs <- unsplit(comps_by_loc, as.factor(locations))
	output_hrefs <- sapply(comprefs, output_href)
	if (post_locs) post_locations(output_hrefs, locations)
	mapply(ChunkReference, output_hrefs, locations, comprefs, SIMPLIFY=FALSE)
}
which_chunkref <- function(argument_lists)
	rapply(argument_lists, function(...) T, classes="ChunkReference", deflt=F, how="list")
determine_locations <- function(argument_lists, targets) {
	chunkref_args_i <- lapply(which_chunkref(argument_lists), simplify2array)
	locations <- mapply(function(arguments, targets, chunkref_args) {
		if (!is.na(targets)) {
			stopifnot(inherits(targets, "ChunkReference"))
			target$href
		} else if (!any(chunkref_args)) {
			TRUE # add to tally of least_loaded_locs needed
		} else if (!all(sapply(clocs <- lapply(arguments[chunkref_args], function(x) get("init_loc", x)), is.null))) {
			clocs[!sapply(clocs, is.null)][[1]] # pick the loc of first cached chunkref, if avail
		} else {
			arguments[chunkref_args][[1]]$href # pick the loc of first non-cached chunkref
		}}, argument_lists, if (missing(targets)) NA else targets, chunkref_args_i,
		SIMPLIFY=FALSE)
	hrefs <- sapply(locations, is.character)
	locations[hrefs] <- get_locations(locations[hrefs])
	no_loc <- sapply(locations, is.logical)
	locations[no_loc] <- get_least_loaded_locations(sum(no_loc))
	orcv::as.Location(unlist(locations, recursive=F))
}
disperse_arguments <- function(argument_lists, locations) {
	args_to_dest <- split(argument_lists, as.factor(locations))
	chunkref_args_i <- lapply(which_chunkref(args_to_dest), lapply, simplify2array)
	chunkref_arg_lists <- mapply(function(args_list, location, chunkref_args) {
			args <- unlist(args_list, recursive=F)
			local_args_i <- !unlist(chunkref_args)
			args[local_args_i] <- push(args[local_args_i], rep_len(location, sum(local_args_i)), post_locs=FALSE)
			relist(args, chunkref_args)
		},
		args_to_dest, unique(locations), chunkref_args_i,
		SIMPLIFY=FALSE)
	new_chunkrefs <- unlist(chunkref_arg_lists)[!unlist(chunkref_args_i)]
	new_chunkref_locs <- do.call(c, (mapply(rep,
						x=unique(locations),
						each=lapply(chunkref_args_i, function(x) sum(!unlist(x))),
						SIMPLIFY=FALSE)))
	post_locations(sapply(new_chunkrefs, href), new_chunkref_locs)
	chunkref_arg_lists
}
send_computations <- function(procedures, arguments_by_loc, locations) {
	comps_to_dest <- split(procedures, as.factor(locations))
	mapply(function(procs, args, location) {
			comprefs <- mapply(ComputationReference, procs, args, SIMPLIFY=FALSE)
			orcv::send(location,
				   paste0("PUT /computation/", paste(sapply(comprefs, href), collapse=',')),
				   comprefs)
			comprefs
		}, comps_to_dest, arguments_by_loc, unique(locations),
		SIMPLIFY=FALSE)
}

push <- function(x, locations, ...) UseMethod("push", x)
push.default <- function(x, locations, post_locs=TRUE, ...) { # returns list of chunkrefs
	locations <- if (missing(locations)) { 
                get_least_loaded_locations(1)
	} else if (!length(locations)) {
		return(list())
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
		post_data(sapply(x, href), lapply(x, data), locations)
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
	if (!length(x)) return()
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
	if (!length(hrefs)) return()
	stopifnot(is.character(hrefs))
	location <- get_locations(hrefs)
	hrefs_at_locs <- split(hrefs, as.factor(locations))
	mapply(function(loc, hrefs) orcv::send(location, paste0("GET /async/data/", paste(hrefs, collapse=','))),
	       unique(locations), hrefs_at_locs)
}

kill_all_nodes <- function() {
	orcv::send(LOCATOR(), "EXIT")
}
