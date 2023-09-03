post_locations <- function(hrefs, locations) {
	if (!length(hrefs)) return()
	stopifnot(is.character(hrefs), largescalemessages::is.Location(locations))
	stopifnot(length(locations) > 0,
		  length(hrefs) == length(locations))
	fd <- largescalemessages::send(LOCATOR(), paste0("POST /data/", paste(hrefs, collapse=',')), locations, keep_conn=T)
	invisible(largescalemessages::receive(fd, simplify=FALSE))
}

get_locs <- function(method_arg) function(ids) {
	if (!length(ids) || identical(ids, 0L)) return(largescalemessages::location(0))
	request <- paste0("GET ", method_arg, if (missing(ids)) NULL else paste(ids, collapse=','))
	fd <- largescalemessages::send(LOCATOR(), request, keep_conn=T)
	locs <- largescalemessages::payload(largescalemessages::receive(fd, simplify=FALSE)[[1]])
	stopifnot(largescalemessages::is.Location(locs))
	invisible(locs)
}
get_locations <- get_locs("/data/") # takes char vector of hrefs
get_host_locations <- get_locs("/host/") #takes char vector of hosts
get_least_loaded_locations <- get_locs("/node/") # takes integer n locations

dapply <- function(X, MARGIN, FUN, ..., balance=FALSE) UseMethod("dapply", X)
dapply.ChunkReferenceArray <- function(X, MARGIN, FUN, ..., balance=FALSE) {
	if (balance == TRUE) balance <- Balancer()
	resp <- apply(unclass(X), MARGIN,
		      function(x) {do.ccall(list(FUN), list(c(x, list(...))), balance=balance)})
	out_dim <- rep(1L, length(dim(X)))
	out_dim[MARGIN] <- dim(X)[MARGIN]
	ChunkReferenceArray(unlist(resp), out_dim)
}

# procedures = list of procs or char vector
# argument_lists = list of lists of args for each proc
# target = target chunkreference
do.ccall <- function(procedures, argument_lists, target, post_locs=TRUE, balance=FALSE) {
	locations <- determine_locations(argument_lists, target, balance)
	arguments_by_loc <- disperse_arguments(argument_lists, locations)
	comps_by_loc <- send_computations(procedures, arguments_by_loc, locations)
	comprefs <- unsplit(comps_by_loc, as.factor(locations))
	output_hrefs <- sapply(comprefs, output_href)
	if (post_locs) post_locations(output_hrefs, locations)
	mapply(ChunkReference, output_hrefs, locations, comprefs, SIMPLIFY=FALSE)
}

determine_locations <- function(argument_lists, target, balance=FALSE) {
	locations <- largescalemessages::location(length(argument_lists))
	no_locs <- integer()
	no_cache_i <- integer()
	no_cache_cr <- list()
	if (isTRUE(balance)) balance <- Balancer()

	if (!missing(target)) {
		locations[] <- if (!is.null(init_loc(target))) {
				init_loc(target)
			} else get_locations(href(target))
		return(locations)
	}

	for (i in seq_along(argument_lists)) {
		arg_list <- argument_lists[[i]]
		chunkref_i <- sapply(arg_list, inherits, "ChunkReference")
		if (!any(chunkref_i)) {
			no_locs <- c(no_locs, i)
		} else {
			cached_locs_store <- lapply(arg_list[chunkref_i], init_loc)
			cached_locs_i <- !sapply(cached_locs_store, is.null)
			if (any(cached_locs_i)) {
				cached_locs <- do.call(c, unname(cached_locs_store[cached_locs_i]))
				locations[i] <- select_from_locs(cached_locs, balance)
			} else {
				no_cache_i <- c(no_cache_i, i)
				no_cache_cr <- c(no_cache_cr, chunkref_i)
			}
		}
	}

	if (length(no_locs)) locations[no_locs] <- get_least_loaded_locations(length(no_locs))
	if (length(no_cache_i)) {
		no_cache_hrefs <- mapply(function(x, i) href(x[i]),
					argument_lists[no_cache_i], no_cache_cr, simplify=F)
		no_cache_locs <- relist(get_locations(simplify2array(no_cache_hrefs)), no_cache_hrefs)
		for (i in seq_along(no_cache_cr)) {
			locations[no_cache_i[i]] <- select_from_locs(no_cache_locs[[i]], balance)
		}
	}

	locations
}
select_from_locs <- function(locs, balance) UseMethod("select_from_locs", balance)
select_from_locs.Balancer <- function(locs, balance) {
		keys <- as.character(locs)
		new_locs <- ! keys %in% names(balance$used_locs)
		if (any(new_locs)) balance$used_locs[keys[new_locs]] <- 0L
		selected_i <- which.min(balance$used_locs[keys])
		balance$used_locs[keys][selected_i] <- balance$used_locs[keys][selected_i] + 1L
		locs[selected_i]
}
select_from_locs.logical <- function(locs, balance) locs[1]
Balancer <- function() {
	x <- structure(new.env(emptyenv()), class="Balancer")
	x$used_locs <- table(NULL)
	x
}

disperse_arguments <- function(argument_lists, locations) {
	args_to_dest <- split(argument_lists, as.factor(locations))
	chunkref_args_i <- lapply(args_to_dest, lapply, vapply, inherits, logical(1), "ChunkReference") 
	chunkref_arg_lists <- mapply(function(args_list, location, chunkref_args) {
			args <- do.call(c, args_list)
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
			largescalemessages::send(location,
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
	post_data(x$href, list(x$data), locations)
	x
}
push.list <- function(x, locations, ...) {
	if (!length(x)) return()
	if (all(sapply(x, inherits, "Chunk"))) {
		post_data(sapply(x, href), lapply(x, data), locations)
	} else NextMethod()
}

post_data <- function(hrefs, values, locations) {
	stopifnot(largescalemessages::is.Location(locations) || largescalemessages::is.FD(locations))
	stopifnot(is.character(hrefs),
		  length(hrefs) == length(locations))
	hrs <- split(hrefs, as.factor(locations))			# group hrefs by location
	vals <- split(values, as.factor(locations))			# group vals by location
	locs <- lapply(split(locations, as.factor(locations)), '[[', 1)	# get corresponding locs
	mapply(function(l, h, v) largescalemessages::send(l, paste0("POST /data/", paste(h, collapse=',')), v),
		locs, hrs, vals)
	invisible(NULL)
}

pull <- function(x, ...) UseMethod("pull", x)
pull.character <- function(x, ...) { # hrefs
	if (!length(x)) return()
	locations <- get_locations(x)
	hrefs_at_locs <- split(x, as.factor(locations))
	locs <- unique(locations)
	fds <- largescalemessages::as.FD(mapply(function(loc, hrefs)
				  	largescalemessages::send(loc, paste0("GET /data/", paste(hrefs, collapse=',')), keep_conn=T),
		      	          locs, hrefs_at_locs))
	unsplit(lapply(largescalemessages::receive(fds, simplify=FALSE),
                       function(x) {
				payload <- largescalemessages::payload(x)
				errors <- sapply(payload, inherits, "error")
				if (any(errors)) stop(payload[errors][[1]]) else payload
			}),
                as.factor(locations))
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
	locations <- get_locations(hrefs)
	hrefs_at_locs <- split(hrefs, as.factor(locations))
	mapply(function(location, hrefs) largescalemessages::send(location, paste0("GET /async/data/", paste(hrefs, collapse=','))),
	       unique(locations), hrefs_at_locs)
}

kill_all_nodes <- function() {
	largescalemessages::send(LOCATOR(), "EXIT")
}
