Worker <- new.env()
with(Worker, {
	DataStore <- new.env(emptyenv()) # data_href -> [awaited]chunk
	CompStore <- new.env(emptyenv()) # (prereq|comp)_href -> env of computations
	WaitingFD <- data.frame(FD=orcv::as.FD(integer(0)), href=character(0))
})

postData <- function(event) {
	hrefs <- extract(orcv::header(event), "POST /data/(.*)")
	data <- orcv::payload(event)
	stopifnot(length(hrefs) == length(data))
	register_posted_data(hrefs, data)
}
register_posted_data <- function(hrefs, data) {
	chunks <- mapply(Chunk, hrefs, data)
	stubs_i <- hrefs %in% ls(Worker$DataStore)
	stubs <- mget(hrefs[stubs_i], Worker$DataStore)
	transfer_audience(stubs, chunks[stubs_i])
	for (i in seq_along(hrefs)) assign(hrefs[[i]], chunks[[i]], Worker$DataStore)
	prereq_i <- hrefs %in% ls(Worker$CompStore)
	for (i in seq_along(hrefs[prereq_i])) {
		computations <- get(hrefs[prereq_i][i], Worker$CompStore)
		eapply(computations, update_comp_args, chunks[prereq_i][[i]])
	}
}
transfer_audience <- function(stubs, chunks) {
	if (!length(stubs)) return()
	transfer_loc_audience(stubs, chunks)
	transfer_fd_audience(stubs, chunks)
}
transfer_loc_audience <- function(stubs, chunks) {
	loc_audiences <- lapply(stubs, function(x) get("audience", x))
	stubs_with_loc_audience_i <- lengths(loc_audiences) > 0L
	mapply(push, chunks[stubs_with_loc_audience_i], loc_audiences[stubs_with_loc_audience_i])
}
transfer_fd_audience <- function(stubs, chunks) {
	fd_audiences <- sapply(stubs, href) 
	stubs_with_fd_audience_i <- fd_audiences %in% Worker$WaitingFD$href
	for (chunk in chunks[stubs_with_fd_audience_i]) {
		avail_fds_i <- Worker$WaitingFD$href %in% href(chunk)
		for (fd in Worker$WaitingFD$FD[avail_fds_i]) {
			fd_i <- Worker$WaitingFD$FD == fd
			chunk_hrefs_for_fd <- Worker$WaitingFD$href[fd_i]
			chunk_response <- mget(chunk_hrefs_for_fd, Worker$DataStore)
			chunk_response[[href(chunk)]] <- chunk
			if (all(available(chunk_response))) {
				push(chunk_response, orcv::as.FD(fd))
				Worker$WaitingFD <- Worker$WaitingFD[!fd_i,]
			}
		}
	}
}
update_comp_args <- function(computation, chunk) {
	stopifnot(inherits(computation, "Computation"))
	stopifnot(inherits(chunk, "Chunk"))
	computation$arguments[[match(chunk$href, sapply(computation$arguments, "[[", "href"))]] <- chunk
	if (all(data_avail(computation))) run_comp(computation)
}

get_data <- function(header_extraction, audience_extraction) {
	function(event) {
		data_hrefs <- extract(orcv::header(event), header_extraction)
		audience <- audience_extraction(event)
		chunks <- register_referenced_data(data_hrefs)
		register_audience(rep(audience, length(chunks)), chunks)
	}
}
getData <- get_data("GET /data/(.*)", orcv::fd)
asyncGetData <- get_data("GET /async/data/(.*)", orcv::location)
register_referenced_data <- function(hrefs) {
	external_chunks_i <- ! hrefs %in% ls(Worker$DataStore)
	async_pull(hrefs[external_chunks_i])
	external_chunks <- lapply(hrefs[external_chunks_i],
				  function(href) assign(href, AwaitedChunk(href), Worker$DataStore))
	internal_chunks <- mget(hrefs[!external_chunks_i], Worker$DataStore)
	chunks <- vector("list", length(hrefs))
	chunks[external_chunks_i] <- external_chunks
	chunks[!external_chunks_i] <- internal_chunks
	chunks
}
register_audience <- function(audience, chunks) {
	UseMethod("register_audience", audience)
}
register_audience.FD <- function(audience, chunks) {
	if (all(available(chunks))) {
		push(chunks, audience) 
	} else {
		Worker$WaitingFD <- rbind(Worker$WaitingFD,
					data.frame(FD=audience, href=sapply(chunks, href)))
	}
}
register_audience.Location <- function(audience, chunks) {
	avail <- available(chunks)
	push(chunks[avail], audience)
	if (!all(avail)) mapply(assign, "audience", audience, chunks[!avail])
}

putComputation <- function(event) {
	computation_hrefs <- extract(orcv::header(event), "PUT /computation/(.*)")
	comprefs <- orcv::payload(event)
	for (compref in comprefs) {
		arguments <- register_referenced_data(sapply(compref$arguments, href))	# fills out datastore
		names(arguments) <- names(compref$arguments)
		computation <- Computation(compref, arguments)
		register_prereqs(arguments, computation)				# fills out compstore
		assign(computation$output_href, AwaitedChunk(computation$output_href), Worker$DataStore)
		if (!length(computation$arguments) || all(data_avail(computation))) run_comp(computation)
	}
}
register_prereqs <- function(prereqs, comp) {
	lapply(prereqs, function(prereq, comp) {
		associated_comps <- if (exists(prereq$href, Worker$CompStore)) {
					get(prereq$href, Worker$CompStore)
				    } else {
					assign(prereq$href, new.env(parent=emptyenv()), Worker$CompStore)
				    }
		assign(comp$href, comp, associated_comps)
	}, comp)
}
data_avail <- function(computation) {
	stopifnot(inherits(computation, "Computation"))
	available(computation$arguments)
}
available <- function(chunk) sapply(chunk, inherits, "Chunk")
run_comp <- function(computation) {
	args <- lapply(computation$arguments, "[[", "data")
	log("Running computation")
	result <- tryCatch(do.call(computation$procedure, args), error=identity)
	log("Completed computation")
	lapply(computation$arguments, prereq_cleanup, computation) # some way to do implicitly with finaliser?
	register_posted_data(computation$output_href, list(result))
}
prereq_cleanup <- function(prereq, associated_computation) {
	comprefs <- get(prereq$href, Worker$CompStore)
	rm(list=associated_computation$href, pos=comprefs)
	if (!length(comprefs)) rm(list=prereq$href, pos=Worker$CompStore)
}

deleteData <- function(event) {
       hrefs <- extract(orcv::header(event), "DELETE /data/(.*)")
       if (length(hrefs)) rm(list=hrefs, pos=Worker$DataStore)
}
