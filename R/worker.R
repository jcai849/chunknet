Worker <- new.env()
with(Worker, {
	DataStore <- new.env(emptyenv()) # data_href -> chunk (value/stub)
	CompStore <- new.env(emptyenv()) # (prereq|comp)_href -> computation
})

postData <- function(event) {
	data_href <- extract(event$data$header, "POST /data/(.*)")
	data <- event$data$payload
	if (exists(data_href, Worker$DataStore)) {
		stub <- get(data_href, Worker$DataStore)
		chunk <- add_data(stub, data)
		respond_audience(chunk)
	} else {
		assign(data_href, Chunk(data), worker$DataStore)
	}
	if (exists(data_href, Worker$CompStore)) {
		computations <- get(data_href, Worker$CompStore)
		ready_comps <- sapply(computations, all_data_avail)
		lapply(computations[ready_comps], run_comp)
	}
}

# TODO: add_data.Stub, all_data_avail.Computation, run_comp.Computation

getData <- function(event) {
	data_href <- extract(event$data$header, "GET /data/(.*)")
	if (! exists(data_href, Worker$DataStore)) {
		data <- assign(data_href, Stub(data_href, audience), Worker$DataStore)
	} else {
		data <- get(data_href, Worker$DataStore)
		respond_audience(data, event$fd)
	}
}

respond_audience <- function(x, ...) UseMethod(x, "respond_audience")
respond_audience.Stub <- function(x, ...) {
	x$audience <- c(x$audience, list(...))
}
respond_audience.Chunk <- function(x, ...) {
	lapply(c(x$audience, list(...)),
	       respond,
	       list(header=paste0("POST /data/", x$data_href), payload=x$data))
}

putComputation <- function(event) {
	computation_href <- extract(event$data$header, "PUT /computation/(.*)")
	computation <- event$data$payload
	# add to compstore under computation href
	# store comp in compstore under all prereq names
	# add finaliser to computation that removes transient prereq data from store
	# if no args, run comp
	# else:
	# - in datastore for prereqs if data doesn't exist, make stub and request it (pull_eventually), then replace all prereq references (transient and stable) in computation with their datastore forms
	# - if all values available, run comp
}

run_comp <- function(computation) {
	result <- tryCatch(do.call(computation$procedure, computation$arguments), error=identity)
	# TODO remove comp from compstore under own name and remove from all prereq lists (implicitly triggering gc)
	event_internal_push(paste0("POST /data/", computation$output), result)
}

deleteData <- function(event) {
       data_href <- extract(event$data$header, "DELETE /data/(.*)")
       log("Deleting data under href %s", href)
       if (length(href)) rm(list=href, pos=Worker$Store)
}
