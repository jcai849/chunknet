Worker <- new.env()
with(Worker, {
	DataStore <- new.env(emptyenv()) # data_href -> chunk (value/stub)
	CompStore <- new.env(emptyenv()) # (prereq|comp)_href -> env of computations
})

postData <- function(event) {
	href <- extract(event$data$header, "POST /data/(.*)")
	data <- event$data$payload
	register_posted_data(href, data)
}

register_posted_data <- function(href, data) {
	chunk <- Chunk(href, data)
	if (exists(href, Worker$DataStore)) {
		stub <- get(href, Worker$DataStore)
		respond_audience(chunk, stub$audience)
	}
	assign(href, chunk, Worker$DataStore)
	if (exists(href, Worker$CompStore)) {
		computations <- get(data_href, Worker$CompStore)
		eapply(computations, function(comp) if (all(data_avail(comp))) run_comp(comp))
	}
}

data_avail <- function(computation) {
	stopifnot(inherits(computation, "Computation"))
	sapply(computation$arguments, inherits, "Chunk")
}

getData <- function(event) {
	data_href <- extract(event$data$header, "GET /data/(.*)")
	audience <- event$fd
	data <- register_referenced_data(data_href, audience)
	respond_audience(data, audience)
}

register_referenced_data <- function(href, audience=integer()) { # fills out datastore
	if (!exists(href, Worker$DataStore)) {
		pull_eventually(href)
		assign(href, Stub(href, audience), Worker$DataStore)
	} else get(href, Worker$DataStore)
}

respond_audience <- function(x, audience, ...) UseMethod(x, "respond_audience")
respond_audience.Stub <- function(x, audience, ...) {
	x$audience <- c(x$audience, audience)
}
respond_audience.Chunk <- function(x, audience, ...) {
	lapply(audience,
	       respond,
	       list(header=paste0("POST /data/", x$href), payload=x$data))
}

putComputation <- function(event) {
	computation_href <- extract(event$data$header, "PUT /computation/(.*)")
	compref <- event$data$payload
	arguments <- lapply(compref$arguments, register_prereq, compref)
	computation <- Computation(compref, arguments)
	if (!length(computation$arguments) || all(data_avail(computation)) run_comp(computation)
}

register_prereq <- function(prereq, comp) {
	register_prereq(prereq, comp)
	register_referenced_data(prereq$href)
}

register_prereq <- function(prereq, comp) { # fills out compstore
	associated_comps <- if (exists(prereq$href, Worker$CompStore)) {
				get(prereq$href, Worker$CompStore)
			    } else {
				assign(prereq$href, new.env(parent=emptyenv()), Worker$CompStore)
			    }
	assign(comp$href, comp, associated_comps)
}

run_comp <- function(computation) {
	result <- tryCatch(do.call(computation$procedure, computation$arguments), error=identity)
	lapply(computation$arguments, prereq_cleanup) # some way to do implicitly with finaliser?
	register_posted_data(computation$href, result)
}

prereq_cleanup <- function(prereq) {
	comprefs <- get(prereq$href, Worker$CompStore)
	rm(list=computation$href, pos=comprefs)
	if (!length(comprefs)) rm(list=prereq$href, pos=Worker$CompStore)
}

deleteData <- function(event) {
       data_href <- extract(event$data$header, "DELETE /data/(.*)")
       log("Deleting data under href %s", href)
       if (length(href)) rm(list=href, pos=Worker$DataStore)
}
