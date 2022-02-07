postData <- function(event) {
	data_href <- extract(event$data$header, "POST /data/(.*)")
	store_data(data_href, event$data$payload)
	if (is_unaccounted_prereq(data_href)) {
		register_data(data_href)
	}
	if (data_has_audience(data_href)) {
		send_audience(data_href)
        }
}

getData <- function(event) {
	data_href <- extract(event$data$header, "GET /data/(.*)")
	if (data_avail(data_href)) {
		log("Data found. Sending %s to FD %d", data_href, event$fd)
		value <- get_data(data_href)
		respond(event, list(header=paste0("POST /data/", data_href), payload=value))
	} else {
		log("Data not found; Adding FD %d to Audience for %s", event$fd, data_href)
		add_audience(data_href, event$fd)
	}
}

putComputation <- function(event) {
	computation_href <- extract(event$data$header, "PUT /computation/(.*)")
	computation <- event$data$payload
	store_data(computation_href, computation)
	prereq_hrefs <- sapply(computation$arguments, "[[", "href")
	if (!length(prereq_hrefs)) {
		set_computation_ready(computation$href)
	} else {
		stage_computation(computation$href, prereq_hrefs)
	}
}

putComputationReady <- function(event) {
	computation_href <- extract(event$data$header, "PUT /computation-ready/(.*)")
	computation <- get_data(computation_href)
	prereq_hrefs <- sapply(computation$arguments, "[[", "href")
	arguments <- lapply(prereq_hrefs, get_data)
	result <- do.call(computation$procedure, arguments)
	event_internal_push(paste0("POST /data/", computation$output), result)
}


Worker <- new.env()
with(Worker, {
	Store <- new.env() # data_href -> value
	Stage <- data.frame(unaccounted_prereq_href=character(),
			    pending_computation_href=character())
	Audience <- data.frame(fd=integer(), data_href=character())
})

store_data <- function(href, value) {
	log("Storing data under href %s", href)
	assign(href, value, Worker$Store)
}

data_avail <- function(href) {
	log("Checking local availability of data %s", href)
	href %in% ls(Worker$Store)
}

get_data <- function(href) {
	log("Accessing data with href %s", href)
	get(href, Worker$Store)
}

set_computation_ready <- function(href) {
	log("Setting computation %s as ready", href)
	event_internal_push(paste0("PUT /computation-ready/", href), NULL)
}

stage_computation <- function(computation_href, prereq_hrefs) {
	log("Staging computation")
	unaccounted_prereq_hrefs <- prereq_hrefs[!data_avail(prereq_hrefs)]
	if (length(unaccounted_prereq_hrefs)) {
		Worker$Stage <- rbind(Worker$Stage, data.frame(unaccounted_prereq_href=unaccounted_prereq_hrefs,
							       pending_computation_href=computation_href))
		log("Unaccounted prerequisite data added to stage")
		locs <- lapply(unaccounted_prereq_hrefs, get_location)
		elsewhere <- ! sapply(locs, function(x) any(apply(x == SELF(), 1, all)))
		if (any(elsewhere)) log("Lazily requesting prereqs located elsewhere")
		lapply(unaccounted_prereq_hrefs[elsewhere], pull_eventually)
	} else {
		set_computation_ready(computation_href)
	}
}

register_data <- function(data_href) {
	log("Registering data %s and checking associated pending computations", data_href)
	now_accounted <- Worker$Stage$unaccounted_prereq_href %in% data_href
	associated_computations <- Worker$Stage$pending_computation_href[now_accounted]
	Worker$Stage <- Worker$Stage[!now_accounted,]
	ready_computations <- associated_computations[!associated_computations %in% Worker$Stage$pending_computation_href]
	lapply(ready_computations, set_computation_ready)
}

is_unaccounted_prereq <- function(data_href) {
	log("Checking if %s is an unaccounted prerequisite", data_href)
	data_href %in% Worker$Stage$unaccounted_prereq_href
}

add_audience <- function(data_href, fd) {
	log("Adding FD %d to the Audience of data href %s", fd, data_href)
	Worker$Audience <- rbind(Worker$Audience, data.frame(fd=unclass(fd), data_href=data_href))
}

send_audience <- function(data_href) {
	fds <- Worker$Audience$fd[Worker$Audience$data_href %in% data_href]
	class(fds) <- "FD"
	data <- get_data(data_href)
	log("Sending %s to Audience member FD %d", data_href, fds)
	for (fd in fds) respond(structure(fd, class="FD"), list(header=paste0("POST /data/", data_href), payload=data))
}

data_has_audience <- function(data_href) {
	log("Checking if data %s has associated audience", data_href)
	data_href %in% Worker$Audience$data_href
}
