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
		orcv::respond(event, get_data(data_href))
		orcv::event_complete(event)
	} else {
		log("Data not found; Adding FD %d to Audience for %s", event$fd, data_href)
		add_audience(data_href, fd)
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
	computation_href <- extract(event$data$header, "PUT /computation/(.*)")
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

store_data <- function(href, value) with(Worker, {
	log("Storing data under href %s", href)
	assign(href, value, Store)
})

data_avail <- function(href) with(Worker, {
	log("Checking local availability of data %s", href)
	href %in% ls(Store)
})

get_data <- function(href) with(Worker, {
	log("Accessing data with href %s", href)
	get(href, Store)
})

set_computation_ready <- function(href) {
	log("Setting computation %s as ready", href)
	event_internal_push(paste0("PUT /computation-ready/", href), NULL)
}

stage_computation <- function(computation, prereq_hrefs) with(Worker, {
	log("Staging computation")
	unaccounted_prereq_hrefs <- prereq_hrefs[!data_avail(prereq_hrefs)]
	if (length(unaccounted_prereq_hrefs)) {
		log("Unaccounted prerequisite data added to stage")
		Stage <- rbind(Stage, data.frame(unaccounted_prereq_href=unaccounted_prereq_hrefs,
						 pending_computation_href=computation$href))
	} else {
		set_computation_ready(computation$href)
	}
})

register_data <- function(data_href) with(Worker, {
	log("Registering data %s and checking associated pending computations", data_href)
	now_accounted <- Stage$unaccounted_prereq_href %in% data_href
	pending_computations <- Stage$pending_computation_href[now_accounted]
	Stage <- Stage[!now_accounted,]
	no_longer_pending_computations <- ! pending_computations %in% Stage$pending_computation_href
	ready_computations <- Stage$pending_computation_href[no_longer_pending_computations]
	lapply(ready_computations, set_computation_ready)
})

is_unaccounted_prereq <- function(data_href) with(Worker, {
	log("Checking if %s is an unaccounted prerequisite", data_href)
	data_href %in% Stage$unaccounted_prereq_href
})

add_audience <- function(data_href, fd) with(Worker, {
	log("Adding an audience of %d to data href %s", fd, data_href)
	Audience <- rbind(Audience, data.frame(fd=fd, data_href=data_href))
})

send_audience <- function(data_href) with(Worker, {
	fds <- Audience$fd[Audience$data_href %in% data_href]
	log("Sending %s to audience of %d", data_href, fds)
	data <- get_data(href)
	mapply(respond, fds, data) 
})

data_has_audience <- function(data_href) with(Worker, {
	log("Checking if data %s has associated audience", data_href)
	data_href %in% Audience$data_href
})
