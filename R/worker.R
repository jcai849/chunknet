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

store_data <- function(href, value) with(Worker,
	assign(href, value, Store)
)

data_avail <- function(href) with(Worker,
	href %in% ls(Store)
)

get_data <- function(href) with(Worker,
	get(href, Store)
)

set_computation_ready <- function(href) {
	event_internal_push(paste0("PUT /computation-ready/", href), NULL)
}

stage_computation <- function(computation, prereq_hrefs) with(Worker, {
	unaccounted_prereq_hrefs <- prereq_hrefs[!data_avail(prereq_hrefs)]
	if (length(unaccounted_prereq_hrefs)) {
		Stage <- rbind(Stage, data.frame(unaccounted_prereq_href=unaccounted_prereq_hrefs,
						 pending_computation_href=computation$href))
	} else {
		set_computation_ready(computation$href)
	}
})

register_data <- function(data_href) with(Worker, {
	now_accounted <- Stage$unaccounted_prereq_href %in% data_href
	pending_computations <- Stage$pending_computation_href[now_accounted]
	Stage <- Stage[!now_accounted,]
	now_non_pending <- !pending_computations %in% Stage$pending_computation_href
	ready_computations <- Stage$pending_computation_href[now_non_pending]
	lapply(non_pending_computations, set_computation_ready)
})

is_unaccounted_prereq <- function(data_href) with(Worker, 
	data_href %in% Stage$unaccounted_prereq_href
)

add_audience <- function(data_href, fd) with(Worker,
	Audience <- rbind(Audience, data.frame(fd=fd, data_href=data_href))
)

send_audience <- function(data_href) with(Worker, {
	fds <- Audience$fd[Audience$data_href %in% data_href]
	data <- get_data(href)
	mapply(respond, fds, data) 
})

data_has_audience <- function(data_href) with(Worker,
	data_href %in% Audience$data_href
)
