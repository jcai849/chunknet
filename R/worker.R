Worker <- new.env()
with(Worker, {
	DataStore <- new.env(emptyenv()) # data_href -> chunk (value/stub)
	CompStore <- new.env(emptyenv()) # prereq_href -> computation
})

postData <- function(event) {
	data_href <- extract(event$data$header, "POST /data/(.*)")
	store_data(data_href, event$data$payload)
	# Check if stub in store; if it has waiting audience, send
	# put data value in datastore 
	# Check if it has computation, alert if so, run comp if all prereqs as values in store
}

getData <- function(event) {
	data_href <- extract(event$data$header, "GET /data/(.*)")

	# following all generic:
	# If data doesn't exist, make stub and add audience
	# if stub exists add audience
	# Check if data exists, send if so

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
	# add to compstore under computation href
	# if no args, run comp
	# else add finalisers to all transient args in comp
	# and in datastore for prereqs: 
	# - if data doesn't exist, make stub and request it (pull_eventually)
	# - if all values, run comp
	# store comp in compstore under all prereq names
}

# don't use as event; just as function
putComputationReady <- function(event) {
	computation_href <- extract(event$data$header, "PUT /computation-ready/(.*)")
	# take comp from store under computation href
	# pull all prereqs from datastore
	# Run comp
	# remove comp from compstore under own name and all prereqs (implicitly triggering gc)
	# send result as data
	computation <- get_data(computation_href)
	prereq_hrefs <- sapply(computation$arguments, "[[", "href")
	arguments <- lapply(prereq_hrefs, get_data)
	result <- tryCatch(do.call(computation$procedure, arguments), error=identity)
        lapply(prereq_hrefs, guarded_delete) # cleanup
	event_internal_push(paste0("POST /data/", computation$output), result)
}

deleteData <- function(event) {
       data_href <- extract(event$data$header, "DELETE /data/(.*)")
       delete_data(data_href)
}


store_data <- function(href, value) {
	log("Storing data under href %s", href)
	# Check if it exists and copy over if so
	assign(href, value, Worker$DataStore)
}

data_avail <- function(href) {
	log("Checking local availability of data %s", href)
	href %in% ls(Worker$DataStore)
}

get_data <- function(href) {
	log("Accessing data with href %s", href)
	get(href, Worker$DataStore)
}

send_audience <- function(data_href) {
	data <- get_data(data_href)
	# Get fds from data
	log("Sending %s to Audience member FD %d", data_href, fds)
	lapply(fds, respond, list(header=paste0("POST /data/", data_href), payload=data))
}

delete_data <- function(href) {
       log("Deleting data under href %s", href)
       if (length(href)) rm(list=href, pos=Worker$Store)
}
