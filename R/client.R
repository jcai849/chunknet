push <- function(value, location) {
    data <- list(generator_href= ".", value=value, href=uuid::UUIDgenerate())

    if (missing(location)) {
        location <- get_location(data)
        loc_header <- paste0("PUT /node/", location$href, "/", data$href)
        loc_fd <- orcv::event_push(list(header=loc_header, payload=list(node=location$href, data=data$href)))
        orcv::event_complete(loc_fd)
    }

    header <- paste0("PUT /data/", data$href)
    log("Pushing Event: %s", header)
    fd <- orcv::event_push(list(header=header, payload=data), location$address, location$port)
    orcv::event_complete(fd)
    structure(data, class="Data")
}

remote_call <- function(procedure, arguments, address, port) {
    location <- get_optimal_location(arguments)

    arguments <- lapply(arguments, function(arg)
        if (inherits(arg, "Data")) arg else push(arg, list(address=address, port=port)))
    computation <- list(procedure=procedure, arguments=arguments, alignments=NULL,
                        href=uuid::UUIDgenerate(), output=uuid::UUIDgenerate())

    loc_header <- paste0("PUT /node/", location$href, "/",computation$href, ",", computation$output)
    loc_fd <- orcv::event_push(list(header=loc_header,
                                    payload=list(node=location$href,
                                                 data=list(computation$href, computation$output))))
    orcv::event_complete(loc_fd)

    header <- paste0("PUT /computation/", computation$href)
    log("Pushing Event: %s", header)
    fd <- orcv::event_push(list(header=header, payload=computation), location$address, location$port)
    orcv::event_complete(fd)
    structure(computation, class="Computation")
}

pull <- function(href) {
    loc_header <- paste0("GET /data/", href)
    fd <- orcv::event_push(list(header=loc_header, payload=href))
    event <- orcv::await_response(fd)
    orcv::event_complete(event)
    location <- event$data

    header <- paste0("GET /data/", href)
    log("Pushing Event: %s", header)
    fd <- orcv::event_push(list(header=header, payload=href), location$address, location$port)
    event <- orcv::await_response(fd)
    orcv::event_complete(event$fd)
    event$data
}

pull_eventually <- function(href) {
    loc_header <- paste0("GET /data/", href)
    fd <- orcv::event_push(list(header=loc_header, payload=href))
    event <- orcv::await_response(fd)
    orcv::event_complete(event)
    location <- event$data

    header <- paste0("GET /data/", href)
    log("Pushing Event: %s", header)
    fd <- orcv::event_push(list(header=header, payload=href), location$address, location$port)
    monitor_response(fd)
}
