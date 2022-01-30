# Overview

worker <- function(port) {
    orcv::start(port)
    PORT(port)
    register_external_handlers()
    register_internal_handlers()
    repeat {
        event <- next_event()
        handle(event)
        log("...DONE")
    }
}

register_external_handlers <- function() {
    on("PUT /data/*", putData)
    on("PUT /computation/*", putComputation)
    on("GET /data/*", getData)
}
register_internal_handlers <- function() {
    on("newData *", putData)
    on("newComputation *", newComputation)
    on("prereqIsAvailable *", prereqIsAvailable)
    on("computationIsReady *", computationIsReady)
}

on <- function(event, handler) {
    log("Adding handler for event %s", event)
    assign(event, handler, Events)
}

next_event <- function() {
    event <- orcv::event_pop()
    log("Pulling Event: %s", event$data$header)
    event
}

handle <- function(event) {
    events <- names(Events)
    event_name <- event$data$header
    log("Handling Event: %s", event_name)
    handler_name <- events[Vectorize(grepl)(glob2rx(events), event_name)]
    handler <- get(handler_name, Events)
    handler(event)
}

event_internal_push <- function(event, context) {
    log("Pushing internal event: %s", event)
    fd <- orcv::event_push(list(header=event, payload=context), "localhost", PORT())
    orcv::event_complete(fd)
}

# handlers
# Event {
#	FD fd
#	list data {
#                  character header
#                  (any) payload
#                 }
# }

putData <- function(event) {
    newData(event$data$payload)
    orcv::event_complete(event)
}
putComputation <- function(event) {
    newComputation(event$data$payload)
    orcv::event_complete(event)
}
getData <- function(event) {
    request <- event$data$payload
    if (search(Store, request)) {
        log("Data found. Sending %s to FD %d", request, event$fd)
        orcv::respond(event, get(request, Store))
        orcv::event_complete(event)
    } else {
        log("Data not found; Adding FD %d to Audience for %s", event$fd, request)
        assign(request, c(get0(request, Audience), event$fd), Audience)
    }
}

newData <- function(data) {
    log("Adding data %s to Store", data$href)
    assign(data$href, data, Store)
    if (search(Stage, data$href))
        event_internal_push(paste0("prereqIsAvailable ", data$href), data$href)
    if (search(Audience, data$href)) {
        fds <- get(data$href, Audience)
        for (fd in fds) {
            class(fd) <- "FD"
            log("Returning data %s to FD %d", data$href, fd)
            orcv::respond(fd, data)
            orcv::event_complete(fd)
        }
        log("Clearing Audience for data %s", data$href)
        rm(list=data$href, pos=Audience)
    }
}
newComputation <- function(computation) {
    log("Adding computation %s to Store", computation$href)
    assign(computation$href, computation, Store)
    argument_hrefs <- sapply(computation$arguments, "[[", "href")
    stage(computation$href, argument_hrefs)
}
prereqIsAvailable <- function(prereq_event) {
    prereq_href <- prereq_event$data$payload
    if (!search(PreReqs, prereq_href)) return(NULL)
    pending_comp_hrefs <- get(prereq_href, PreReqs)
    for (pending_comp_href in pending_comp_hrefs) {
        log("Accessing unaccounted prerequisites for pending computation %s from the Stage", pending_comp_href)
        unaccounted_prereqs <- get(pending_comp_href, Stage)
        log("Prerequisite %s of pending computation %s accounted for", prereq_href, pending_comp_href)
        assign(pending_comp_href, unaccounted_prereqs[unaccounted_prereqs != prereq_href], Stage)
        if (!length(unaccounted_prereqs)) {
            event_internal_push(paste0("computationIsReady ", pending_comp_href), pending_comp_href)
            log("Clearing pending computation %s from Stage", pending_comp_href)
            rm(list=pending_comp_href, pos=Stage)
        }
    }
    rm(list=prereq_href, pos=PreReqs)
    orcv::event_complete(prereq_event)
}
computationIsReady <- function(computation_event) {
    computation_href <- computation_event$data$payload
    log("Accessing computation %s from Store", computation_href)
    computation <- get(computation_href, Store)
    prereqs <- lapply(sapply(computation$arguments, "[[", "href"),
                      function(x) {log("Accessing argument %s from Store", x); get(x, Store)$value})
    result <- do.call(computation$procedure, prereqs)
    event_internal_push(paste0("newData ", computation$output),
        list(generator_href=computation$href, value=result, href=computation$output))
    orcv::event_complete(computation_event)
    # broadcast(computation)
}

# broadcast <- function(computation) # (mirror computation)

# client-requests

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

# globals

Store    <- new.env(parent=emptyenv())
Stage    <- new.env(parent=emptyenv())
PreReqs  <- new.env(parent=emptyenv())
Events   <- new.env(parent=emptyenv())
Audience <- new.env(parent=emptyenv())

# Prereq Staging

search <- function(collection, key, ...)
    key %in% names(collection)

stage <- function(computation_href, prereqs) {
    log("Searching Store for prereqs of computation %s", computation_href)
    avail <- search(Store, prereqs)
    if (length(prereqs) == 0L || all(avail)) {
        event_internal_push(paste0("computationIsReady ", computation_href), computation_href)
    } else {
        unaccounted_prereqs <- prereqs[!avail]
        log("Adding computation %s to Stage", computation_href)
        assign(computation_href, unaccounted_prereqs, Stage)
        for (unaccounted_prereq in unaccounted_prereqs) {
            log("Adding pending computation %s to prereq %s in PreReqs", computation_href, unaccounted_prereq)
            assign(unaccounted_prereq, c(get0(unaccounted_prereq, PreReqs), computation_href), PreReqs)
            pull_eventually(unaccounted_prereq)
        }
    }
}

# misc

PORT <- local(function(set) {
        if (!missing(set)) port <<- set
        else port
})
LOC <- local(function(address, port) {
    if (missing(address) && missing(port)) {
        list(address=address, port=port)
    } else {
        address <<- address
        port    <<- port
    }
})

log <- function(msg, ...) {
    cat(paste0(format(Sys.time(), "%H:%M:%OS9 "),
        sprintf(msg, ...), "\n"))
}

