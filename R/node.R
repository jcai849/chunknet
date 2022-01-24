# Overview

node <- function(port) {
    orcv::start(port)
    PORT(port)
    register_external_handlers()
    register_internal_handlers()
    repeat {
        event <- next_event()
        handle(event)
    }
}

register_external_handlers <- function() {
    on("PUT /data/*", putData)
    on("PUT /computation/*", putComputation)
    on("GET /data/*", getData)
}
register_internal_handlers <- function() {
    on("prereqAvailable", prereqAvailable)
    on("newComputation", newComputation)
    on("newData", newData)
    on("computationReady", computationReady)
}

on <- function(event, handler) {
    assign(event, handler, Events)
}

next_event <- function() {
    event <- orcv::event_pop()
    cat("Pulling Event: ", event$data$header, "\n")
    event
}

handle <- function(event) {
    events <- names(Events)
    event_name <- event$data$header
    cat("Handling Event: ", event_name, "\n")
    handler_name <- events[Vectorize(grepl)(glob2rx(events), event_name)]
    handler <- get(handler_name, Events)
    handler(event)
}

event_internal_push <- function(event, context) {
    fd <- orcv::event_push(list(header=event, payload=context), "localhost", PORT())
    cat("Pushing internal event: ", event, "\n")
    orcv::event_complete(fd)
}

# handlers
# Event {
#	FD connection
#	list data {
#                  character header
#                  (any) payload
#                 }
# }

putData <- function(event) {
    newData(event$data$payload)
    # async_respond(event, "204")
    orcv::event_complete(event)
}
putComputation <- function(event) {
    newComputation(event$data$payload)
    # async_respond(event, "204")
    orcv::event_complete(event)
}
getData <- function(event) {
    request <- event$data$payload
    orcv::respond(event, get(request, Store))
    orcv::event_complete(event)
}

newData <- function(data) {
    assign(data$href, data, Store)
    if (search(Stage, data$href))
        event_internal_push("prereqAvailable", data$href)
  # TODO: if (some client wanting this data) send(data, to_client); remove from clientelle
}
newComputation <- function(computation) {
    stage(computation, computation$prereqs)
    avail <- search(Store, computation$prereqs)
    for (prereq in computation$prereqs[avail])
        event_internal_push("prereqAvailable", prereq$href)
    for (prereq in computation$prereqs[!avail]) {
        # ask for locations
        pull_async(prereq, location)
    }
}
prereqAvailable <- function(prereq_href) {
    if (!search(Stage, prereq_href)) return(NULL)
    pendingComps <- get(prereq_href, Stage)
    for (i in length(pendingComps):1) {
        compCountDown <- pendingComps[[i]]
        compCountDown$remaining_prereqs <- compCountDown$remaining_prereqs - 1
        if (compCountDown$remaining_prereqs == 0L)
            event_internal_push("computationReady", compCountDown$computation)
            pendingComps <- pendingComps[-i]
    }
    if (pendingComps == list()) rm(prereq_href, Stage)
}
computationReady <- function(computation) {
    prereqs <- lapply(sapply(computation$prereqs, '$', href),
                      function(x) get(x, Store)$value)
    result <- do.call(computation$procedure, prereqs)
    event_internal_push("newData", list(href=computation$output, value=result))
    # broadcast(computation)
}

# broadcast <- function(computation) # (mirror computation)

# client-requests

push_nonblocking <- function(value, address, port) {
    data <- list(generator_href= ".", value=value, href=uuid::UUIDgenerate())
    header <- paste0("PUT /data/", data$href)
    cat("Pushing Event: ", header, "\n")
    fd <- orcv::event_push(list(header=header, payload=data), address, port)
    orcv::event_complete(fd)
    structure(data, class="Data")
}
remote_call_nonblocking <- function(procedure, arguments, address, port) {
    arguments <- lapply(arguments, function(arg)
        if (inherits(arg, "Data")) arg else orcv::event_push(arg, address, port))
    computation <- list(procedure=procedure, arguments=arguments, alignments=NULL,
                        href=uuid::UUIDgenerate(), output=uuid::UUIDgenerate())
    header <- paste0("PUT /computation/", computation$href)
    cat("Pushing Event: ", header, "\n")
    fd <- orcv::event_push(list(header=header, payload=computation), address, port)
    orcv::event_complete(fd)
    structure(computation, class="Computation")
}
pull_blocking <- function(href, address, port) {
    header <- paste0("GET /data/", href)
    cat("Pushing Event: ", header, "\n")
    fd <- orcv::event_push(list(header=header, payload=href), address, port)
    event <- orcv::await_response(fd)
    orcv::event_complete(event$fd)
    event$data
}

# globals

Store <- new.env(parent=emptyenv())
Stage <- new.env(parent=emptyenv())
Events <- new.env(parent=emptyenv())
Audience <- new.env(parent=emptyenv())

# Prereq Staging

search <- function(collection, key, ...)
    key %in% names(collection)

stage <- function(input, prereqs) {
    if (length(prereqs) == 0L) {
        event_internal_push(computationReady, input)
    } else {
        compCountDown <- new.env(parent=emptyenv())
        compCountDown$computation <- input
        compCoutnDown$remaining_prereqs <- length(prereqs)

        for (prereq in prereqs) {
            already_staged <- search(Stage, prereq$href)
            assign(prereq$href,
                   if (already_staged) c(get(prereq$href, Stage), compCountDown) else list(compCountDown),
                   Stage)
        }
    }
}

# misc

PORT <- local(function(set) {
        if (!missing(set)) port <<- set
        else port
})
