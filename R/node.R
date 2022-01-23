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
    on("PUT /data", putData)
    on("PUT /computation", putComputation)
    on("GET /data", getData)
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

next_event <- function() orcv::event_pop()

handle <- function(event) {
    handler <- get(event$data$header, Events)
    handler(event)
}

event_internal_push <- function(event, context) {
    fd <- orcv::event_push(list(header=event, payload=context), "localhost", PORT())
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
        location <- NULL
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
    result <- do.call(computation$computation, prereqs)
    event_internal_push("newData", result)
    broadcast(computation)
}

# broadcast <- function(computation) # (mirror computation)

# client-requests

push <- function(value, address, port) {
    data <- list(generator_href= ".", value=value, href=uuid::getUUID())
    fd <- orcv::event_push(list(header="PUT /data", payload=data), address, port)
    orcv::event_complete(fd)
    structure(data, class="Data")
}
remote_call <- function(procedure, arguments, address, port) {
    arguments <- lapply(arguments,function(arg) {if (inherits(arg, "Data") arg else push(arg, address, port))})
    computation <- list(procedure=procedure, arguments=arguments, alignments=NULL)
    fd <- orcv::event_push(list(header="PUT /computation", payload=computation), address, port)
    orcv::event_complete(fd)
    structure(computation, class="Computation")
}
pull <- function(href, address, port) {
    fd <- orcv::event_push(list(header="GET /data", payload=href))
    value <- orcv::await_response(fd)
    orcv::event_complete(fd)
    value
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
