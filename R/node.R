# Overview

node <- function(port) {
    orcv::start(port)
    PORT(port)
    register_external_handlers()
    register_internal_handlers()
    repeat {
        event <- next_event()
        handle(event)
        cat("...DONE\n")
    }
}

register_external_handlers <- function() {
    on("PUT /data/*", putData)
    on("PUT /computation/*", putComputation)
    on("GET /data/*", getData)
}
register_internal_handlers <- function() {
    on("prereqAvailable *", prereqAvailable)
    on("newComputation *", newComputation)
    on("newData *", putData)
    on("computationReady *", computationReady)
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
    cat("Pushing internal event: ", event, "\n")
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
    if (search(Store, request)) {
        orcv::respond(event, get(request, Store))
        orcv::event_complete(event)
    } else {
        already_requested <- search(Audience, request)
        assign(request,
               if (already_requested) c(get(request, Audience), event$fd) else event$fd,
               Audience)
    }
}

newData <- function(data) {
    assign(data$href, data, Store)
    if (search(Stage, data$href))
        event_internal_push(paste0("prereqAvailable ", data$href), data$href)
    if (search(Audience, data$href)) {
        fds <- get(data$href, Audience)
        for (fd in fds) {
            class(fd) <- c("FD", class(fd))
            orcv::respond(fd, data)
            orcv::event_complete(fd)
        }
        rm(list=data$href, pos=Audience)
    }
}
newComputation <- function(computation) {
    stage(computation, computation$arguments)
    avail <- search(Store, sapply(computation$arguments, "[[", "href"))
    for (prereq in computation$arguments[avail])
        event_internal_push(paste0("prereqAvailable ", prereq$href), prereq$href)
    for (prereq in computation$arguments[!avail]) {
        # ask for locations
        pull_async(prereq, location)
    }
}
prereqAvailable <- function(prereq_event) {
    prereq_href <- prereq_event$data$payload
    if (!search(Stage, prereq_href)) return(NULL)
    pendingComps <- get(prereq_href, Stage)
    for (i in length(pendingComps):1) {
        compCountDown <- pendingComps[[i]]
        compCountDown$remaining_prereqs <- compCountDown$remaining_prereqs - 1
        if (compCountDown$remaining_prereqs == 0L)
            event_internal_push(paste0("computationReady ", compCountDown$computation$href), compCountDown$computation)
            pendingComps <- pendingComps[-i]
    }
    if (!length(pendingComps)) rm(list=prereq_href, pos=Stage)
    orcv::event_complete(prereq_event)
}
computationReady <- function(computation_event) {
    computation <- computation_event$data$payload
    prereqs <- lapply(sapply(computation$arguments, "[[", "href"),
                      function(x) get(x, Store)$value)
    result <- do.call(computation$procedure, prereqs)
    event_internal_push(paste0("newData ", computation$output),
        list(generator_href=computation$href, value=result, href=computation$output))
    orcv::event_complete(computation_event)
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
        if (inherits(arg, "Data")) arg else push_nonblocking(arg, address, port))
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
        event_internal_push(paste0("computationReady ", input$href), input)
    } else {
        compCountDown <- new.env(parent=emptyenv())
        compCountDown$computation <- input
        compCountDown$remaining_prereqs <- length(prereqs)

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
