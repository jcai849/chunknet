comms <- (function() {
    init_comms <- function(port) {
        context <- init.context()
        Replier <<- init.socket(context, "ZMQ_REP")
        bind.socket(replier, paste0("tcp://127.0.0.1:", port))
        Requester <<- init.socket(context, "ZMQ_REQ")
    }
    list(init_comms = init_comms,
         Replier = function() Replier,
         Requester = function() Requester)
})()

init_comms <- comms$init_comms
Replier <- comms$Replier
Requester <- comms$Requester

node <- function(port) {
    init_comms(port)
    repeat {
        request <- next_event()
        handle(request$callback, request$context)
    }
}

next_event <- function() {
    event <- next_internal_event()
    if (is.null(internal_event)) {
        event <- parse(next_external_event())
    }
    return(event)
}

parse <- base::identity # (HTTP later)

handle <- function(callback, context) {
    callback(context)
}

DataStore <- new.env(parent=emptyenv())
Stage <- new.env(parent=emptyenv())
EventQueue <- new.env(paren=emptyenv())

next_internal_event <- function() {
        event <- EventQueue$queue[[1]]
        EventQueue$queue <- EventQueue[-1]
        event
}
next_external_event <- function() {
    receive.socket(Replier)
}
emit <- function(callback, context)
    EventQueue$queue <- c(EventQueue$queue, list(callback=callback, context=context))

prerequisites <- function(x) x$prereqs
search <- function(collection, key, ...)
    key %in% names(collection)

stage <- function(input, prereqs) {
    if (length(prereqs) == 0L) {
        emit(computationReady, input)
    } else {
        compCountDown <- new.env(parent=emptyenv())
        compCountDown$computation <- input
        compCoutnDown$remaining_prereqs <- length(prereqs)

        for (prereq in prereqs) {
            already_staged <- search(Stage, href(prereq))
            assign(href(prereq),
                   if (already_staged) c(get(href(prereq), Stage), compCountDown) else list(compCountDown),
                   Stage)
        }
    }
}

# callbacks

newData <- function(data) {
    assign(data$href, data, DataStore)
    if (search(Stage, data$href))
        emit(prereqAvailable, data$href)
}
newComputation <- function(computation) {
    stage(computation, prerequisites(computation))
    avail <- search(DataStore, prerequisites(computation))
    for (prereq in prerequisites(computation)[avail])
        emit(prereqAvailable, href(prereq))
    for (prereq in prerequisites(computation)[!avail])
        nonblocking_emerge(href(prereq))
}
prereqAvailable <- function(prereq_href) {
    if (!search(Stage, prereq_href)) return(NULL)
    pendingComps <- get(prereq_href, Stage)
    for (i in length(pendingComps):1) {
        compCountDown <- pendingComps[[i]]
        compCountDown$remaining_prereqs <- compCountDown$remaining_prereqs - 1
        if (compCountDown$remaining_prereqs == 0L)
            emit(computationReady, compCountDown$computation)
            pendingComps <- pendingComps[-i]
    }
    if (pendingComps == list()) rm(prereq_href, Stage)
}
computationReady <- function(computation) {
    prereqs <- lapply(sapply(prerequisites(computation), href), get, DataStore)
    result <- do.call(computation$computation, prereqs)
    broadcast(computation)
    assign(computation$result$href, result, DataStore)
}
getData <- function(whatwhere) {
    send(get(whatwhere$what, DataStore), whatwhere$where)
}

# client-requests

push <- function(x, location) {
    getUUID()
    list("PUT .../data", x)
    send.socket()
    receive.socket()

}
remote_call <- function(function, arguments, location) {
    arguments <- lapply(arguments, push)
    list("PUT .../computation", computation(function, arguments))
    send.socket()
}
