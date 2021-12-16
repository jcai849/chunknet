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
next_internal_event <- function() {
        event <- Events$queue[[1]]
        Events$queue <- Events$queue[-1]
        event
}
emit <- function(callback, context) {
    request <- new.env(parent=emptyenv())
    request$callback <- callback
    request$context <- context
    Events$queue <- c(Events$queue, request)
}
next_external_event <- function() {
    # TODO: poll on clients and servers as well as main listener
    receive.socket(Replier)
}

parse <- function(input) {
    switch(input$header,
           "PUT /data" = list(callback=putData, context=input$payload),
           "PUT /computation" = list(callback=putComputation, context=input$payload),
           "GET /data" =  list(callback=getData, context=input$payload))
}
handle <- function(callback, context)
    callback(context)
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
            already_staged <- search(Stage, prereq$href)
            assign(prereq$href,
                   if (already_staged) c(get(prereq$href, Stage), compCountDown) else list(compCountDown),
                   Stage)
        }
    }
}

# callbacks

putData <- function(data) {
    send.socket(Replier, "204")
    newData(data)
}
putComputation <- function(computation) {
    send.socket(Replier, "204")
    newComputation(computation)
}
getData <- function(whatwhere) {
    send.socket(Replier, get(whatwhere$what, Store))
}

newData <- function(data) {
    assign(data$href, data, Store)
    if (search(Stage, data$href))
        emit(prereqAvailable, data$href)
  # TODO: if (some client wanting this data) send(data, to_client); remove from clientelle
}
newComputation <- function(computation) {
    stage(computation, computation$prereqs)
    avail <- search(Store, computation$prereqs)
    for (prereq in computation$prereqs[avail])
        emit(prereqAvailable, prereq$href)
    for (prereq in computation$prereqs[!avail])
        pull_async(prereq$href)
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
    prereqs <- lapply(sapply(computation$prereqs, '$', href),
                      function(x) get(x, Store)$value)
    result <- do.call(computation$computation, prereqs)
    emit(newData, result)
    broadcast(computation)
}

# client-requests

push_async <- function(value, location) {
    # TODO: replace location argument with location service
    data <- list(generator_href= ".", value=value, href=getUUID())
    sock <- init.socket(Context, "ZMQ_REQ")
    connect.socket(sock, location)
    send.socket(sock, list(header="PUT /data", payload=data))
    # TODO: Add to clients list to poll on if available
    sock
}
push_sync <- function(value, location) {
    sock <- push_async(value, location)
    data <- receive.socket(sock)
    disconnect.socket(sock)
    data

}
remote_call <- function(procedure, arguments, location) {
    arguments <- lapply(arguments, push)
    computation <- list(procedure=procedure, arguments=arguments, alignments=NULL)
    sock <- init.socket(Context, "ZMQ_REQ")
    connect.socket(sock, location)
    send.socket(sock, list(header="PUT /computation", payload=computation))
    receive.socket()
    disconnect.socket(sock)
}
pull_async <- function(href, location) {
    # TODO: replace location argument with location service
    # TODO: if value apparently missing, try to recover the data through its generating computation
    sock <- init.socket(Context, "ZMQ_REQ")
    connect.socket(sock, location)
    send.socket(sock, list(header="GET /data", payload=href))
    # TODO: add to servers list to poll on if available
    sock
}
pull_sync <- function(href, location) {
    sock <- pull_async(href, location)
    data <- receive.socket()
    disconnect.socket(sock)
    data
}

# globals

Store <- new.env(parent=emptyenv())
Stage <- new.env(parent=emptyenv())
Events <- new.env(parent=emptyenv())
Audience <- new.env(parent=emptyenv())

comms <- (function() {
    Context <- init.context()
    Replier <- init.socket(Context, "ZMQ_REP")
    init_comms <- function(port)
        bind.socket(Replier, paste0("tcp://127.0.0.1:", port))
    list(init_comms = init_comms,
         Replier = Replier,
         Context = Context)
})()

init_comms <- comms$init_comms
Context <- comms$Context
Replier <- comms$Replier
broadcast <- base::identity
