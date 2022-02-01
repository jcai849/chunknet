postNode <- function(event) {
    newNode(event$data$payload)
    orcv::event_complete(event)
}

getNodes <- function(event) {
    nodes <- eapply(Nodes, identity)
    orcv::respond(event, nodes)
    orcv::event_complete(event)
}

putDataLoc <- function(event) {
    node <- event$data$payload$node
    data <- event$data$payload$data
    dataAvail(node, data)
    orcv::event_complete(event)
}

getDataLoc <- function(event) {
    data_hrefs <- event$data$payload
    node_hrefs <- lapply(data_hrefs, get, Data)
    nodes <- lapply(node_hrefs, lapply, get, Nodes)
    orcv::respond(event, nodes)
    orcv::event_complete(event)
}

newNode <- function(node) {
    assign(node$href, node, Nodes)
}

dataAvail <- function(node, data) {
    for (datum in data) {
        assign(datum, c(get0(datum, Data), datum), Data)
    }
}

Nodes <- new.env(parent=emptyenv())
Data <- new.env(parent=emptyenv())

get_optimal_location <- function(data) {
    header <- paste0("GET /data/", paste0(data, collapse=","))
    fd <- orcv::event_push(list(header=header, payload=data))
    event <- orcv::await_response(fd)
    orcv::event_complete(event)
}

get_location <- function(data) {
    fd <- orcv::event_push(list(header="GET /nodes"), LOC()$address, LOC()$port)
    event <- orcv::await_response(fd)
    orcv::event_complete(event$fd)
    nodes <- event$data
    nodes[[sample(seq(length(nodes)))]]
}
