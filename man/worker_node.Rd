\name{worker_node}
\alias{worker_node,locator_node}
\title{
Cluster node initialisers
}
\description{
Initiate one of either worker or locator node.
}
\usage{
worker_node(address = NULL, port = 0L, ..., verbose = FALSE)
locator_node(address = NULL, port = 0L, ..., verbose = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{address}{
 character address for the communication node to be reachable by. Leave \code{NULL} for localhost.
}
  \item{port}{
Integer port to bind to.
}
  \item{\dots}{
Arguments to pass on to methods
}
  \item{verbose}{
Logical, start verbose or not.
}
}
\value{
None; loops.
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (address = NULL, port = 0L, ..., verbose = FALSE) 
{
    options(chunknetVerbose = verbose)
    orcv::start(address, port, threads = 1L)
    init_function(...)
    repeat {
        event <- orcv::receive(keep_conn = TRUE)
        handle(event)
        log("...DONE")
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
