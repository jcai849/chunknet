\name{push}
\alias{push}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Push data to other node
}
\description{
Push data to another node's message queue.
}
\usage{
push(x, locations, ...)
push.default(x, locations, post_locs=TRUE,...)
push.list(x, locations, ...)
push.Chunk(x, locations, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
Argument to dispatch on. What item or contained item to send.

}
  \item{locations}{
Optional set of locations to send \code{x} to.
}
  \item{post_locs}{
Logical, to send locations of objects to the locator node or not.
}
  \item{\dots}{
Further arguments passed on to methods
}
}
\value{
List of ChunkReferences referring to the sent object(s).
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\seealso{
\code{\link{pull}}
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (x, locations, ...) 
UseMethod("push", x)
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
