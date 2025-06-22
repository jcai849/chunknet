\name{pull}
\alias{pull}
\alias{pull.ChunkReference}
\alias{pull.character}
\alias{pull.list}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Pull data to current node
}
\description{
Synchronous pull of data to current node.
}
\usage{
pull(x, ...)
\method{pull}{ChunkReference}(x, ...)
\method{pull}{character}(x, ...)
\method{pull}{list}(x, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
Object to dispatch on.
}
  \item{\dots}{
Further arguments sent to methods.
}
}
\details{
Pull data from external source locally.
A vector of character hrefs yield a list of the referenced data that is then unsplit, with whatever method defined for unsplit on the data then determining the resultant return value.
A list of ChunkReferences returns their resultant values, as does a singular ChunkReference.
}
\value{
Value of the unsplit data sources.
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}

\seealso{
\code{\link{async_pull}}, \code{\link{push}}
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (x, ...) 
UseMethod("pull", x)
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
