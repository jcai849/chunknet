\name{async_pull}
\alias{async_pull}
\title{
Asynchronous Data Pull
}
\description{
Send a request for data as referenced by a list of \code{href}'s, with the response to be returned asynchronously.
}
\usage{
async_pull(hrefs, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{hrefs}{
Character vector of href references for data objects
}
  \item{\dots}{
Kept for future methods
}
}
\details{
Locations for the data referenced by the \code{href} vector are attained, with the containing nodes requested to send the data when available.
Data can be obtained from the underlying message queue after it is posted.
}
\value{
Closed file descriptors of the locations sought.
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\seealso{
\code{\link{pull}}, \code{\link{push}}
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (hrefs, ...) 
{
    if (!length(hrefs)) 
        return()
    stopifnot(is.character(hrefs))
    locations <- get_locations(hrefs)
    hrefs_at_locs <- split(hrefs, as.factor(locations))
    mapply(function(location, hrefs) orcv::send(location, paste0("GET /async/data/", 
        paste(hrefs, collapse = ","))), unique(locations), hrefs_at_locs)
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
