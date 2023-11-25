\name{do.ccall}
\alias{do.ccall}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Chunk Call
}
\description{
A chunk function applicator
}
\usage{
do.ccall(procedures, argument_lists, target, post_locs = TRUE, balance = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{procedures}{
list of functions, or character vector naming functions
}
  \item{argument_lists}{
list containing argument lists corresponding to each procedure
}
  \item{target}{
Optional target ChunkReference
}
  \item{post_locs}{
Logical, send location of created chunk references to locator server or not.
}
  \item{balance}{
Logical of whether to balance results along the cluster, or a Balance function to apply the balancing.
}
}
\details{
The principal means of performing remote operations on chunks.
Returns immediately, without waiting for results of chunk operations.
}
\value{
List of ChunkReferences.
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
function (procedures, argument_lists, target, post_locs = TRUE, 
    balance = FALSE) 
{
    locations <- determine_locations(argument_lists, target, 
        balance)
    arguments_by_loc <- disperse_arguments(argument_lists, locations)
    comps_by_loc <- send_computations(procedures, arguments_by_loc, 
        locations)
    comprefs <- unsplit(comps_by_loc, as.factor(locations))
    output_hrefs <- sapply(comprefs, output_href)
    if (post_locs) 
        post_locations(output_hrefs, locations)
    mapply(ChunkReference, output_hrefs, locations, comprefs, 
        SIMPLIFY = FALSE)
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
