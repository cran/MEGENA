\name{calculate.PFN}
\alias{calculate.PFN}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
PFN calculation
}
\description{
main function to calculate PFN a ranked list of edge pairs
}
\usage{
calculate.PFN(edgelist,doPar = FALSE,num.cores = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{edgelist}{three column edgelist: first two columns are topological edges, and the third column is the weight. Must be a data.frame object.}
  \item{doPar}{TRUE/FALSE logical variable to choose parallelization.}
  \item{num.cores}{number of cores to use in parallelization.}
}
\details{
If \code{doPar = TRUE}, then num.cores are registered for PCP. 
}
\value{
 output is three column edgelist data.frame, third column being the weight.
}
\author{
Won-Min Song
}
\examples{
# test simplest case of planar network (a 3-clique).
a <- c(1,1,2);b <- c(2,3,3);w <- runif(3,0,1);
el <- cbind(a,b,w);el <- as.data.frame(el[order(el[,3],decreasing = TRUE),])
calculate.PFN(edgelist = el,doPar = FALSE,num.cores = NULL)
}