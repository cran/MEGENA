\name{set.parallel.backend}
\alias{set.parallel.backend}
\title{
register multiple cores for parallelization
}
\description{
Parallelization function, adapting "registerDoParallel()" with few safety features. 
}
\usage{
set.parallel.backend(num.cores = NULL)
}
\arguments{
  \item{num.cores}{an integer. number of cores to parallelize.}
}
\details{
returns 0 upon successful finish. 
}
\author{
Won-Min Song
}
\examples{
rm(list = ls())
library(MEGENA)
n.cores = 2;
set.parallel.backend(num.cores = n.cores)
# check how many workers are there
cat(paste("number of cores to use:",getDoParWorkers(),"\n",sep = ""))
}