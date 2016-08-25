\name{calc.numcats}
\alias{calc.numcats}
\title{
  Number of categories used in a free sort
}
\description{

  Returns the number of categories used by each participant in a free
  sorting task.

}
\usage{
calc.numcats(edta)
}
\arguments{
  \item{edta}{A data frame that includes the following columns:
    \code{SjNo, CategoryNo}, described below.}
}
\details{

  Dataset \code{lawson} provides an example of the format expected by
  the calc.numcats function. Only the following columns are used, other
  columns will be ignored:

  \code{SjNo} - integer; must be different for each participant.

  \code{CategoryNo} - integer. Each group that participant produced must
  have a unique number. Other than that, the numbers are arbitrary.

}
\value{
  A vector of integers. Each number is the number of categories produced
  by one participant.
}

\author{
  Andy J. Wills (andy@willslab.co.uk)
}
\note{
  The primary purpose of this function is to calculate the \code{numcat}
  argument of function \code{phi.intra.chance}. For this reason, only
  numbers of categories are returned, not the associated subject
  numbers. 
}
\seealso{
  \code{\link{phi.intra.chance}}
}
\examples{
  data(lawson)
  edta <- lawson[lawson$ExptGroup == 2,]
  calc.numcats(edta)
}
