\name{calc.missing}
\alias{calc.missing}
\title{
  Number of non-classified items in a free sort
}
\description{

  Returns the number of items not classified by each participant in a
  free sorting task.

}
\usage{
calc.missing(edta)
}
\arguments{
  \item{edta}{A data frame that includes the following columns:
    \code{SjNo, CategoryNo}, described below.}
}
\details{

  Dataset \code{lawson} provides an example of the format expected by
  the calc.numcats function. Each row is one classificatin decision by
  one participant. Only the following columns are used, other columns
  will be ignored:

  \code{SjNo} - integer; must be different for each participant.

  \code{CategoryNo} - integer. If the item has not been classified, NA
  must be recorded. Otherwise, a unique category number must be recorded.

}
\value{
  A vector of integers. Each number is the number of items not classified
  by one participant.
}

\author{
  Andy J. Wills (andy@willslab.co.uk)
}
\note{
  The primary purpose of this function is to calculate the \code{missing}
  argument of function \code{phi.intra.chance}. For this reason, only
  numbers of missing items are returned, not the associated subject
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
