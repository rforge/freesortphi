\name{sig.con}
\alias{sig.con}
\title{
 Calculate significance level for Monte Carlo simulation
}
\description{

  Calculates probability an observed value comes from a distribution
  estimated by Monte Carlo methods.

}
\usage{
sig.con(x, val)
}
%- maybe also 'usage' for other objects documented here.
\arguments{

  \item{x}{numeric; observed value}

  \item{val}{vector; values from a Monte Carlo simulation}

}
\details{

  The function estimates the percentile occupied by value \code{x}
  within the distribution of numbers of \code{val}. One of the following
  estimates is returned:

  n.s., <.1, <.05, <.01

  where 'n.s.' >= .1.

  The significance value returned is 1-tailed, with the direction of the
  one-tailed test assumed from the relationship between \code{val} and
  the mean of \code{x}. For example, if \code{val = 0.8} and
  \code{mean(x) = 0.7}, then it is assumed the test concerns \code{val >
    mean}.

  The primary use of \code{sig.con} is to interpret the results of a
  Monte Carlo simulation conducted with \code{phi.intra.chance} or
  \code{phi.inter.rnd}. Examples of usage of \code{sign.con} are
  provided in the help file for those functions. The use of a one-tailed
  test is justified in these cases because the hypotheses are
  fundamentally directional (e.g. it makes little sense to ask whether
  phi-intra is below chance). 

}
\value{
  A character string indicating significance level (e.g. "< .05")
}

\author{
  Andy J. Wills (andy@willslab.co.uk)
}
\note{

 The quality of the estimate provided by this function depends on the
 quality of the estimated distribution provided in \code{val}. It is
 recommended that \code{val} contains the results of at least 1e5 Monte
 Carlo iterations.
  
}
\seealso{
  \code{\link{phi.intra.chance}}, \code{\link{phi.inter.rnd}}
}
