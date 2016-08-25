\name{phi.inter.rnd}
\alias{phi.inter.rnd}
\title{
  Inter-group Phi for Randomized Groups

}
\description{
  
  Randomize group membership, and then calculate inter-group phi as
  per function \code{\link{phi.inter}}. 

}
\usage{
phi.inter.rnd(iter, grp1, grp2)
}

\arguments{
  \item{iter}{Dummy variable; value is ignored}

  \item{grp1}{A data frame, for the first group of participants, that
    includes the following columns: \code{SjNo, CategoryNo, ItemNo},
    described below. Each row is one classification decision.}

  \item{grp2}{A data frame, for the second group of participants; format
    as for grp1.}
}

\details{

  Dataset \code{lawson} provides an example of the format expected by
  the phi.inter function. For further details, see \code{\link{phi.inter}}.

  The primary purpose of \code{phi.inter.rnd} is for Monte Carlo
  estimation of a chance level for \code{\link{phi.inter}}; an example
  of usage is given below. 

  The logic behind this calculation is that if two groups classify the
  stimulus set differently, their mean phi-inter will be lower than in
  the case where group membership is determined randomly.

  If this is not apparent, consult the brief tutorial available in the
  Appendix of Lawson et al. (2016).

  The phi-inter metric is based on Cramer (1946), slightly developed by
  Wills & McLaren (1998), and first employed in its current form by
  Lawson et al. (2016). 
  
}
\value{
  Phi-inter; a number ranging between 0 and 1.
}
\references{

  Cramer, H. (1946). \emph{Mathematical models of
  statistics}. Princeton, NJ: Princeton University Press.

  Lawson, R., Chang, F. & Wills, A.J. (2016). Free classification of
  large sets of everyday objects is more thematic than
  taxonomic. \emph{Manuscript submitted to Acta Psychologica}.

  Wills, A.J. & McLaren, I.P.L. (1998). Perceptual learning and free
  classification. \emph{Quarterly Journal of Experimental Psychology,
  51B}, 235-270.

}
\author{
  Andy J. Wills (andy@willslab.co.uk)
}
\seealso{
  \code{\link{phi.inter}}, \code{\link{sig.con}}
}
\examples{

  ## Estimate phi-inter chance level for the 'lawson' dataset, using 10
  ## iterations.

  data(lawson)
  grp1 <- lawson[lawson$ExptGroup == 1,]
  grp2 <- lawson[lawson$ExptGroup == 2,]
  iterate <- 10
  mc <- sapply(1:iterate,phi.inter.rnd,grp1=grp1,grp2=grp2)
  mean(mc)
  sig.con(0.6,mean(mc))

  ## In practice, a much larger number of iterations should be used.
  ## Lawson et al. (2016) use 1e5 iterations.

  ## At 1e5 iterations this function is very slow. The function has not
  ## yet been optimised, so peformance may improve in future versions of
  ## the package.

  ## If you have a multi-core processor (or Beowulf cluster), speed of
  ## calculation can be dramatically increased by use of the 'sfSapply'
  ## function in the 'snowfall' package. Example code follows:

  ## library(snowfall)
  ## library(rlecuyer)
  ## sfInit(parallel=TRUE,cpus=4,type="SOCK")
  ## sfClusterSetupRNG()
  ## iterate <- 40
  ## mc <- sfSapply(1:iterate,phi.inter.rnd,grp1=grp1,grp2=grp2)
  ## mean(mc)
  ## sfStop()

}