\name{phi.intra.chance}
\alias{phi.intra.chance}
\title{
  Intra-group Phi for Random Responses
}
\description{

  Calculate intra-group phi for one set of simulated participants who use
  the number of categories specified in \code{numcat} but who otherwise
  respond randomly. 

}
\usage{
phi.intra.chance(iter = 1, numcat, nitems, missing=NULL)
}

\arguments{
  
  \item{iter}{Dummy variable; value is ignored} 

  \item{numcat}{vector of integers; each integer is the number of
    categories produced by one simulated participant}

  \item{nitems}{integer; the number of stimulus items in the free sort set}

  \item{missing}{vector of integers; each integer is the number of items
    not classified by one simulated participant}
}
\details{

  Calculates \code{\link{phi.intra}} for a set of simulated participants
  who free sort the number of stimuli specified by \code{nitems} into
  the number of categories specified in \code{numcat}, but who otherwise
  respond randomly.

  \code{numcat} should include one integer value for each simulated
  participant. For example, if you wish to simulate five participants,
  three of whom use 9 groups and the remainder who use 12 groups, then:

  numcat <- c(9,9,9,12,12)

  The order of the numbers within the vector is irrelevant and is
  ignored. Function \code{calc.numcat} returns \code{numcat} from a data
  frame of free sort data in the format described in dataset
  \code{lawson}.

  \code{missing} is optional but, if specified, must include one integer
  value for each simulated participant. The number specifies the number
  of items not classified by that participant. For example, if you wish
  to simulate five participants, three of whom classified all the items,
  but two of whom missed 3 and 5 items respectively, then:

  missing <- c(0,0,0,3,5)

  The order of the numbers within the vector must correspond to the
  order of numbers in \code{numcat}. So, in the above examples, the
  three simulated participants who used nine groups classified all the
  items (zero missing items). The algorithm randomly selects which items
  are not classified for each participant.

  The main use of the \code{phi.intra.chance} function is in the Monte
  Carlo estimation of the chance level of the phi.intra statistic; an
  example of usage is given below.

  The phi.intra metric is based on Cramer (1946), slightly developed by
  Wills & McLaren (1998), and first employed in its current form by
  Haslam et al. (2007).

  The particular Monte Carlo method of estimating the chance level of
  phi intra was first reported by Haslam et al. (2007). The Appendix of
  Lawson et al. (2016) provides a brief tutorial.
  
}
\value{
  Phi.intra; a number ranging between 0 and 1.
}
\note{
  This calculation assumes that all participants classify all items,
  i.e. that \code{nitems} is correct for all participants. 
  }
\references{

  Cramer, H. (1946). \emph{Mathematical models of
  statistics}. Princeton, NJ: Princeton University Press.

  Haslam, C., Wills, A.J., Haslam, S.A., Kay, J., Baron, R. & McNab,
  F. (2007). Does maintenance of colour categories rely on language?
  Evidence to the contrary from a case of semantic dementia. \emph{Brain
  and Language, 103}, 251-263.
  
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
  \code{\link{phi.intra}}, \code{\link{calc.numcats}}, \code{\link{sig.con}}
}
\examples{

  ## Estimate phi.intra chance level for 'words' group of 'lawson'
  ## dataset, using 10 iterations.

  data(lawson)
  edta <- lawson[lawson$ExptGroup == 2,]
  x <- calc.numcats(edta)
  iterate <- 10
  mc <- sapply(1:iterate,phi.intra.chance,numcat=x,nitems=140)
  mean(mc)
  sig.con(0.7,mean(mc))

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
  ## mc <- sfSapply(1:iterate,phi.intra.chance,numcat=x,nitems=140)
  ## mean(mc)
  ## sfStop()

}