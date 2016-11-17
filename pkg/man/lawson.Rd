\name{lawson}
\alias{lawson}
\docType{data}
\title{
  Lawson et al. (2017) Freesort Dataset
}
\description{

 Data from Experiment 1 of Lawson et al. (2017), a free sort of a large
 set of everyday objects (140 items) by 30 adult participants. 
  
}
\usage{data("lawson")}
\format{
  A data frame with 4200 observations on the following 5 variables.
  \describe{
    \item{\code{ExptGroup}}{Experimental condition. 1 = pictures, 2 = words.}
    \item{\code{SjNo}}{Participant Number}
    \item{\code{CategoryNo}}{Category number}
    \item{\code{ItemNo}}{Stimulus number, range: 1-140}
    \item{\code{ItemName}}{Stimulus name}
  }
}
\details{
  
  Lawson et al. (2017, Exp. 1) asked 30 adult participants to sort 140
  everday objects into groups. Participants in the 'words' condition
  sorted cards with the words listed in \code{ItemName} printed on
  them. Particiants in 'pictures' condition sorted cards with color
  pictures depicting those words. The pictures are not reproduced here
  for copyright reasons.

  For each participant, \code{CategoryNo} ranges from 1 to the number of
  groups that participant created. Within a given participant, items
  with the same category number were put into the same group. The number
  itself is arbitrary (e.g. the number does not indicate which group was
  formed first, there is no planned correspodence between a particular
  category number across participants, etc).
  
}
\source{

  Lawson, R., Chang, F. & Wills, A.J. (2017). Free classification of
  large sets of everyday objects is more thematic than
  taxonomic. \emph{Acta Psychologica, 172}, 26-40.

}
\author{
  Andy J. Wills (andy@willslab.co.uk)
  }
\examples{
  data(lawson)
}
\keyword{datasets}
