\name{phi.intra}
\alias{phi.intra}
\title{
  Intra-group Phi
}
\description{

  Calculates intra-group phi for a group of participants, each of whom
  has free sorted the same set of items. Intra-group phi is a measure of
  agreement between free sorters within a group. 

}
\usage{
phi.intra(edta)
}

\arguments{
  \item{edta}{A data frame that includes the following columns:
    \code{SjNo, CategoryNo, ItemNo}, described below. Each row is one
  classification decision.}
}

\details{

  Dataset \code{lawson} provides an example of the format expected by
  the phi.intra function. Only the following columns are used, other
  columns will be ignored:

  \code{SjNo} - integer; must be different for each participant.

  \code{CategoryNo} - integer. For each participant, it should range from
  1 to the number of groups that participant produced. Each group that
  participant produced must have a unique number. Other than that, the
  numbers are arbitrary.

  \code{ItemNo} - integer; ranges from 1 to the number of items in the
  stimulus set. Item numbering must be consistent across participants,
  e.g. if ItemNo 21 is 'duck' for participant 201, it must also be
  'duck' for all other participants in the data frame.

  Each row of the data frame is one classification decision. For
  example, one row might indicate that subject 204 placed item 21 into
  category 3. The list must be complete for every participant. For
  example, if the stimulus set contains 20 items then there must be
  exactly 20 entries for each participant. If a participant fails to
  classify an item, include the item number in a row but set the
  CategoryNo to 'NA'.

  Cramer's Phi (Cramer, 1946) is calculated for each pair of
  participants, and the mean of these values is returned. Phi-intra
  ranges between 0 and 1; 1 indicating perfect consistency among sorters
  (within the definition of consistency employed; see Lawson et al.,
  2016, appendix for details). The level of agreement expected among
  randomly responding classifiers can be calculated using
  \code{phi.intra.chance}.

  The phi-intra metric is based on Cramer (1946), slightly developed by
  Wills & McLaren (1998), and first employed in its current form by
  Haslam et al. (2007). The Appendix of Lawson et al. (2016) provides a
  brief tutorial.

}
\value{
  Phi-intra; a number ranging between 0 and 1.
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
  \code{\link{phi.inter}}, \code{\link{phi.intra.chance}}

}
\examples{
  data(lawson)
  edta <- lawson[lawson$ExptGroup == 2,]
  phi.intra(edta)
}