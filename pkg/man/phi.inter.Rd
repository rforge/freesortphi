\name{phi.inter}
\alias{phi.inter}
\title{
  Inter-group Phi
}
\description{

  Calculates inter-group phi between two groups of participants, each of
  whom has free sorted the same set of items. Inter-group phi is a
  measure of agreement between two groups of free sorters.

}
\usage{
phi.inter(grp1, grp2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  
  \item{grp1}{A data frame, for the first group of participants, that
    includes the following columns: \code{SjNo, CategoryNo, ItemNo},
    described below. Each row is one classification decision.}

  \item{grp2}{A data frame, for the second group of participants; format
    as for grp1.}

}

\details{

  Dataset \code{lawson} provides an example of the format expected by
  the phi.inter function. Only the following columns are used, other
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

  Cramer's Phi (Cramer, 1946) is calculated for each pair of cross-group
  participants, and the mean of these values is returned. For example if
  grp1 contains participants 1-2, and grp2 contains participants 3-4,
  then Cramer's Phi is calculated for participant pairs [1,3], [1,4],
  [2,3], [2,4].

  Phi-inter ranges between 0 and 1; 1 indicating perfect consistency
  among groups (within the definition of consistency employed; see
  Lawson et al., 2017, appendix for details). For an inferential test of
  whether the sorts produced by the two groups differ significantly, see 
  \code{phi.inter.rnd}.

  The phi-inter metric is based on Cramer (1946), slightly developed by
  Wills & McLaren (1998), and first employed in its current form by
  Lawson et al. (2017). The Appendix of Lawson et al. (2017) provides a
  brief tutorial.

}
\value{
  Phi-inter; a number ranging between 0 and 1.
}
\references{

  Cramer, H. (1946). \emph{Mathematical models of
  statistics}. Princeton, NJ: Princeton University Press.

  Lawson, R., Chang, F. & Wills, A.J. (2017). Free classification of
  large sets of everyday objects is more thematic than
  taxonomic. \emph{Manuscript accepted by Acta Psychologica}.

  Wills, A.J. & McLaren, I.P.L. (1998). Perceptual learning and free
  classification. \emph{Quarterly Journal of Experimental Psychology,
  51B}, 235-270.

}
\author{
  Andy J. Wills (andy@willslab.co.uk)
}
\seealso{
  \code{\link{phi.intra}}, \code{\link{phi.inter.rnd}}
}
\examples{
  data(lawson)
  grp1 <- lawson[lawson$ExptGroup == 1,]
  grp2 <- lawson[lawson$ExptGroup == 2,]
  phi.inter(grp1,grp2)
}
