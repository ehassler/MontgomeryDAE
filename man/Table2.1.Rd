\name{Table2.1}
\alias{Table2.1}
\docType{data}
\title{
Table 2.1: Tension Bond Strength Data for the Portland Cement Formulation Experiment
}
\description{
Cure time data for normal mortar versus mortar with an added polymer latex emulsion.
}
\usage{data("Table2.1")}
\format{
  A data frame with 10 observations on the following 2 variables.
  \describe{
    \item{\code{ModifiedMortar}}{a numeric vector}
    \item{\code{UnmodifiedMortar}}{a numeric vector}
  }
}
\source{
src%%  ~~ reference to a publication or URL from which the data were obtained ~~
}
\references{
Montgomery, D. C. (2017, 9th ed.) \emph{Design and Analysis of Experiments}, Wiley, New York.
}
\examples{
data(Table2.1)
boxplot(Table2.1)
summary(Table2.1)
# Two-sample t-test with pooled variance estimate (Output of Table 2.2)
t.test(Table2.1[,"ModifiedMortar"], Table2.1[,"UnmodifiedMortar"], paired=FALSE, var.equal=TRUE)
}
\keyword{datasets}
