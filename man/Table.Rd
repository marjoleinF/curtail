\name{Table}
\alias{Table}

\encoding{latin1}

\title{Create a look-up table for deterministic curtailment}

\description{
  Creates a look-up table for early stopping of item administration, for deterministic
  curtailment.
}

\usage{
  Table(dataset.train = NULL, Xstar, nitems = NULL, highest = NULL, lowest  = NULL)
}

\arguments{
  \item{dataset.train}{Optional, a dataframe, containing item scores only}
  \item{Xstar}{cut-off value for the final test score to be used}
  \item{nitems}{number of items in questionnaire (assumed equal for all items)}
  \item{highest}{Optional, highest theoretically possible item score (assumed equal for 
  all items)}
  \item{lowest}{Optional, lowest theoretically possible item score (assumed equal for
  all items)}
}
  
\details{
  Note that either dataset.train needs to be specified, or nitems, highest and lowest. 
  
  The code is still under development and may change in future versions.
}

\value{
  Returns a look-up table with cut-off values for every item, to be used for early 
  stopping of item administration.     
}

\references{
  Fokkema, M., Smits, N., Finkelman, M. D., Kelderman, H., & Cuijpers, P. (2014). 
  Curtailment: A method to reduce the length of self-report questionnaires while 
  maintaining diagnostic accuracy. Psychiatry Research 215, 477-482. \cr \cr
  Fokkema, M., Smits, N., Kelderman, H., Carlier, I.V. & Van Hemert, A.M. (2014).
  Combining decision trees and stochastic curtailment for assessment length 
  reduction of test batteries used for classification. Applied Psychological 
  Measurement, 38(1), 3-17.\cr \cr
  Finkelman, M.D., Smits, N., Kim, W. & Riley, B. (2012). Curtailment and stochastic 
  curtailment to shorten the CES-D. Applied Psychological Measurement, 36(8), 632-658.\cr \cr
}

\examples{
  Table(itemscores, 19)
  Table(Xstar = 19, nitems = 20, highest = 3, lowest = 0)
}

\keyword{curtail, curtailment}