\name{stochTable}
\alias{stochTable}

\encoding{latin1}

\title{Create a look-up table for stochastic curtailment}

\description{
  Creates a look-up table for early stopping of item administration, for stochastic
  curtailment.
}

\usage{
  stochTable(dataset.train, Xstar, gamma0 = 0.95, gamma1 = 0.95)
}

\arguments{
  \item{dataset.train}{Dataframe, containing item scores only}
  \item{Xstar}{cut-off value for the final test score to be used}
  \item{gamma0}{The threshold for the probability, calculated using the 'not-at-risk'
  training observations,  that the classification decision based on the stochastically
  curtailed version will match that of the full-length instrument.}
  \item{gamma1}{The threshold for the probability, calculated using the 'at-risk'
  training observations,  that the classification decision based on the stochastically
  curtailed version will match that of the full-length instrument.}
}
  
\details{
  The code is still under development and might change in future versions.
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

\examples{\donttest{
  stochTable(itemscores, 19)
  # try lower gamma values for earlier stopping:
  stochTable(itemscores, 19, gamma0 = .75, gamma1 = .75)
}}

\keyword{curtail, curtailment}