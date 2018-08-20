\name{stochCurtailXval}
\alias{stochCurtailXval}

\encoding{latin1}

\title{Simulate stochastic curtailment using leave-one-out CV}

\description{
  Simulates stochastic curtailment on an existing dataset of item scores, using 
  leave-one-out (LOO) cross validation, and a user-specified cut-off value. For a given
  observation, all other observations will be used to derive the probabilities of
  obtaining a final test score greater than or equal to the cut-off value.
}

\usage{
  stochCurtailXval(dataset, Xstar, gamma0 = .95, gamma1 = .95, plot = TRUE, 
  verbose = FALSE)
}

\arguments{
  \item{dataset}{A dataframe, containing item scores only.}
  \item{Xstar}{Cut-off value to be used for classifying observations as 'at risk' 
  (test-score values greater than or equal to the cut-off value) or 'not at risk' 
  (test-score values less than cut-off value)}
  \item{gamma0}{The threshold for the probability, calculated using the 'not-at-risk'
  training observations,  that the classification decision based on the stochastically
  curtailed version will match that of the full-length instrument.}
  \item{gamma1}{The threshold for the probability, calculated using the 'at-risk'
  training observations,  that the classification decision based on the stochastically
  curtailed version will match that of the full-length instrument.}
  \item{plot}{Should test length distribution be plotted?}
  \item{verbose}{Should progress be plotted to the command line?}
}
  
\details{
  The code is still under development and might change in future versions.
}

\value{
  The function prints accuracy estimates to the command line, and plots the 
  curtailed test length distribution. In addition, the function invisibly returns 
  a list with the following elements:

  \item{test.results}{data.frame with columns full.lenght.decision (classification 
  decsion according to full-length test); curtailed.decision (classification decision
  according to curtailed test administration); current.item (item at which testing
  was halted); current.score (cumulative testscore at item at which testing was halted).}
  \item{curtailed.test.length.distribution}{Descriptive statistics of number of items
  administered and number of tests curtailed.}
  \item{accuracy}{Confusion matrix of full-length and curtailed test classification 
  decisions.}
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
    ## Only first 100 observations are specified here, to reduce computation time: 
    tmp <- stochCurtailXval(itemscores[1:100,], 19, gamma0=.95, gamma1=.95) 
    tmp$curtailed.test.length.distribution
  }
}

\keyword{curtail, curtailment}
