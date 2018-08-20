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
  The function prints a confusion matrices with classification accuracy to the command line, 
  and plots the test length distribution. Invisibly, the function invisibly returns an 
  augmented dataset with the original item scores and: 
  \item{SCrisk}{whether the observation is flagged as 'at risk' in curtailed test 
  administration}
  \item{SCnorisk}{whether the observation is flagged as 'not at risk' in curtailed test 
  administation}
  \item{currit}{the number of the current item (last item administered in curtailed
  administration) for each observation in the dataset}
  \item{currts}{the current test score (cumulative score for last item administered) for
  each observation in the dataset}
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
    tmp <- stochCurtailXval(itemscores, 19, gamma0=.95, gamma1=.95) 
    ## Recreate confusion matrix manually:
    table(tmp$SCrisk, rowSums(itemscores) >= 19)
  }
}

\keyword{curtail, curtailment}
