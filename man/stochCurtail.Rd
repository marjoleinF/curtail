\name{stochCurtail}
\alias{stochCurtail}

\encoding{latin1}

\title{Simulates stochastic curtailment on item scores of a test}

\description{
  Simulates stochastic curtailment on an existing test dataset of item scores, based on an
  existing training dataset of item scores, and a user-specified cut-off value.
}

\usage{
  stochCurtail(dataset.train, dataset.test = NULL, Xstar, gamma0=.95, 
  gamma1=.95, plot = TRUE)
}

\arguments{
  \item{dataset.train}{A dataframe, containing item scores only, which will be used to
  derive the probabilities of obtaining a final test score greater than, or equal to,
  the cut-off value, based on the current cumulative score}
  \item{dataset.test}{A dataframe containing item scores only. Curtailment will be
  simulated on these observations. When no test dataset is specified, curtailment will
  be simulated on the training dataset.}
  \item{Xstar}{Cut-off value to be used for classifying observations as 'at risk' 
  (test-score values greater than or equal to the cut-off value) or 'not at risk' 
  (test-score values less than cut-off value)}
  \item{gamma0}{The threshold for the probability, calculated using the 'not-at-risk'
  training observations,  that the classification decision based on the stochastically
  curtailed version will match that of the full-length instrument.}
  \item{gamma1}{The threshold for the probability, calculated using the 'at-risk'
  training observations,  that the classification decision based on the stochastically
  curtailed version will match that of the full-length instrument.}
  \item{plot}{Should a histogram of test lengths be plotted?}
}
  
\details{
  The code is still under development and might change in future versions.
}

\value{
  The function returns a list with the following objects:

  \item{item.scores}{Original test dataset of item scores}
  \item{cumulative.scores}{Cumulative scores on every item for the test dataset}
  \item{current.item}{Vector with the number of the current item (last item administered 
  in curtailed administration) for each observation in the test dataset}
  \item{current.score}{Vector with the current score (cumulative score for last item 
  administered) for each observation in the test dataset}
  \item{curtailed.test.length.distribution}{List with descriptive statistics on 
  efficiency of the stochastically curtailed test in the test dataset: mean, standard 
  deviation and median number of items administed}
  \item{accuracy}{List with descriptive statistics on accuracy: number of 'at risk' and 
  'not at risk' observations flagged as 'at risk' and 'not at risk' after application 
  of stochastic curtailment in the test dataset}

}


\examples{\donttest{
  ## obtain a test and training dataset
  set.seed(32061983)
  samp <- sample(1:1000, 500); train <- samp[1:500]
  trainingdata <- itemscores[train,]
  testdata <- itemscores[-train,]
  stochCurtail(trainingdata, testdata, 19)
  ## try lower gamma values for earlier stopping, but lower accuracy:
  stochCurtail(trainingdata, testdata, 19, gamma0=.75, gamma1=.75)
}}

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

\keyword{curtail, curtailment}