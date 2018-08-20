\name{Curtail}
\alias{Curtail}

\encoding{latin1}

\title{Simulate deterministic curtailment on item scores of a test}

\description{
  Simulates deterministic curtailment on an existing dataset of item scores, based on a
  user-specified cut-off value.
}

\usage{
  Curtail(dataset.test, Xstar, highest = NULL, lowest = NULL, plot = TRUE)
}

\arguments{
  \item{dataset.test}{A dataframe containing item scores only.}
  \item{Xstar}{Cut-off value to be used for classifying observations as 'at risk' 
  (test-score values greater than or equal to the cut-off value) or 'not at risk' 
  (test-score values less than cut-off value)}
  \item{highest}{Highest possible item score. If not provided, function will take the 
  max of all observed item scores.}
  \item{lowest}{Lowest possible item score. If not provided, function will take the min 
  of all observed item scores.}
  \item{plot}{Should a histogram of test lengths be plotted?}
}
  
\details{

  Note that currently, only items with the same number of response categories, and the
  same ordering of the response categories, are supported.

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
  tmp <- Curtail(itemscores, 19)
  tmp$curtailed.test.length.distribution
}}

\keyword{curtail, curtailment}
