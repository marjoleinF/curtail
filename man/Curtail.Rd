\name{Curtail}
\alias{Curtail}

\encoding{latin1}

\title{Curtailment}

\description{
  Simulates deterministic curtailment on an existing dataset of item scores, based on a
  user-specified cut-off value.
}

\usage{
  Curtail(dataset.test, Xstar, highest = NULL, lowest = NULL)
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
}
  
\details{

  Note that currently, only items with the same number of response categories, and the
  same ordering of the response categories, are supported.

  The code is still under development and might change in future versions.
}

\value{
  The function returns a list with the following objects:

  \item{item.scores}{Original dataset of item scores}
  \item{cumulative.scores}{Dataset of cumulative scores on every item}
  \item{current.item}{Vector with the number of the current item (last item administered 
  in curtailed administration) for each observation in the dataset}
  \item{current.score}{Vector with the current score (cumulative score for last item 
  administered) for each observation in the dataset}
  \item{curtailed.test.length.distribution}{List with descriptive statistics on 
  efficiency: mean, standard deviation and median number of items administed}
  \item{accuracy}{List with descriptive statistics on accuracy: number of 'at risk' and 
  'not at risk' observations flagged as 'at risk' and 'not at risk' after application 
  of curtailment}

}

%\references{
%Fokkema M, Smits N, Kelderman H (2015).
%  \dQuote{Detecting Treatment-Subgroup Interactions in Clustered Data
%  with Generalized Linear Mixed-Effects Model Trees}.
%  \url{http://EconPapers.RePEc.org/RePEc:inn:wpaper:2015-10}
%}

%\seealso{\code{\link[lme4]{lmer}}}

\examples{
  Curtail(itemscores, 19)
}

\keyword{curtail, curtailment}
