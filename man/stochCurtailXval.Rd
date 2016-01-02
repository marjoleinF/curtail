\name{stochCurtailXval}
\alias{stochCurtailXval}

\encoding{latin1}

\title{Stochastic Curtailment based on k-fold cross validation}

\description{
  Simulates stochastic curtailment on an existing dataset of item scores, using 
  leave-one-out (LOO) cross validation, and a user-specified cut-off value. For a given
  observation, all other observations will be used to derive the probabilities of
  obtaining a final test score greater than or equal to the cut-off value.
}

\usage{
  stochCurtailXval(dataset, Xstar, gamma0=.95, gamma1=.95)
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
}
  
\details{
  The code is still under development and might change in future versions.
}

\value{
  The function returns an augmented dataset with the following variables: 
  
  Variables for all but the last item (therefore, variable names consist of
  the following characters, appended with the item number):
  \item{item}{the original item scores}
  \item{test}{the cumalative sum score for the items up to this number}
  \item{Pkplus}{see Finkelman et al. (2012)}
  \item{Pkmin}{see Finkelman et al. (2012)}

  Further, the following variables are included:
  \item{plusflag}{for internal calculations}
  \item{minflag}{for internal calculations}
  \item{SCrisk}{whether the observation is flagged as 'at risk' in curtailed test 
  administration}
  \item{SCnorisk}{whether the observation is flagged as 'not at risk' in curtailed test 
  administation}
  \item{currit}{the number of the current item (last item administered in curtailed
  administration) for each observation in the dataset}
  \item{currts}{the current test score (cumulative score for last item administered) for
  each observation in the dataset}
}

%\references{
%Fokkema M, Smits N, Kelderman H (2015).
%  \dQuote{Detecting Treatment-Subgroup Interactions in Clustered Data
%  with Generalized Linear Mixed-Effects Model Trees}.
%  \url{http://EconPapers.RePEc.org/RePEc:inn:wpaper:2015-10}
%}

%\seealso{\code{\link[lme4]{lmer}}}

\examples{
  \dontrun{
    tmp <- stochCurtailXval(itemscores, 19, gamma0=.95, gamma1=.95) 
    table(tmp$test20>=19, tmp$SCrisk) # accuray
    table(tmp$test20>=19, tmp$SCnorisk) # accuracy
    table(tmp$currit); mean(tmp$currit); sd(tmp$currit) # efficiency
  }
}

\keyword{curtail, curtailment}
