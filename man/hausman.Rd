\name{hausman}
\alias{hausman}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Conducts auxiliary Hausman tests
%%  ~~function to do ... ~~
}
\description{Tests correlations between level 1 and level 2 errors in the pooled random effects estimator and REWE. If the pooled test is significant, the pooled estimator is biased and the within estimator should be considered. If the within test is significant, then the within estimator is biased and REWIE, BWI, or two-way fixed effects should be consider.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
hausman(formula,timevar,csvar,df)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{formula}{ is a character string or formula object denoting the models to be tested, i.e., "y ~ x1+x2". NOTE:do not provide a lmer formula object (ie."y~x+(1|x").
%%     ~~Describe \code{x} here~~
}
  \item{timevar}{ is a character string providing the name of the time indicator variable.}
  \item{csvar}{ is a character string providing the name of the cross-section indicator variable.}
  \item{df}{ is the dataframe containing the data. NOTE: do not provide preprocessed variables to hausman(). All variable transformations used to conduct the tests are done natively within the function.}
}
\details{
Conducts auxiliary Hausman test of homogeneity in panel data. If the pooled test is failed, it means that the between variation adn within variation are correlated and so the pooled estimator is biased. The within test tests for correlation between the idiosyncratic and homogenous-within variation. If the test is filed, time heterogeneity exists and the one-way within estimator is biased. If both tests are failed, BWI, REWIE, and two-way fixed effects are unbiased estimators. If only the pooled test is failed, then the one-way within estimator (one-way fixed effects and between within) are unbiased. If neither test is failed, the pooled model is appropriate.
}
\value{
Returns the results from the two Hausman tests (called from the plm package using phtest) in a list.
  \item{pooled_vs_FE}{ is the result from the pooled Hausman test}
  \item{REWE_vs_2FE}{ is the result from the within Hausman test}

%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}

\author{
Scott Duxbury, Assistant Professor of Sociology at University of North Carolina, Chapel Hill
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link[plm]{phtest}}
}
\examples{
\dontshow{
require(plm)

data("Crime")
hausman("lcrmrte~ldensity+polpc",csvar="county", timevar="year",Crime)



}
\donttest{
library(plm)
library(lme4)


##not run
data("Crime")


hausman("lcrmrte~ldensity+polpc",csvar="county", timevar="year",Crime)


}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~fixed effects }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~mixed models}% __ONLY ONE__ keyword per line
