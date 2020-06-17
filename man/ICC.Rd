\name{ICC}
\alias{ICC}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Computes intraclass correlation coefficient (ICC)
%%  ~~function to do ... ~~
}
\description{Calculates the ICC for the idiosyncratic, within, and between variation. Calculates the ICC for total variation and for within variation.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
ICC(y,timevar,csvar,df)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{y}{ is the name of the response vector provided as a character string.
%%     ~~Describe \code{x} here~~
}
  \item{timevar}{ is a character string providing the name of the time indicator variable.}
  \item{csvar}{ is a character string providing the name of the cross-section indicator variable.}
  \item{df}{ is a dataframe containing y, timevar, and csvar}

}
\details{
Calculates ICC foreach level of variation.
}
\value{
Returns the ICC results in a list.
  \item{ICC.mat}{ is the matrix of ICCs and variance components for each level of variation.}
  \item{within.ICC.mat}{ is the matrix of ICCs and variance components only examining the within variation.}

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


\examples{
\dontshow{
require(plm)

data("Crime")
ICC("lcrmrte",csvar="county", timevar="year",Crime)



}
\donttest{
library(plm)
library(lme4)


##not run
data("Crime")


ICC("lcrmrte",csvar="county", timevar="year",Crime)


}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~fixed effects }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~mixed models}% __ONLY ONE__ keyword per line
