\name{rewie.rsq}
\alias{rewie.rsq}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Computes R-squared for RE panel models
  %%  ~~function to do ... ~~
}
\description{Calculates R-squared for BW, REWE, REWIE, and BW. Includes idiosyncratic R-squared, between R-squared, homogneous-within R-squared, and within R-Squared.
  %%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
  rewie.rsq(model,timevar,csvar,df)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{model}{ is an lmerMod object fitted by calling the lmer() function in lme4.
    %%     ~~Describe \code{x} here~~
  }
  \item{timevar}{ is a character string providing the name of the time indicator variable.}
  \item{csvar}{ is a character string providing the name of the cross-section indicator variable.}
  \item{df}{ is a dataframe containing y, timevar, and csvar}
}
\details{
  Calculates the R-squared for each level of variation. It is calculated by computing the proportion of remaining variance to overall variance the model and then subtracting the quotient from 1.
}
\value{
  Returns the results for R-squares
  \item{Rsq.total}{ is the overall R-squared.}
  \item{Rsq.within}{ is the within R-squared.}
  \item{Rsq.time}{ is the time (homogenous-within) R-squared.}
  \item{Rsq.idio}{ is the idiosyncratic R-squared.}
  \item{Rsq.betw}{ is the between R-squared.}


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
    require(lme4)

    data("Crime")
    output<-lmer(lcrmrte~ldensity+(1|county)+(1|year),data=Crime)
    rewie.rsq(output,csvar="county",timevar="year",df=Crime)

  }
  \donttest{
    require(plm)
    require(lme4)

    data("Crime")
    output<-lmer(lcrmrte~ldensity+(1|county)+(1|year),data=Crime)
    rewie.rsq(output,csvar="county",timevar="year",df=Crime)


  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~fixed effects }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~mixed models}% __ONLY ONE__ keyword per line
