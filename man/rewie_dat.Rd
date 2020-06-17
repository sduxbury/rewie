\name{rewie.dat}
\alias{rewie.dat}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Function to transform data for mixed modeling with REWE, REWIE, BW, and BWI.
%%  ~~function to do ... ~~
}
\description{Centers data for the specified variables. For REWE and BW, data are centered to provide cross-section means and within variation. For REWIE and BWI, data are centered to provide cross-section means, time means for the within variation, and the idiosyncratic variation centered on both cross-section and time means.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
rewie.dat(df,vars.to.center,csvar,timevar=NULL,model="BWI")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{df}{ df is the data frame containing the variables to transform.
%%     ~~Describe \code{x} here~~
}
  \item{vars.to.center}{ The variables to be transformed.}
  \item{timevar}{ is a character string providing the name of the time indicator variable.}
  \item{csvar}{ is a character string providing the name of the cross-section indicator variable.}
  \item{model}{ specifies the type of transformation. Can be one of the following: "BWI","REWE","REWIE", or"BW". For REWE and BW, the within variation and cross-section means are returned. For REWIE and BWI, the cross-section means, idiosyncratic variation, and time means of the within variation are returned.}
}
\details{
Transforms panel data to conduct panel analysis with random effects within estimators and its extensions. The variables are centered on their time and cross-section means to be passed to lmer() for mixed modeling. The time means are the means of the differences between the cross-section means and the observations. The idiosyncratic variation is the observation twice centered (minus the cross-section mean, then minus the time mean). When passed to a mixed model, the idiosyncratic variation returns the same estimate as two-way fixed effects, the between variation gives the between estimator, and the time mean gives the period effect of the variable. Other time invariant and cross-section invariant variables can also be included, but these models do not need to be transformed.
}
\value{
Returns a dataframe including the transformed values to be used in mixed modeling.


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
\code{\link[btergm]{edgeprob}}
}
\examples{
\dontshow{
require(plm)

data("Crime")
rewie.dat(Crime,c("lcrmrte","ldensity","polpc","lpctymle"),csvar="county",model="BW")



}
\donttest{
library(plm)
library(lme4)


##not run
data("Crime")


#####Ceate data for REWE and BW
crime.data<-rewie.dat(Crime,c("lcrmrte","ldensity"),csvar="county",model="BW")

#random effects within estimator. Equivalent to one-way fixed effects with a random intercept
REWE<-lmer(lcrmrte_within~ldensity_within+(1|year),data=crime.data)

#Between within model w/o time intercept
BW<-lmer(lcrmrte~ldensity_within+ldensity_between+(1|county),data=crime.data)

#cross-classified between within model including time intercept
ccBW<-lmer(lcrmrte~ldensity_within+ldensity_between+(1|county)+(1|year),data=crime.data)



###Create data for REWIE and BWI
crime.data<-rewie.dat(Crime,c("lcrmrte","ldensity"),csvar="county",timevar="year",model="BWI")

#Random effects within-idiosyncratic estimator. Idiosyncratic estimator is the
  #two-way fixed effects estimator.
  #_common variables yield the effect of common time trends
    #(period effects/homogenous within effect) on the outcome
REWIE<-lmer(lcrmrte_within~ldensity_within_idiosyncratic+
            ldensity_within_common+
            (1|year),data=crime.data)

#Between-within-idiosyncratic model. Idiosyncratic estimator and common estimator are
  #the same as REWIE, but also includes between effects
BWI<-lmer(lcrmrte~ldensity_within_idiosyncratic+
          ldensity_within_common+
          ldensity_between+
          (1|year)+(1|county),data=crime.data)

}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~fixed effects }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~mixed models}% __ONLY ONE__ keyword per line
