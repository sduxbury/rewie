

#Computes intraclass correlations for each level of variation. Returns the ICC for total variation (ICC.mat) and for within variation (within.ICC.mat)


ICC <- function(y,timevar, csvar, df) {

  if(is.factor(df[,y])){
    stop("ICC only defined for continuous variables.")
  }

  dep.y<-df[,y]
  time<-df[,timevar]
  cs<-df[,csvar]

  one.way.anova<-lme4::lmer(dep.y~1+(1|time)+(1|cs))
  vcomp<-summary(one.way.anova)$varcor
  cs.vcomp<-vcomp[["cs"]][1]
  ts.vcomp<-vcomp[["time"]][[1]]
  residual.vcomp<-attr(vcomp,"sc")^2
  total.variance<-cs.vcomp+ts.vcomp+residual.vcomp

  ##ICC for time, between, and observations
  ICC_between<-cs.vcomp/total.variance
  ICC_time<-ts.vcomp/total.variance
  ICC_idiosyncratic<-residual.vcomp/total.variance

  #now to get idiosyncratic and common within ICC
  within_y<-rewie.dat(df, y,csvar=csvar,model="REWE")
  dv<-paste(y,"_within",sep="")
  dv<-within_y[,dv]
  within.anova<-lme4::lmer(dv~1+(1|time))

  withinv<-summary(within.anova)$varcor
  withinvcomp<-withinv[["time"]][[1]]
  residual.within.vcomp<-attr(withinv,"sc")^2
  within_variance<-withinvcomp+residual.within.vcomp

  #####within ICCs
  ICC_within_idio<-residual.within.vcomp/within_variance
  ICC_within_common<-withinvcomp/within_variance


  ##package  results

  ICC.mat<-matrix(0,nrow=4,ncol=2)
  colnames(ICC.mat)<-c("ICC","variance component")
  rownames(ICC.mat)<-c("total","temporal","idiosyncratic","between")

  ICC.mat[1,1]<-NA
  ICC.mat[1,2]<-total.variance

  ICC.mat[2,1]<-ICC_time
  ICC.mat[2,2]<-ts.vcomp

  ICC.mat[3,1]<-ICC_idiosyncratic
  ICC.mat[3,2]<-residual.vcomp

  ICC.mat[4,1]<-ICC_between
  ICC.mat[4,2]<-cs.vcomp

  #iwthin
  ICC.within.mat<-matrix(0,nrow=3,ncol=2)
  colnames(ICC.within.mat)<-c("ICC_within","within variance component")
  rownames(ICC.within.mat)<-c("total","temporal","idiosyncratic")

  ICC.within.mat[1,1]<-NA
  ICC.within.mat[1,2]<-within_variance

  ICC.within.mat[2,1]<-ICC_within_common
  ICC.within.mat[2,2]<-withinvcomp

  ICC.within.mat[3,1]<-ICC_within_idio
  ICC.within.mat[3,2]<-residual.within.vcomp

  ICC.list<-list(ICC.mat=ICC.mat,
                 within.ICC.mat=ICC.within.mat)

return(ICC.list)
}
