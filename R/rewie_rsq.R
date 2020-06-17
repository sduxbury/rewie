

#Computes the R-squared at each level of variation by subtracting the variance component int the model from the variance in one-way anova


rewie.rsq <- function(model,timevar, csvar, df) {

  if(class(model)!="lmerMod"){
    stop("function only works for lmer models")
  }



  #get data
  df<-stats::model.frame(model)
  y<-colnames(df)[1]

  #get baseline variance components
  ICC_out<-ICC(y=y,timevar=timevar,csvar=csvar,df=df)

  #get variance components
  vcomp<-summary(model)$varcor
  ts.vcomp<-vcomp[[timevar]][[1]]
  residual.vcomp<-attr(vcomp,"sc")^2
  within_variance<-ts.vcomp+residual.vcomp


#get variance components
  cs.vcomp<-vcomp[[csvar]][1]
  total.variance<-cs.vcomp+ts.vcomp+residual.vcomp

  ICC<-ICC_out$ICC.mat

  Rsq.total<-1-(total.variance/ICC[1,2])
  Rsq.time<-1-(ts.vcomp/ICC[2,2])
  Rsq.idio<-1-(residual.vcomp/ICC[3,2])
  Rsq.betw<-1-(cs.vcomp/ICC[4,2])

  ICC<-ICC_out$within.ICC.mat

  Rsq.within<-1-(within_variance/ICC[1,2])

  rsq.total.list<-list(Rsq.total=Rsq.total,
                 Rsq.within=Rsq.within,
                 Rsq.time=Rsq.time,
                 Rsq.idio=Rsq.idio,
                 Rsq.betw=Rsq.betw)


  message("R-squared is ill defined for mixed models. The R-square here is calculated by dividing the variance component in the fitted model by the variance component in one-way anova and then subtracting the quotient from 1. If the R-square value is negative, it is because the sampling error in the estimation of variance components is greater than the improvement in variance explained in the null model. Interpret such negative values as an R-square of 0. ")
  return(rsq.total.list)



}
