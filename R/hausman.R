

#Hausman test for the comparison of pooled esitmator to within estimator,
  #and the within estimator to the idiosyncratic estimator
  #Do not specify random intercepts in the formula, rather supply them as timevar or csvar



hausman <- function(formula,timevar, csvar,df) {

  form<-stats::as.formula(formula)
  vars.to.center<-c(all.vars(form[[2]]),all.vars(form[[3]])) #extract IV names
#transform data
within.df<-rewie.dat(df,vars.to.center,timevar=timevar,csvar=csvar,model="BW")



##estimate models
pooled<-plm::plm(formula,model="random",index=csvar,data=df)
WE<-plm::plm(formula,model="within",index=csvar,data=df)
pooled.ht<-plm::phtest(pooled,WE,method="aux")

#transform to within cetner here
center.vars<-paste(all.vars(form[[3]]),"_within",sep="")

for(i in 1:length(center.vars)){

  if(i == 1){
    within_form<-paste(form[[2]],"_within","~",sep="")
    within_form<-paste(within_form,center.vars[i],sep="")
  }else{

  within_form<-paste(within_form,"+",center.vars[i])
  }
}

within_form<-stats::as.formula(within_form)


#REWE
REWE<-plm::plm(within_form,model="random",index=timevar,data=within.df)
TFE<-plm::plm(within_form,model="within",index=timevar,data=within.df)
within.ht<-plm::phtest(REWE,TFE,method="aux")


hausman_list<-list(pooled_vs_FE=pooled.ht,
                   REWE_vs_2FE=within.ht)

return(hausman_list)

}
