

#This function preprocesses the data matrix to be used in random effects within estimation. The transformed variables in the output
  #dataframe is intended ot be passed to LMER or another mixed modeling package



rewie.dat <- function(df,vars.to.center, csvar, timevar=NULL,model="BWI") {

if(class(vars.to.center)!="character"){
 stop("Provide vars.to.center as a character string or vector of character strings")
}

if(class(df)!="data.frame"){
 stop("Data must be provided in data frame format.")
}

if(length(setdiff(vars.to.center,colnames(df)))!=0){
  stop("All variable names must appear exactly in dataframe. Check that all variable names are correct.")
}

if(model!="BWI" & model!="REWE" & model!="BW" & model!="REWIE"){
  stop("Model must be one of the following types: 'BWI', 'REWE', 'BW', or 'REWIE'")
}

if(model%in%c("BWI","REWIE") & is.null(timevar)){
  stop("timevar must be specified for idiosyncratic-within transformation. ")
}


  #within transform
within.df<-rockchalk::gmc(df,x=vars.to.center,by=csvar,suffix=c("_between","_within"))

if(model%in%c("BW","REWE")){
  return(within.df)
}else{

vars.for.second.center<-c(paste(vars.to.center,"_within",sep=""))
REWIE.df<-rockchalk::gmc(within.df,x=vars.for.second.center,by=timevar,suffix=c("_common","_idiosyncratic"))

return(REWIE.df)

}



}
