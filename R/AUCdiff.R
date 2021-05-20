#' AUCdiff
#'
#' Compute difference between areas under the curves for model and observation
#' @param  m  model estimates
#' @param  o  observations

#' @return AUCdiff
 

AUCdiff = function(m,o) {
  
  xvals=seq(from=1,to=length(m),by=1) #length of time series
  
  auco=trapz(xvals,o) #calculate area under the curve (auc) for observations
  
  aucm=trapz(xvals,m) #calculate auc for model
  
  
  AUCdiff=abs(auco-aucm) #subtract the model auc from the observations auc
  
  return(AUCdiff)
}

