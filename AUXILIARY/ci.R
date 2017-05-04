# Compute confidence intervals for emigration rates
ci <- function (bdata,expected,alpha)
{ namcase <- names(expected)
  low.quantiles <- apply(bdata,2,quantile,probs=alpha/2,na.rm=TRUE)
  high.quantiles <- apply(bdata,2,quantile,probs=1-alpha/2,na.rm=TRUE)
  ci.low <- 2*expected-high.quantiles
  ci.up <- 2*expected-low.quantiles
  #mean <- apply(bdata,2,mean,na.rm=TRUE)
  #median <- apply(bdata,2,median,na.rm=TRUE)
  prates <- data.frame (low=ci.low,estimate=expected,up=ci.up)
  rownames (prates) <- namcase 
  return(prates)
}