# Migration rates
migr.rates <- function (datTemp)
{
# ===================================================================
# ===   Produce survival object with events and exposures         ===
# ===    in period 1975-2008 and ages 18-39                       ===
# ===     Computes also emigration rate                           ===
# ===================================================================
yearR <- 1975:2008
ageR <- 18:39
require (eha)
require (survival)
n_s <- 3
namcase <- c("Combined","Male","Female")
surv_s <- surv_T <- vector(mode="list",length= n_s)
names(surv_s)<- names (surv_T) <-  namcase
for(iter in 1:n_s){
  # Select one variant
  if (iter==1) datTemp_s <- datTemp
  if (iter==2) datTemp_s <- datTemp[datTemp$sex=="Male",]
  if (iter==3) datTemp_s <- datTemp[datTemp$sex=="Female",]
  # ===================================================================
  # ===         Emigration rates by sex and age                     ===
  # ===================================================================
  surv_s[[iter]] <- survm(datTemp_s,yearR,ageR,iweight) 
  surv_T[[iter]] <- Surv (surv_s[[iter]]$datTemp$YearStart,
                          surv_s[[iter]]$datTemp$YearStop,
                          surv_s[[iter]]$datTemp$emig.status.risk)
}
# surv_s[[1]]$mrate

cases_s <- ev_u <- vector("list", n_s)
names(cases_s)<-  names(ev_u) <- namcase
for(iter in 1:n_s){
  # table.events is function of eha package
  ev_u[[iter]] <- table.events (enter=surv_s[[iter]]$surv[,1],
                                exit=surv_s[[iter]]$surv[,2],
                                event=surv_s[[iter]]$surv[,3])
  iweight=1
  if (iweight==1) nweight <- surv_s[[iter]]$datTemp$weight.hhd else 
    nweight <-  rep(1,nrow(surv_s[[iter]]$datTemp))
  ASMR <-piecewise_w (survv=surv_s[[iter]]$surv,cutpoints=c(18,25,30,40),nweight) 
  # ===================================================================
  # =============   Trend in emigration rate    =======================
  # ===================================================================
  Trend <-piecewise_w (survv=surv_T[[iter]],cutpoints=c(1975,1985,1995,2009),nweight)
  
  cases_s[[iter]] <-list (nam=namcase[iter],ASMR=ASMR,trend=Trend)
  # Each list element contains: events, expsosures, intensity (=rate)
}
sum (cases_s[[1]]$trend$events)


# ===================================================================
# ====                Estimate emigration rate           ============
# ====                  from survival object             ============
# ====     with exponential survival regression model    ============
# ===================================================================
exponential <- vector ("list",n_s)
names (exponential) <- namcase
for (iter in 1:n_s)
{ 
  #  no weights
  one <- rep(1,nrow(surv_s[[iter]]$datTemp))
  time <- surv_s[[iter]]$surv[,2]-surv_s[[iter]]$surv[,1]
  surv_time_u <- Surv(time=time,surv_s[[iter]]$surv[,3]) 
  z.u <- survreg (surv_time_u~one,data=surv_s[[iter]]$datTemp,dist="exponential")
  # variance is variance linked to estimated coefficient[1]
  rate_u <- exp(-z.u$coefficients[1])    # Emig rate males+females
  # cases_s[[iter]]$surv$mrat
  estim <- exp(-z.u$coefficients[1]) 
  a <- exp(-z.u$coefficients[1]-1.96*sqrt(z.u$var[1])) 
  b <- exp(-z.u$coefficients[1]+1.96*sqrt(z.u$var[1])) 
  int95_u <- c(a,estim,b)
  
  # Weights
  #  cases_s[[iter]]$surv$datTemp$weight.hhd *
  surv_time_w <- Surv(time= time,surv_s[[iter]]$surv[,3])
  nweight <- surv_s[[iter]]$datTemp$weight.hhd
  z.w <- survreg (surv_time_w~one,dist="exponential",weights=nweight)
  rate_w <- exp(-z.w$coefficients[1])    # emig rate 
  
  # 95% intervals around emigration rate, assuming normal distribution 
  estim <- exp(-z.w$coefficients[1]) 
  a <- exp(-z.w$coefficients[1]-1.96*sqrt(z.w$var[1])) 
  b <- exp(-z.w$coefficients[1]+1.96*sqrt(z.w$var[1])) 
  int95_w <- c(a,estim,b)
  exponential[[iter]]<- list(nam=namcase[iter],
                             rate_u=rate_u,
                             rate_w=rate_w,
                             int95_u=int95_u,
                             int95_w=int95_w )
}
return (list (surv=surv_s,cases=cases_s,exponential=exponential))
}