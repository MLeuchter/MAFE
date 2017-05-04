
evaluate_replicates <- function (replSet)
{
bootPC <- bootPCMale <- bootPCFem <- NULL
boot_rate_u <- boot_rate_w <-NULL
boot_prob <- NULL
boot_rate_w_trend_MF <- boot_rate_w_trend_M <- boot_rate_w_trend_F <- NULL
boot_rate_w_ASMR_MF <- boot_rate_w_ASMR_M <- boot_rate_w_ASMR_F <- NULL
for(r in 1:repN){
  cat("Replicate: ",r,"\n")
 # output: boot_rate_u, boot_rate_w, boot_rate_w_trend_MF, boot_rate_w_ASMR_MF,
 #          boot_prob
#  resK0 <- pcEstModel(replSet[[r]], ageR=aR, yearR=yR, considerGender=FALSE)
  migr_rates <- migr.rates(replSet[[r]])
  #  cases: unweighted, weighted
  # surv: (1) rates; (2) survival object, (3) data set datTemp
  
#  bootPC <- rbind(bootPC,resK0)
# Rates for 2 sex combined, males and females
  rates_u <- c(migr_rates$exponential$Combined$rate_u,
               migr_rates$exponential$Male$rate_u,
               migr_rates$exponential$Female$rate_u)
  names (rates_u) <- namcase
  rates_w <- c(migr_rates$exponential$Combined$rate_w,
               migr_rates$exponential$Male$rate_w,
               migr_rates$exponential$Female$rate_w)
  names (rates_w) <- namcase
  # Add results to those of previous replicates (rbind)
  boot_rate_u <- rbind (boot_rate_u,rates_u)
  boot_rate_w <- rbind (boot_rate_w,rates_w)
  
  # trend in one replicate: migr_rates$cases$Combined$trend
  # ASMR in one replicate: migr_rates$cases$Combined$ASMR
  # total emig rates, events, exposure (u and w):  migr_rates$surv$Combined$mrate 
  boot_rate_w_trend_MF <- rbind (boot_rate_w_trend_MF,migr_rates$cases$Combined$trend$intensity)
  boot_rate_w_trend_M <- rbind (boot_rate_w_trend_M,migr_rates$cases$Male$trend$intensity)
  boot_rate_w_trend_F <- rbind (boot_rate_w_trend_F,migr_rates$cases$Female$trend$intensity)
  boot_rate_w_ASMR_MF <- rbind (boot_rate_w_ASMR_MF,migr_rates$cases$Combined$ASMR$intensity)
  boot_rate_w_ASMR_M <- rbind (boot_rate_w_ASMR_M,migr_rates$cases$Male$ASMR$intensity)
  boot_rate_w_ASMR_F <- rbind (boot_rate_w_ASMR_F,migr_rates$cases$Female$ASMR$intensity)

# Probability that 18-year old emigrates before age 40
  rates_age <- cbind(migr_rates$cases$Combined$ASMR$intensity,
                migr_rates$cases$Male$ASMR$intensity,
                migr_rates$cases$Female$ASMR$intensity)
  pm <- 1-exp(-sum (rates_age[2:4,2]*c(7,5,10)))
  pf <- 1-exp(-sum (rates_age[2:4,3]*c(7,5,10)))
  pt <- 1-exp(-sum (rates_age[2:4,1]*c(7,5,10)))
  boot_prob <- rbind (boot_prob,c(pm,pf,pt))
}
return (list(boot_rate_u=boot_rate_u,
             boot_rate_w=boot_rate_w,
             boot_rate_w_trend_MF=boot_rate_w_trend_MF,
             boot_rate_w_trend_M=boot_rate_w_trend_M,
             boot_rate_w_trend_F=boot_rate_w_trend_F,
             boot_rate_w_ASMR_MF=boot_rate_w_ASMR_MF,
             boot_rate_w_ASMR_M=boot_rate_w_ASMR_M,
             boot_rate_w_ASMR_F=boot_rate_w_ASMR_F,
             boot_prob=boot_prob ))
}

