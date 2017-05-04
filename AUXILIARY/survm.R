# Compute survival object

survm <- function (datTemp,yearR,ageR,iweight)
{#aR <- 18:39
 #yR <- 1975:2008
 
  nE.year <- length(yearR)
 nE.age <- length (ageR)
 # Observation starts in 1975 if age in 1975 between 18 and < 40 (born between 1936 and 1957)
  # IF born after 1957, start = year in which age 18 is reached
  ny18a <- datTemp$born + ageR[1] # +18 # year in which person gets 18 (Sabine used 0)
  # Respondent is alive and in Senegal at 18 (NB: For children that dies, no HHStatus info!! => excluded)
  ny18 <- ifelse ((!is.na(datTemp$death.yr) & datTemp$death.yr < ny18a) | 
                    (!is.na(datTemp$emigr.year) & datTemp$emigr.year < ny18a) ,NA, ny18a)
  # line 2534 emigrates before age 18 (born 1963, emigration 1976)
  YearStart <- ifelse (ny18 < yearR[1],yearR[1],ny18)
  YearStart <- ifelse (YearStart > yearR[nE.year],NA,YearStart)
  # YearStart = NA if person emigrated before age 18
 
  ny40a <- datTemp$born+ageR[nE.age]+1 # 39+1
  YearStopa <- ifelse (ny40a>yearR[nE.year],datTemp$int.yr,ny40a)  #  yearR[nE.year]
  #YearStopa <- ifelse (ny40>datTemp$int.yr,datTemp$int.yr,ny40)
  # If person died  before 40
  YearStopb <- ifelse ((!is.na(datTemp$death.yr) & datTemp$death.yr < YearStopa & datTemp$death.yr > YearStart),datTemp$death.yr,YearStopa)
  # If person left Senegal in observation window
  YearStop <- ifelse ((!is.na(datTemp$emigr.year) & (datTemp$emigr.year >= YearStart & 
                                                       datTemp$emigr.year <= YearStopb)),datTemp$emigr.year,YearStopb)
  YearStop <- ifelse (is.na(YearStart),NA,YearStop)
  YearStop <- ifelse ((!is.na(YearStart) & !is.na(YearStop)&YearStop<=YearStart),YearStart+0.5,YearStop) # start and stop in same year, => 1 year (for survival object)
  datTemp$YearStart <- YearStart
  datTemp$YearStop <- YearStop
  # Remove persons who migrated between 1975-2008 at age < 18 (total = 98)
  datTemp2 <- datTemp
  datTemp <-datTemp2[!is.na(datTemp2$YearStart),]  # 3285 subjects
  
 # table (datTemp$YearStart,useNA="always")
 
 # =========  Survival object    =================
 
 # datTemp = data.sd

# res <- getEventAndRiskSet_fw(data.sd, ageR, yearR,iweight)
 # datTemp <- res$datTemp

# -------  Select s+d who emigrated between 1975 and survey date ---
datTemp$emig.YG <- cut(datTemp$emigr.year,breaks=c(0,1975,2009,3000),include.lowest=TRUE,right=FALSE,labels=c("<1975","1975-08","2009+")) 
# --------  Age group at emigration -----------
datTemp$emig.AG <- cut(datTemp$emigr.age,breaks=c(0,18,40,200),include.lowest=TRUE,right=FALSE,labels=c("<18","18-39","40+"))   # 18-39
#  addmargins (table (datTemp$emig.AG,datTemp$emig.YG,useNA="always"))


# REDUNDANT
# emigr.year.risk: Year emigration if emigration in observation window
# emigr.age.risk: Age emigration if emigration in observation window
# emig.status.risk: Emigration status if emigration in observation window
datTemp$emigr.year.risk <- ifelse (datTemp$emig.YG=="1975-08" & datTemp$emig.AG=="18-39",datTemp$emigr.year,NA)
datTemp$emigr.age.risk <- datTemp$emigr.year.risk - datTemp$born
# END REDUNTANT
datTemp$emig.status.risk <- ifelse (!is.na(datTemp$emig.YG) & datTemp$emig.YG=="1975-08" & datTemp$emig.AG=="18-39",1,0)

require (survival)
one <- rep(1,nrow(datTemp))
 surv0 <- Surv(datTemp$YearStart-datTemp$born,datTemp$YearStop-datTemp$born,datTemp$emig.status.risk)
 nmigrations.u <- sum (surv0[,3])  # Unweighted
 nexposure.u <- sum (surv0[,2]-surv0[,1],na.rm=TRUE) # Unweighted
 rate.u <- nmigrations.u/nexposure.u
# if (iweight==1) nweight <- datTemp$weight.hhd else nweight <-  rep(1,nrow(datTemp))
 nweight <- datTemp$weight.hhd
 nmigrations.w <- sum (surv0[,3]*nweight)
 nexposure.w <- sum ((surv0[,2]-surv0[,1])*nweight,na.rm=TRUE)
 rate.w <- nmigrations.w / nexposure.w
 mrate <- data.frame(Case=c("unweighted","weighted"),
                     events=c(nmigrations.u,nmigrations.w),
                     exposure=c(nexposure.u,nexposure.w),
                     rate=c(rate.u,rate.w))
 return (list (mrate=mrate,surv=surv0,datTemp=datTemp))
}