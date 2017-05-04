# Create data file for sons and daughter
sons_daughters <- function (d.merge)
{
# === A. Compute first departure and first return: year and age  ====
# First emigration: year and age
a13a.an <- ifelse (d.merge$a13a.an>0, d.merge$a13a.an,NA)
a13a.age <- ifelse (d.merge$a13a.age>0, d.merge$a13a.age,NA)
# First return: year and age
a13d.an <- ifelse (d.merge$a13d.an>0, d.merge$a13d.an,NA)
a13d.age <- ifelse (d.merge$a13d.age>0, d.merge$a13d.age,NA)
# --------- Emigration status --- depends on emigr.age ----------
# status.emig <- ifelse (!is.na(d.merge$emigr.age),1,0)  # 1233
#  -------  Determine return migrants (flag.return)  ----------
# flag.return <- ifelse (!is.na(d.merge$return.age)&!is.na(d.merge$emigr.age),ifelse (d.merge$return.age>=d.merge$emigr.age,1,99),0)
# 355 return after first departure; 8 return before first departure (flag.return=99)

# -------  Exclude or include deceased persons in the HH  -----------
##every deceased person is a child of a Household head
#    d.merge$a3 = position in HH
#    d.merge$a2   age at death
# How many children died and what was age at death?
c_death <- subset (d.merge,!is.na(d.merge$a2.an))
# Age at death
addmargins(table (d.merge$a2.age[is.na(d.merge$a3) &!is.na(d.merge$a2.age)]))
# Age at emigration
c_a13a.age <- subset (a13a.age,is.na(d.merge$a3) & !is.na(d.merge$a2.an))
table (c_a13a.age)

# Use the following code to include deceased HH members as children of HH head.
# d.merge$a3 <- ifelse (is.na(d.merge$a3) & !is.na(d.merge$a2.an), 4, d.merge$a3) 
#  summary(d.merge$a3)

# ==    B. Determine position in HH for members of HH              ==
# HH status by sex  (Male= Homme;  Female=Femme)
HHstatus <- ifelse (d.merge$a3==1,"Head_of_HH",as.character(d.merge$a3))
HHstatus <- ifelse (d.merge$a1=="Male"&d.merge$a3==2,"Husband",ifelse (d.merge$a1=="Female"&d.merge$a3==2,"Wife",HHstatus))
HHstatus <- ifelse (d.merge$a1=="Male"&d.merge$a3==4,"Son",ifelse (d.merge$a1=="Female"&d.merge$a3==4,"Daughter",HHstatus))
HHstatus <- ifelse (d.merge$a1=="Male"&d.merge$a3==6,"Father",ifelse (d.merge$a1=="Female"&d.merge$a3==6,"Mother",HHstatus))
HHstatus <- ifelse (d.merge$a1=="Male"&d.merge$a3==5,"Son-in-law",ifelse (d.merge$a1=="Female"&d.merge$a3==5,"Daughter-in-law",HHstatus))
HHstatus <- ifelse (d.merge$a1=="Male"&d.merge$a3==11,"Grandson",ifelse (d.merge$a1=="Female"&d.merge$a3==11,"Granddaughter",HHstatus))
HHstatus <- ifelse (d.merge$a1=="Male"&d.merge$a3==8,"Brother",ifelse (d.merge$a1=="Female"&d.merge$a3==8,"Sister",HHstatus))
HHstatus <- ifelse (d.merge$a1=="Male"&d.merge$a3==9,"Brother-in-law",ifelse (d.merge$a1=="Female"&d.merge$a3==9,"Sister-in-law",HHstatus))
HHstatus <- ifelse (d.merge$a1=="Male"&d.merge$a3==10,"Nephew",ifelse (d.merge$a1=="Female"&d.merge$a3==10,"Niece",HHstatus))
HHstatus <- ifelse (d.merge$a1=="Male"&d.merge$a3==13,"FatherSpouseCM",ifelse (d.merge$a1=="Female"&d.merge$a3==13,"MotherSpouseCM",HHstatus))
HHstatus <- ifelse (d.merge$a1=="Male"&d.merge$a3==14,"MaleNoParentLink",ifelse (d.merge$a1=="Female"&d.merge$a3==14,"FemaleNoParentLink",HHstatus))
HHstatus <- ifelse (d.merge$a1=="Male"&d.merge$a3==7,"Grandfather",ifelse (d.merge$a1=="Female"&d.merge$a3==7,"Grandmother",HHstatus))
HHstatus <- ifelse (d.merge$a1=="Male"&d.merge$a3==3,"Co-husband",ifelse (d.merge$a1=="Female"&d.merge$a3==3,"Co-wife",HHstatus))
HHstatus <- ifelse (d.merge$a1=="Male"&d.merge$a3==12,"OtherFatherCM",ifelse (d.merge$a1=="Female"&d.merge$a3==12,"OtherMotherCM",HHstatus))

# ====        C.Create data frame of HH members                ======
# year.int computed in ReadMac2014 get.date.interview.R
data <- data.frame(ident=d.merge$ident,weight.hhd=d.merge$weight.hhd,sex=d.merge$a1,born=d.merge$q3annee,
                   death.age=d.merge$a2.age,death.yr=d.merge$a2.an,int.age=d.merge$q3age,
                   int.yr=d.merge$year.int,emigr.year=a13a.an,emigr.age=a13a.age,
                   strata.area=d.merge$strata.area,
                   strata.hh=d.merge$strata.hh, n.menage=d.merge$n.menage, 
                   area.ident=d.merge$num.codeur,num.dr=d.merge$num.dr,return.year=a13d.an,return.age=a13d.age)
# hhident = n.menage
# strata.area was stratum.area
data$HHstatus <- HHstatus  # character variable
data$HHstatus2 <- d.merge$a3 # status number

# ====    D. Allocate stratum (num.codeur = area.ident) to each HH member====
# ====        (see Sabine EmigRateWeighted2014.r)     ================
b<- data[!duplicated(data$hh.ident),]   # 1140
zz=as.numeric(unname (table (data$hh.ident)))
data$area.ident.ind <- NA
cd= 0
#for (ih in 1:nrow(b))
#{ cc =  cd + 1
#  cd =  cd + zz[ih]
#  data$area.ident.ind[cc:cd] <- b$area.ident[ih]
# }

# ====  E.  Remove subjects with unknown year of birth  ======
data <- subset (data,!is.na(data$born))   # 12108 left (242 missing dates of birth)

# =====         G. Data file of sons and daughers              ======
data.sd <- subset (data,data$HHstatus=="Son" | data$HHstatus=="Daughter")   
#  5475 persons in 2013  and 5478 in 2014 data
# NOTE:  data is file with all HH members; data.sd is file with sons and daughters 
# -----  Age categories of sons and daughters at survey date --------
data.sd$int.AG <- cut(data.sd$int.age,breaks=c(0,18,40,200),include.lowest=TRUE,right=FALSE,labels=c("0-17","18-39","40+"))   # 18-39
table (data.sd$int.AG)
# Creat datTemp
# ========  H.  Select subjects included in observation window  ===========
# Children born between 1936 and 1990 are between 18 and 40 in period 75-08
data.sd.75 <- subset (data.sd,data.sd$born>=1936 & data.sd$born <= 1990 & is.na(data.sd$death.yr))
# Remove those who died or emigrated before 1975: they are not at risk
data.sd.75e <- subset (data.sd.75,is.na(data.sd.75$emigr.year) | (!is.na(data.sd.75$emigr.year) & data.sd.75$emigr.year >= 1975) )
data.sd.75e <- subset (data.sd.75e,is.na(data.sd.75e$death.yr) | (!is.na(data.sd.75e$death.yr) & data.sd.75e$death.yr >= 1975))
# data.sd.75e = 3383 children

#  ========  I. Final data file: datTemp  =============================
datTemp <- data.sd.75e  # 3383

return (datTemp)
}


