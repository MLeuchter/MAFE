# CREATE one BOOTSTRAP REPLICATE from parent sample (pseudopopulation) 
createRepl <- function(datR){
  R <- NULL
  for(strA in 1:10){
    A <- datR[datR$strata.area %in% strA, ] # select data from stratum strA (dim(A) = number individuals)
    B <- A[!duplicated(A$n.menage),] # construct a household data set (dim(B) = number of households in stratum)
    tab <- table(B$num.dr, exclude=NULL) # num.dr = survey area ID (see "PRESENTATION OF THE DATASETS    )
    tab <- tab[which(tab!=0)]            # Number of HH in each district in stratum strA
    vals <- as.numeric(names(tab)) 
    vals[is.na(vals)] <- 0 
    # select randomly n_h-1 districts:
    sU <- sample(x=vals, size=length(tab)-1, replace=T) 
    C <- NULL
    # apply ultimate cluster principle: when an area is taken, take all successive stage units (= households) into replicate
    for(j in sU){ 
      # Select all HH in district j: all HH with num.dr=j (n.menage = ID of HH)
      if(j>0){
        hhid <- B[which(B$num.dr == j),"n.menage"]
      } else {
        hhid <- B[which(is.na(B$num.dr)),"n.menage"] 
      }
      # Construct data file of all members of HH in selected districts
      C <- rbind(C,A[A$n.menage %in% hhid,]) 
    }
    R <- rbind(R,C) 
  }
  colnames(R) <- names(datR)
  return(R)
}
# ---------------------------------------------------------------------
