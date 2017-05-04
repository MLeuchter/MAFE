# piecewise (from eha) WITH WEIGHTS
piecewise_w <- function (survv, cutpoints,nweight) 
{ # survv <- survo$surv
  enter <- survv[,1]
  exit <- survv[,2]
  event <- survv[,3]
  n <- length(cutpoints) + 1
  d <- numeric(n)
  tt <- numeric(n)
  nn <- length(enter)
  d[1] <- sum(nweight[((exit <= cutpoints[1]) & (exit > 0))]*event[((exit <= cutpoints[1]) & (exit > 0))])
  left <- pmin(enter, cutpoints[1])
  right <- pmin(exit, cutpoints[1])
  tt[1] <- sum(nweight*(right - left))
  for (j in 2:(n - 1)) {
    d[j] <- sum(nweight[((exit <= cutpoints[j]) & (exit > cutpoints[j - 1]))]
                *event[((exit <= cutpoints[j]) & (exit > cutpoints[j - 1]))])
    left <- pmin(pmax(enter, cutpoints[j - 1]), cutpoints[j])
    right <- pmax(pmin(exit, cutpoints[j]), cutpoints[j - 1])
    tt[j] <- sum(nweight*(right - left))
  }
  d[n] <- sum(nweight[(exit > cutpoints[n - 1])] *event[(exit > cutpoints[n - 1])])
  left <- pmax(enter, cutpoints[n - 1])
  right <- pmax(exit, cutpoints[n - 1])
  tt[n] <- sum(nweight*(right - left))
  intensity <- ifelse(tt > 0, d/tt, NA)
  z <- list(events = d, exposure = tt, intensity = intensity)
  return (z)
}