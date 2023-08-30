#' sbeale function
#'
#' Calculate loads, confidence intervals, degrees of freedom.
#' The Beale Ratio Estimator function: Rem Confesor Jr. 20160802
#' Estimation of pollutant loads in rivers in streams: A guidance document for NPS programs. Project report prepared under Grand X998397-01-0. Refer to Tin, M. 1965. Comparison of Ratio Estimators. American Statistical Association J.
#' Imports:
#' stats (>= 4.0.5)
#' @param uflow average flow for the strata
#' @param conc vector of concentration data for strata
#' @keywords beale
#' @examples
#' sbeale()

sbeale <- function(uflow,conc) {  # this syntax  should be followed when executing the function
  flowmu <- mean(uflow,na.rm=T) # average flow for the mon/period/strata based from USGS data; this is flowmu in Pete's code
  temp02 <- uflow[!is.na(conc)] # array of flows in days with conc data
  temp03 <- conc[!is.na(conc)] # array of conc in days with conc data
  
  obs.len <- length(temp03) # no of days with conc; this r(i) in Pete's code
  str.len <- length(uflow)  # no of days with usgs flow data or length of strata; this nf in Pete's code
  
  dload <- temp03*temp02*86.4 # array of daily load, kg/day, in days with conc. data  &&  usgs flow data
  avload <- mean(dload,na.rm=T) # average load, kg/day, of days with conc. data  &&  usgs flow data;NAs are omitted; this is avload in Pete's code
  avflow <- mean(temp02,na.rm=T) # average flow,cms, of days with conc. data  &&  usgs flow data;NAs are omitted; this is avflow in Pete's code
  
  sql <- stats::cov(temp02,dload) # covariance of flow and load: this is sxy in Pete's code
  sqq <- stats::var(temp02)        # variance of flow: this is sx2 in Pete's code
  sll <- stats::var(dload)        # variance of load: this is sy2 in Pete's code
  
  sxy <- sql
  sx2 <- sqq
  sy2 <- sll
  
  sumx3 <- sum((temp02-avflow)^3)
  sumx2y <- sum((temp02-avflow)^2 * (dload-avload))
  sumxy2 <- sum((temp02-avflow) * (dload-avload)^2)
  sx2y <- (sumx2y/(obs.len-1))/(avflow^2*avload)
  sxy2 <- (sumxy2/(obs.len-1))/(avflow*avload^2)
  sx3  <- (sumx3/(obs.len-1))/(avflow^3)
  
  #Assuming str.len >>>; 1/str.len = 0 Pete's autobeale code
  # More general equation form:  
  fa <- 1 + (1/obs.len-1/str.len) * (sxy/(avload*avflow))
  fb <- 1 + (1/obs.len-1/str.len) * (sx2/(avflow*avflow))
  
  dload.bi <- avload*(flowmu/avflow) # average daily load for the period/strata k/day
  dload.ub <- avload*(flowmu/avflow)*(fa/fb) # average daily load for the period/strata k/day
  tload <- round(dload.ub*str.len,1) # total load for the period/strata kg
  
  s1 <- sx2/(avflow*avflow)
  s2 <- sy2/(avload*avload)
  s3 <- sxy/(avflow*avload)
  
  # 2nd-order MSE:
  f1 <- (1/obs.len) * (s1 + s2 - 2*s3)
  f2 <- (1/obs.len)^2 * (2 * s1^2 - 4*s1*s3 + s3^2 + s1*s2)
  f3 <- ((2/obs.len)/str.len) * (sx3 - 2*sx2y + sxy2)
  # #################
  # if(f3 < 1e-15){
  #   f3 <- 0
  # }
  # #################
  mse.d <- (avload*flowmu/avflow)^2*(f1+f2+f3)
  mse.s <- mse.d*(str.len^2)
  
  if (obs.len < 2) {
    dload.ub <- avload*(flowmu/avflow) # average daily load for the period/strata k/day
    tload <- round(dload.ub*str.len,5) # total load for the period/strata kg
    mse.d <- 1e+99
  }
  
  degf <- obs.len-1
  CI <- qt(0.975,df=degf)*(mse.s^0.5)/(degf^0.5)
  returnval <- c(dload.bi, dload.ub, tload, degf, obs.len, str.len, mse.d, CI, flowmu, avflow)
  names(returnval) <- c("dload.bi_kg/d", "dload.ub_kg/d", "tload_kg/lenS", "degf", "lenObs", "lenStr", "MSE_kg/lenS", "Cl_kg/lenS", "flowmu_m3/s", "flowav_m3/s")
  returnval  # return the values
}