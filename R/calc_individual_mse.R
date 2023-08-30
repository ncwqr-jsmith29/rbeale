#' Calculate individual MSE function
#'
#' Calculate MSE of a given individual in the population.
#' @param a A dataframe of individuals (alleles) and strata mses
#' @keywords calculate individual mse
#' @examples
#' calc_individual_mse()

calc_individual_mse <- function(a, N){
  v1 <- sum(a$dload.bi_kgd * a$lenStr/N, na.rm = T)
  v2 <- sum(a$dload.ub_kgd * a$lenStr/N, na.rm = T)
  v3 <- sum(a$tload_kglenS, na.rm = T)
  
  #calc effective degrees of freedom
  numer <- (sum(a$MSE_kglenS * a$lenStr^2 / N^2, na.rm = T))^2 * N^4
  denom <- sum(a$MSE_kglenS^2 * a$lenStr^4 / a$degf, na.rm = T)
  v4 <- numer / denom
  
  v5 <- sum(a$lenObs, na.rm = T)
  v6 <- sum(a$lenStr, na.rm = T)
  v7 <- sum(a$MSE_kglenS * a$lenStr^2 / N^2, na.rm = T) * N^2
  
  mse <- v7
  degf <- v4
  
  v8 <- qt(0.975,df=degf)*(mse^0.5)/(degf^0.5)
  v9 <- sum(a$flowmu_m3s * a$lenStr / N, na.rm = T)
  v10 <- sum(a$flowav_m3s * a$lenStr / N, na.rm = T)
  
  return(c(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10))
}