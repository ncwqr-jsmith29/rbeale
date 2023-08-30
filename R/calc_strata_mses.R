#' Calculate strata MSE function
#'
#' Calculate MSE of all strata in a given individual in the population.
#' @param a A dataframe of individuals (alleles)
#' @keywords calculate strata mse
#' @examples
#' calc_strata_mse()

calc_strata_mses <- function(a, strata, obs_data, variable, strata_mses){
  for(j in 1:strata){
    boundary1 <- a[j] #include left boundary
    if(j != strata){
      boundary2 <- a[j+1]-1 #don't include right boundary
    }else{
      boundary2 <- a[j+1] #unless it's day 365/366
    }
    sbeale_result <- sbeale(obs_data$Flow[boundary1:boundary2], obs_data[boundary1:boundary2, variable])
    strata_mses[j,] <- sbeale_result
  }
  return(strata_mses)
}