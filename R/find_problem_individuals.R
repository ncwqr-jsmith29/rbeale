#' Find problem individuals function
#'
#' Find and return individuals in a population whose strata don't include at least 3 data points.
#' @param a A dataframe of individuals (alleles)
#' @keywords find problem individuals
#' @export
#' @examples
#' find_problem_individuals()

find_problem_individuals <- function(a){
  individual_logical <- F
  for(j in 1:strata){
    boundary1 <- a[j] #include left boundary
    if(j != strata){
      boundary2 <- a[j+1]-1 #don't include right boundary
    }else{
      boundary2 <- a[j+1] #unless it's day 365/366
    }
    if(sum(!is.na(variable.data[boundary1:boundary2])) < 3){
      individual_logical <- T
      break
    }
    #non_na_instrata[,j] <- sum(!is.na(variable.data[boundary1:boundary2]))
  }
  problem_individuals <- c(problem_individuals, individual_logical)
  return(problem_individuals)
  #return(non_na_instrata)
}
