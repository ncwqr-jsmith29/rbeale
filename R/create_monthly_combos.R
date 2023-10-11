#' Create monthly combos
#'
#' Create a population of individuals (create alleles)
#' Imports:
#' RcppAlgos (>= 2.5.3)
#' @param k Number of strata
#' @param n Number of individuals
#' @param yearmo Year-Month of working data
#' @param variable.data Data of working variable
#' @keywords create monthly combos combinations
#' @examples
#' create_monthly_combos()

create_monthly_combos <- function(k,n,yearmo,variable.data,n.cores){
  year <- as.numeric(substr(yearmo, 1, 4))
  mo <- as.numeric(substr(yearmo, 5, 6))
  if((lubridate::leap_year(year)) && (mo == 2)){
    last.allele <- 29
  }else{
    last.allele <- switch(mo, 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
  }

  #doesn't make sense for 2nd allele to be < 4, doesn't make sense for 2nd to last allele to be > last.allele - 3 (362 or 363)
  #make 1000x the necessary number because we will just eliminate problem individuals and keep the extras for when there are problems with children
  if(k == 2){
    big_combos <- 4:(last.allele-3)
  }else{
    max_combos <- RcppAlgos::comboCount(4:(last.allele-3), m = k-1)
    #limit for RcppAlgos package = 2^31-1
    if(max_combos > n*1000){
      max_combos <- n*1000
    }
    #"ComboSample" creates strictly increasing combinations and automatically samples from all possible combinations.
    big_combos <- RcppAlgos::comboSample(4:(last.allele-3), m = k-1, n = max_combos, seed = 12345, Parallel = T, nThreads = n.cores - 1)
  }

  big_combos <- as.data.frame(cbind(1, big_combos, last.allele))

  colnames(big_combos) <- 1:(k+1)

  return(big_combos)
}
