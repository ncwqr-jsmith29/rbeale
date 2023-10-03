#' Create monthly combos
#'
#' Create a population of individuals (create alleles)
#' Imports:
#' RcppAlgos (>= 2.5.3)
#' @param strata Number of strata
#' @param last.allele Value of the greatest allele (i.e. 31 days in October)
#' @param n.cores Number of cores in computer, for parallel computing
#' @keywords create monthly combos combinations
#' @examples
#' create_monthly_combos()

create_monthly_combos <- function(strata, last.allele, n.cores){

  first.allele <- 1

  n = last.allele - 2 #max value of 2nd to last allele is last.allele-2 so last strata has 3 days (remember, the last strata includes both the left and right boundary)
  r = strata - 1
  #min value of 2nd allele is 4 (1+3) (remember, all other strata do not include the right boundary)
  combos <- RcppAlgos::comboGeneral(4:n, r,
                                    repetition = F,
                                    Parallel = T,
                                    nThreads = n.cores - 1)
  #now, individuals in combos have at least 3 days in first strata and at least 3 in last strata

  diffs <- t(diff(t(combos)))

  if(strata == 3){
    three_day_problem <- which(diffs < 3)
    combos <- combos[-three_day_problem,]
  }

  if(strata > 3){
    #find individuals whose last strata has fewer than 3 days (remember, the last strata includes both the left and right boundary)
    last_strata_problem <- which(diffs[,strata-2] < 2)
    diffs <- diffs[-last_strata_problem,]
    combos <- combos[-last_strata_problem,]
    #find individuals whose strata has fewer than 3 days (remember, all other strata do not include right boundary)
    if(strata == 4){
      three_day_problem <- which(diffs[,1:(strata - 3)] < 3, arr.ind = T)
    }else{
      three_day_problem <- which(diffs[,1:(strata - 3)] < 3, arr.ind = T)[,"row"]
    }
    three_day_problem <- three_day_problem[!duplicated(three_day_problem)]
    combos <- combos[-three_day_problem,]
  }

  #if strata == 2, then no individuals need to be eliminated
  combos <- cbind(1, combos, last.allele)
  colnames(combos) <- 1:(strata+1)

  return(combos)

}
