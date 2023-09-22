#' Crossover function
#'
#' Take a population, divide in two, and combine the individuals to create children (mating)
#' Imports:
#' dplyr (>= 1.0.7)
#' tidyr (>= 1.1.3)
#' @param pop data frame of individuals (alleles)
#' @param variable.data vector of concentration data for current working variable; used to adjust alleles to have at least 3 data points
#' @keywords crossover
#' @examples
#' crossover()

crossover <- function(pop, variable.data){
  allele_indices <- seq(2,ncol(pop)-1) #doesn't make sense to have 1 or 365 be a crossover pt
  n.alleles <- ncol(pop)
  popSize <- nrow(pop)
  parents.indices <- 1:popSize

  parent1.indices <- sample(parents.indices, popSize/2)
  parent2.indices <- parents.indices[-parent1.indices]

  parent1 <- pop[parent1.indices,]
  colnames(parent1) <- paste("p1", 1:ncol(parent1), sep = ".")
  parent2 <- pop[parent2.indices,]
  colnames(parent2) <- paste("p2", 1:ncol(parent2), sep = ".")

  crossover_pts <- sample(allele_indices, nrow(parent1), replace = T)

  parents.cross <- suppressMessages(dplyr::bind_cols(parent1, parent2, crossover_pts))
  names(parents.cross)[ncol(parents.cross)] <- "cross"

  p1.start.index <- 1
  p1.end.index <- n.alleles
  p2.start.index <- n.alleles+1
  p2.end.index <- 2*n.alleles
  crossover_pt.index <- n.alleles*2+1

  create_children <- function(a){
    if(a[1, crossover_pt.index] == 2){
      #children created by crossing end of parents
      child1 <- c(a[1, p1.start.index:(a[1, crossover_pt.index])], a[1, (p1.end.index+a[1, crossover_pt.index]+1):p2.end.index])
      child2 <- c(a[1, p2.start.index:(p1.end.index + a[1, crossover_pt.index])], a[1, (p1.start.index + a[1, crossover_pt.index]):p1.end.index])
    }else if(a[1, crossover_pt.index] == (n.alleles - 1)){
      #children created by crossing beginning of parents
      child1 <- c(a[1, p2.start.index:(p1.end.index+a[1, crossover_pt.index]-1)], a[1, a[1, crossover_pt.index]:p1.end.index])
      child2 <- c(a[1, p1.start.index:(a[1, crossover_pt.index]-1)], a[1, (p1.end.index + a[1, crossover_pt.index]):p2.end.index])
    }else{
      #children created by crossing end of parents
      child1 <- c(a[1, p1.start.index:(a[1, crossover_pt.index])], a[1, (p1.end.index+a[1, crossover_pt.index]+1):p2.end.index])
      child2 <- c(a[1, p2.start.index:(p1.end.index + a[1, crossover_pt.index])], a[1, (p1.start.index + a[1, crossover_pt.index]):p1.end.index])
      #children created by crossing beginning of parents
      child1 <- cbind(child1, c(a[1, p2.start.index:(p1.end.index+a[1, crossover_pt.index]-1)], a[1, a[1, crossover_pt.index]:p1.end.index]))
      child2 <- cbind(child2, c(a[1, p1.start.index:(a[1, crossover_pt.index]-1)], a[1, (p1.end.index + a[1, crossover_pt.index]):p2.end.index]))
    }
    return(cbind(child1, child2))
  }

  children <- as.data.frame(lapply(split(parents.cross, seq(nrow(parents.cross))), create_children))
  children <- as.data.frame(t(children))

  #change children columns from lists to vectors
  children <- tidyr::unnest(children, 1:n.alleles)
  colnames(children) <- 1:n.alleles

  #count the number of non-na values in each strata for each individual
  non_na_instrata <- as.data.frame(matrix(ncol = (n.alleles - 1)))
  names(non_na_instrata) <- c(1:(n.alleles-1))

  find_nonna_strata <- function(a){
    for(j in 1:(n.alleles-1)){
      boundary1 <- a[j] #include left boundary
      if(j != (n.alleles-1)){
        boundary2 <- a[j+1]-1 #don't include right boundary
      }else{
        boundary2 <- a[j+1] #unless it's day 365/366
      }
      non_na_instrata[,j] <- sum(!is.na(variable.data[boundary1:boundary2]))
    }
    return(non_na_instrata)
  }

  non_na_instrata <- dplyr::bind_rows(apply(children, 1, find_nonna_strata))

  #eliminate individuals with strata that don't have at least 3 non-na data point
  problem_strata <- which(non_na_instrata < 3, arr.ind = T)
  problem_children <- unique(problem_strata[,1])
  children <- children[-problem_children,]

  #eliminate clones
  children <- dplyr::anti_join(children, pop, by = colnames(children))

  #check if there are too many children, even after elimination
  if(nrow(children) > popSize){
    children <- children[1:popSize,]
  }

  return(children)
}
