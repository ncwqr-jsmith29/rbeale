#' Spring rbeale function
#'
#' Calculate spring loads.
#' Imports:
#' parallel (>= 4.0.5)
#' dplyr (>= 1.0.7)
#' lubridate (>= 1.8.0)
#' doParallel (>= 1.0.17)
#' @param parent_directory Directory where input files are and output files should go
#' @param input_rivers Vector of input rivers, river names as character objects, spelled the same as csv file
#' @keywords spring rbeale
#' @export
#' @examples
#' spring()

spring <- function(parent_directory, input_rivers){
  print("Starting spring...")

  #Before running, convert data portal excel files to csv

  rivers <- input_rivers

  for(river in rivers){

    data.portal.file <- read.csv(paste(parent_directory, "/", river, "/", river, ".csv", sep = ""))
    all_obs_data <- fix_spring_input(data.portal.file)

    variables <- c("TSS", "TP", "SRP", "NO23", "TKN", "Cl", "SO4", "Si")
    wat_years <- unique(all_obs_data$WatYr)

    n.cores <- parallel::detectCores()

    #set max strata you want to test
    maxstrata=14

    options(warn = -1)
    cl <- parallel::makeCluster(n.cores-1)
    doParallel::registerDoParallel(cl)

    report_vars <- c("dload.bi_kgd", "dload.ub_kgd", "tload_kglenS", "degf", "lenObs", "lenStr", "MSE_kglenS", "Cl_kglenS", "flowmu_m3s", "flowav_m3s")

    #convert tkn to totn
    # flow <- all_obs_data$Flow
    # all_obs_data[,26] <- all_obs_data[,26]/flow/86.4*1000

    best_individuals <- data.frame(s1 = numeric(),
                                   s2 = numeric(),
                                   s3 = numeric(),
                                   s4 = numeric(),
                                   s5 = numeric(),
                                   s6 = numeric(),
                                   s7 = numeric(),
                                   s8 = numeric(),
                                   s9 = numeric(),
                                   s10 = numeric(),
                                   s11 = numeric(),
                                   s12 = numeric(),
                                   s13 = numeric(),
                                   s14 = numeric())

    for(variable in variables){

      ##Build data frames for output files
      best_individuals_all <- data.frame(s1 = numeric(),
                                         s2 = numeric(),
                                         s3 = numeric(),
                                         s4 = numeric(),
                                         s5 = numeric(),
                                         s6 = numeric(),
                                         s7 = numeric(),
                                         s8 = numeric(),
                                         s9 = numeric(),
                                         s10 = numeric(),
                                         s11 = numeric(),
                                         s12 = numeric(),
                                         s13 = numeric(),
                                         s14 = numeric())
      best_strata_mses_all <- data.frame(strata = numeric(),
                                         strata.n = numeric(),
                                         dload.bi_kgd = numeric(),
                                         dload.ub_kgd = numeric(),
                                         tload_kglenS = numeric(),
                                         degf = numeric(),
                                         lenObs = numeric(),
                                         lenStr = numeric(),
                                         MSE_kglenS = numeric(),
                                         Cl_kglenS = numeric(),
                                         flowmu_m3s = numeric(),
                                         flowav_m3s = numeric()) #mse.m_lowest_all_strata #all_strata_best_mse
      best_individual_mses_all <- data.frame(strata = numeric(),
                                             dload.bi_kgd = numeric(),
                                             dload.ub_kgd = numeric(),
                                             tload_kglenS = numeric(),
                                             degf = numeric(),
                                             lenObs = numeric(),
                                             lenStr = numeric(),
                                             MSE_kglenS = numeric(),
                                             Cl_kglenS = numeric(),
                                             flowmu_m3s = numeric(),
                                             flowav_m3s = numeric()) #mse.m_lowest_summary #best_summary_mses
      individual_mses_all <- data.frame(strata = numeric(),
                                        dload.bi_kgd = numeric(),
                                        dload.ub_kgd = numeric(),
                                        tload_kglenS = numeric(),
                                        degf = numeric(),
                                        lenObs = numeric(),
                                        lenStr = numeric(),
                                        MSE_kglenS = numeric(),
                                        Cl_kglenS = numeric(),
                                        flowmu_m3s = numeric(),
                                        flowav_m3s = numeric())
      out.year=data.frame(year = numeric())


      for(year in wat_years){

        obs_data <- dplyr::filter(dplyr::filter(all_obs_data, WatYr == year), Mo == c(3:7))

        if(all(is.na(obs_data[,variable]))){
          print(paste("No data for", variable, "in", year, sep = " "))
          filename = paste(parent_directory, "/", river, "/Output/Spring/", variable, "/", variable, "_Error_", year, ".txt", sep = '')
          writeLines(paste("No data for", variable, "in", year, sep = " "), filename)
          next
        }

        try_strata = seq(1, maxstrata)

        best_strata_mses <- data.frame(strata = numeric(),
                                       strata.n = numeric(),
                                       dload.bi_kgd = numeric(),
                                       dload.ub_kgd = numeric(),
                                       tload_kglenS = numeric(),
                                       degf = numeric(),
                                       lenObs = numeric(),
                                       lenStr = numeric(),
                                       MSE_kglenS = numeric(),
                                       Cl_kglenS = numeric(),
                                       flowmu_m3s = numeric(),
                                       flowav_m3s = numeric()) #mse.m_lowest_all_strata #all_strata_best_mse
        best_individual_mses <- data.frame(strata = numeric(),
                                           dload.bi_kgd = numeric(),
                                           dload.ub_kgd = numeric(),
                                           tload_kglenS = numeric(),
                                           degf = numeric(),
                                           lenObs = numeric(),
                                           lenStr = numeric(),
                                           MSE_kglenS = numeric(),
                                           Cl_kglenS = numeric(),
                                           flowmu_m3s = numeric(),
                                           flowav_m3s = numeric()) #mse.m_lowest_summary #best_summary_mses


        N <- length(which(!is.na(obs_data$Flow)))

        if(all(!is.na(obs_data[,variable]))){
          strata_mses <- sbeale(obs_data$Flow, obs_data[, variable])
          strata_mses[7] <- strata_mses[7] * N^2
          best_strata_mses[1,] <- c(1, 1, strata_mses)
          best_individual_mses[1,] <- c(1, strata_mses)

          print(paste("Running sbeale for", variable, "...", year, sep = " "))

          #format for output files
          one_individual_mse<- c(0, strata_mses)#format for output files
          best_individuals <- c(0, strata_mses)  #format for output files

        }else{

        for(strata in try_strata){
          print(paste("Processing", strata, "strata for", variable, "...", year, sep = " "))
          if(strata == 1){
            popSize <- 1
            pop <- c(1,153)
            strata_mses <- sbeale(obs_data$Flow, obs_data[, variable])
            strata_mses[7] <- strata_mses[7] * N^2
            best_strata_mses[1,] <- c(1, 1, strata_mses)
            best_individual_mses[1,] <- c(1, strata_mses)
          }else{
            if(strata == 2){
              popSize = 153

              variable.data <- dplyr::pull(obs_data, variable)

              big_combos = create_spring_combos(strata, popSize, year, variable.data, n.cores)

              problem_individuals <- logical()
              parallel::clusterExport(cl, list("find_problem_individuals", "problem_individuals", "variable.data", "strata"), envir = environment())
              problem_individuals <- parallel::parRapply(cl, big_combos, find_problem_individuals)
              #eliminate problem individuals
              big_combos <- big_combos[!problem_individuals,]

              if(nrow(big_combos) < popSize){
                if (nrow(big_combos) %% 2 == 0){
                  popSize <- nrow(big_combos)
                }else{
                  popSize <- nrow(big_combos) - 1
                }
                print(paste("New popSize = ", popSize, sep = ""))
              }

              if(popSize == 0){
                break #breaks out of strata for-loop, which moves on to next variable
              }

              pop = big_combos

              strata_mses <- data.frame(dload.bi_kgd = numeric(),
                                        dload.ub_kgd = numeric(),
                                        tload_kglenS = numeric(),
                                        degf = numeric(),
                                        lenObs = numeric(),
                                        lenStr = numeric(),
                                        MSE_kglenS = numeric(),
                                        Cl_kglenS = numeric(),
                                        flowmu_m3s = numeric(),
                                        flowav_m3s = numeric())
              pop_strata_mses <- vector(mode = "list", length = popSize)
              names(pop_strata_mses) <- 1:popSize

              pop_strata_mses <- apply(pop, 1, calc_strata_mses, strata = strata, obs_data = obs_data, variable = variable, strata_mses = strata_mses)

              individual_mses <- do.call(rbind.data.frame, lapply(pop_strata_mses, calc_individual_mse, N))
              names(individual_mses) <- report_vars
              individual_mses <- dplyr::mutate(individual_mses, index = 1:nrow(individual_mses), .before = 1)

              lowest_individual_mses <- dplyr::arrange(individual_mses, MSE_kglenS)
              lowest_individual_indices <- dplyr::pull(lowest_individual_mses, index)

              lowest_individual_mses <- dplyr::select(lowest_individual_mses, -index)

              pop <- pop[lowest_individual_indices,]
              pop_strata_mses <- pop_strata_mses[lowest_individual_indices]

              best_individual <- as.numeric(as.vector(pop[1,]))
              best_individual <- c(best_individual, rep(NA, maxstrata-strata-1))
              best_individuals[strata,] <- best_individual

              #best individual mse for this strata
              best_individual_mses[strata,] <- c(strata, as.numeric(as.vector(lowest_individual_mses[1,])))

              #best strata mses for this strata
              add.rows <- cbind(rep(strata, strata), 1:strata, pop_strata_mses[[1]])
              names(add.rows)[1:2] <- c("strata", "strata.n")
              best_strata_mses <- dplyr::bind_rows(best_strata_mses, add.rows)

            }else{ #if strata > 2
              popSize = 1500

              variable.data <- dplyr::pull(obs_data, variable)

              big_combos = create_spring_combos(strata, popSize, year, variable.data, n.cores)

              problem_individuals <- logical()
              parallel::clusterExport(cl, list("find_problem_individuals", "problem_individuals", "variable.data", "strata"), envir = environment())
              problem_individuals <- parallel::parRapply(cl, big_combos, find_problem_individuals)
              #eliminate problem individuals
              big_combos <- big_combos[!problem_individuals,]

              if(nrow(big_combos) < popSize){
                if (nrow(big_combos) %% 2 == 0){
                  popSize <- nrow(big_combos)
                }else{
                  popSize <- nrow(big_combos) - 1
                }
                print(paste("New popSize = ", popSize, sep = ""))
              }

              if(popSize == 0){
                break #breaks out of strata for-loop, which moves on to next variable
              }

              pop = big_combos[1:popSize,]
              big_combos = big_combos[-(1:popSize),]

              iter = 35
              stopIter <- F

              for(i in seq(1,iter)){
                print(paste("Iter = ", i, sep = ""))
                if(i == 1){
                  strata_mses <- data.frame(dload.bi_kgd = numeric(),
                                            dload.ub_kgd = numeric(),
                                            tload_kglenS = numeric(),
                                            degf = numeric(),
                                            lenObs = numeric(),
                                            lenStr = numeric(),
                                            MSE_kglenS = numeric(),
                                            Cl_kglenS = numeric(),
                                            flowmu_m3s = numeric(),
                                            flowav_m3s = numeric())
                  pop_strata_mses <- vector(mode = "list", length = popSize)
                  names(pop_strata_mses) <- 1:popSize

                  parallel::clusterExport(cl, list("calc_strata_mses", "obs_data", "strata_mses", "strata", "sbeale", "variable"), envir = environment())
                  pop_strata_mses <- parallel::parRapply(cl, pop, calc_strata_mses, strata = strata, obs_data = obs_data, variable = variable, strata_mses = strata_mses)

                }else{
                  pop_strata_mses <- old_strata_mses
                }

                children = crossover(pop, variable.data)
                #check if children need to be supplemented by additional individuals from big_combos
                if(nrow(children) < popSize){
                  extras.needed <- popSize - nrow(children)
                  if(nrow(big_combos) < extras.needed){
                    #stop("big_combos < extras.needed")
                    print("big_combos < extras.needed, stopping iter")
                    stopIter <- T
                    children <- rbind(children, big_combos)
                    children = crossover(pop, dplyr::pull(obs_data, variable)) #do another crossover to ensure that added children are valid and are not clones
                  }else{
                    children <- rbind(children, head(big_combos, extras.needed))
                    big_combos <- big_combos[-(1:extras.needed),]
                  }
                }
                parallel::clusterExport(cl, list("calc_strata_mses", "obs_data", "strata_mses", "strata", "sbeale", "variable"), envir = environment())
                child_strata_mses <- parallel::parRapply(cl, children, calc_strata_mses, strata = strata, obs_data = obs_data, variable = variable, strata_mses = strata_mses)
                bigger_pop_strata_mses = append(pop_strata_mses, child_strata_mses)


                individual_mses <- do.call(rbind.data.frame, lapply(bigger_pop_strata_mses, calc_individual_mse, N))
                names(individual_mses) <- report_vars
                individual_mses <- dplyr::mutate(individual_mses, index = 1:nrow(individual_mses), .before = 1)

                bigger_pop <- as.data.frame(rbind(pop, children))

                lowest_individual_mses <- dplyr::slice_head(dplyr::arrange(individual_mses, MSE_kglenS), n = popSize)
                lowest_individual_indices <- dplyr::pull(lowest_individual_mses, index)

                lowest_individual_mses <- dplyr::select(lowest_individual_mses, -index)

                #make new pop from lowest mses
                pop <- bigger_pop[lowest_individual_indices,]

                old_strata_mses <- bigger_pop_strata_mses[lowest_individual_indices]

                if(stopIter == T){
                  break
                }

              }#iter

              best_individual <- as.numeric(as.vector(pop[1,]))
              if(strata < maxstrata){
                best_individual <- c(best_individual, rep(NA, maxstrata-strata-1))
              }
              best_individuals[strata,] <- best_individual

              #best individual mse for this strata
              best_individual_mses[strata,] <- c(strata, as.numeric(as.vector(lowest_individual_mses[1,])))

              #best strata mses for this strata
              add.rows <- cbind(rep(strata, strata), 1:strata, old_strata_mses[[1]])
              names(add.rows)[1:2] <- c("strata", "strata.n")
              best_strata_mses <- dplyr::bind_rows(best_strata_mses, add.rows)
            }#end else
          }#strata
        }

          one_individual_mse <- dplyr::slice_head(dplyr::arrange(best_individual_mses, MSE_kglenS), n = 1)

      }#end else


        ##add data to the dataframes whether it's from the raw or crossover data
        individual_mses_all[nrow(individual_mses_all) + 1,] <- one_individual_mse #best_
        #best_individuals_all[nrow(best_individuals_all) + nrow(best_individuals),] <- best_individuals #best_individuals
        best_strata_mses_all[nrow(best_strata_mses_all) + 1,] <- best_strata_mses #best_strata_mses
        best_individual_mses_all[nrow(best_individual_mses_all) + 1,] <- best_individual_mses #best_individual_mses

        #create year output to build final output files
        out.year[nrow(out.year) +1,] <- year

      }#year

      #add the year into the new output files
      #best_individuals_all <- cbind(out.year, best_individuals_all)
      individual_mses_all <- cbind(out.year, individual_mses_all)
      best_individual_mses_all <- cbind(out.year, best_individual_mses_all)
      best_strata_mses_all <- cbind(out.year, best_strata_mses_all)


      #write file for each variable - output should have 4 total files with all of that variable's data
      filename = paste(parent_directory, "/", river, "/Output/Spring/", variable, "/", variable, "_best_strata_mses_", year, ".csv", sep = '')
      write.csv(best_strata_mses, filename, row.names = FALSE)

      filename = paste(parent_directory, "/", river, "/Output/Spring/", variable, "/", variable, "_best_individual_mses_", year, ".csv", sep = '')
      write.csv(best_individual_mses, filename, row.names = FALSE)

      filename = paste(parent_directory, "/", river, "/Output/Spring/", variable, "/", variable, "_best_", year, ".csv", sep = '')
      write.csv(one_individual_mse, filename, row.names = FALSE)

      filename = paste(parent_directory, "/", river, "/Output/Spring/", variable, "/", variable, "_best_individuals_", year, ".csv", sep = '')
      write.csv(best_individuals, filename, row.names = FALSE)

    }#variable
  }#river
}#function
