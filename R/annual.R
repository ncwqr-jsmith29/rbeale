#' Annual rbeale function
#'
#' Calculate annual loads.
#' Imports:
#' parallel (>= 4.0.5)
#' dplyr (>= 1.0.7)
#' lubridate (>= 1.8.0)
#' doParallel (>= 1.0.17)
#' @param parent_directory Directory where input files are and output files should go
#' @param input_rivers Vector of input rivers, river names as character objects, spelled the same as csv file
#' @keywords annual
#' @export
#' @examples
#' annual()



##Before running, convert data portal excel files to csv
annual <- function(parent_directory, input_rivers){
print("Starting annual...")

rivers <- input_rivers

for(river in rivers){

  data.portal.file <- read.csv(paste(parent_directory, "/", river, "/", river, ".csv", sep = ""))
  all_obs_data <- fix_annual_input(data.portal.file)

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

  #Where s1-s14 = a different strata. In order to have all values in the output, must have s# for as many strata as is running.
  best_individuals <- data.frame(yearmo = numeric(),
                                 a1 = numeric(),
                                 a2 = numeric(),
                                 a3 = numeric(),
                                 a4 = numeric(),
                                 a5 = numeric(),
                                 a6 = numeric(),
                                 a7 = numeric(),
                                 a8 = numeric(),
                                 a9 = numeric(),
                                 a10 = numeric(),
                                 a11 = numeric(),
                                 a12 = numeric(),
                                 a13 = numeric(),
                                 a14 = numeric(),
                                 a15 = numeric())

  for(variable in variables){

    ##Build data frames for output files
    best_individuals_all <- data.frame(yearmo = numeric(),
                                       a1 = numeric(),
                                       a2 = numeric(),
                                       a3 = numeric(),
                                       a4 = numeric(),
                                       a5 = numeric(),
                                       a6 = numeric(),
                                       a7 = numeric(),
                                       a8 = numeric(),
                                       a9 = numeric(),
                                       a10 = numeric(),
                                       a11 = numeric(),
                                       a12 = numeric(),
                                       a13 = numeric(),
                                       a14 = numeric(),
                                       a15 = numeric())
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
    out.year.mult=data.frame(year = numeric())
    out.year.mult.bim = data.frame(year = numeric())
    out.year.mult.bi = data.frame(year = numeric())

    for(year in wat_years){

      obs_data <- dplyr::filter(all_obs_data, WatYr == year)

      if(all(is.na(obs_data[,variable]))){
        print(paste("No data for", variable, "in", year, sep = " "))
        filename = paste(parent_directory, "/", river, "/Output/Annual/", variable, "/", variable, "_Error_", year, ".txt", sep = '')
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
        best_strata_mses[1,] <- c(0, 1, strata_mses)
        best_individual_mses[1,] <- c(0, strata_mses)

        print(paste("Running sbeale for", variable, "...", year, sep = " "))

        #format for output files
        one_individual_mse<- c(0, strata_mses)#format for output files

      }else{

      for(strata in try_strata){
        print(paste("Processing", strata, "strata for", variable, "...", year, sep = " "))
        if(strata == 1){
          popSize <- 1
          pop <- c(1,365)
          strata_mses <- sbeale(obs_data$Flow, obs_data[, variable])
          strata_mses[7] <- strata_mses[7] * N^2
          best_strata_mses[1,] <- c(1, 1, strata_mses)
          best_individual_mses[1,] <- c(1, strata_mses)
        }else{
          if(strata == 2){
            if(!lubridate::leap_year(year)){
              popSize = 359
            }else{
              popSize = 360
            }

            variable.data <- dplyr::pull(obs_data, variable)

            big_combos = create_annual_combos(strata, popSize, year, variable.data, n.cores)

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
            best_individual <- c(year, best_individual, rep(NA, maxstrata-strata))
            best_individuals_all[nrow(best_individuals_all) + 1,] <-  best_individual

            #best individual mse for this strata
            best_individual_mses[strata,] <- c(strata, as.numeric(as.vector(lowest_individual_mses[1,])))

            #best strata mses for this strata
            add.rows <- cbind(rep(strata, strata), 1:strata, pop_strata_mses[[1]])
            names(add.rows)[1:2] <- c("strata", "strata.n")
            best_strata_mses <- dplyr::bind_rows(best_strata_mses, add.rows)

          }else{ #if strata > 2
            popSize = 1500

            variable.data <- dplyr::pull(obs_data, variable)

            big_combos = create_annual_combos(strata, popSize, year, variable.data, n.cores)

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
              best_individual <- c(year, best_individual, rep(NA, maxstrata-strata))
            }
            best_individuals_all[nrow(best_individuals_all) + 1,] <-  best_individual

            #best individual mse for this strata
            best_individual_mses[strata,] <- c(strata, as.numeric(as.vector(lowest_individual_mses[1,])))
            #best strata mses for this strata
            add.rows <- cbind(rep(strata, strata), 1:strata, old_strata_mses[[1]])
            names(add.rows)[1:2] <- c("strata", "strata.n")
            best_strata_mses <- dplyr::bind_rows(best_strata_mses, add.rows)

          }#end else
        }#end 2 strata
      }#strata

        one_individual_mse <- dplyr::slice_head(dplyr::arrange(best_individual_mses, MSE_kglenS), n = 1)

    }#end strings of NAs

      nstrata <- nrow(best_strata_mses)
      nstratabestindmses <- nrow(best_individual_mses)

      ##add data to the dataframes whether it's from the raw or crossover data
      individual_mses_all[nrow(individual_mses_all) + 1,] <- one_individual_mse #best_
      best_strata_mses_all[nrow(best_strata_mses_all) + nstrata,]
      best_strata_mses_all<- rbind(best_strata_mses_all, best_strata_mses[,c(1:12)]) #best_strata_mses
      ###Note: best_strata_mses_all will output NAs for any strata there were not enough data to run
      best_individual_mses_all[nrow(best_individual_mses_all) + nstratabestindmses,]
      best_individual_mses_all<- rbind(best_individual_mses_all, best_individual_mses) #best_individual_mses

      #create year output to build final output files
      ##Note: for outputs with more than one strata/more than one potential row per year, use out.year.mult
      out.year[nrow(out.year) +1,] <- year
      #build output years for best_strata_mses_all
      out.year.mult[nrow(out.year.mult) + nstrata,]
      input=data.frame(rep(year, times=nstrata))
      out.year.mult<- rbind(out.year.mult, input)
      #build output years for best ind mses
      out.year.mult.bim[nrow(out.year.mult.bim) + nstratabestindmses,]
      input.bim=data.frame(rep(year, times=nstratabestindmses))
      out.year.mult.bim<- rbind(out.year.mult.bim, input.bim)

  }#year

    #edit columns names for output
    names(out.year.mult) <- "year"
    names(out.year.mult.bim) <- "year"

    #add the year into the new output files
    individual_mses_all <- cbind(out.year, individual_mses_all)
    best_individual_mses_all <- cbind(out.year.mult.bim, best_individual_mses_all)
    best_strata_mses_all <- cbind(out.year.mult, best_strata_mses_all)

    #write file for each variable - output should have 4 total files with all of that variable's data
    filename = paste(parent_directory, "/", river, "/Output/Annual/", variable, "/", variable, "_best_strata_mses", ".csv", sep = '')
    write.csv(best_strata_mses_all, filename, row.names = FALSE)

    filename = paste(parent_directory, "/", river, "/Output/Annual/", variable, "/", variable, "_best_individual_mses", ".csv", sep = '')
    write.csv(best_individual_mses_all, filename, row.names = FALSE)

    filename = paste(parent_directory, "/", river, "/Output/Annual/", variable, "/", variable, "_best", ".csv", sep = '')
    write.csv(individual_mses_all, filename, row.names = FALSE)

    filename = paste(parent_directory, "/", river, "/Output/Annual/", variable, "/", variable, "_best_individuals", ".csv", sep = '')
    write.csv(best_individuals_all, filename, row.names = FALSE)

}#variable
}#river
}#function
