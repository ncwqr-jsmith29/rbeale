#' Monthly rbeale function
#'
#' Calculate monthly loads.
#' Imports:
#' parallel (>= 4.0.5)
#' dplyr (>= 1.0.7)
#' lubridate (>= 1.8.0)
#' doParallel (>= 1.0.17)
#' @param parent_directory Directory where input files are and output files should go
#' @param input_rivers Vector of input rivers, river names as character objects, spelled the same as csv file
#' @keywords monthly rbeale
#' @export
#' @examples
#' monthly()

monthly <- function(parent_directory, input_rivers){
  print("Starting monthly...")

  #Before running, convert data portal excel files to csv

  rivers <- input_rivers

  for(river in rivers){

    data.portal.file <- read.csv(paste(parent_directory, "/", river, "/", river, ".csv", sep = ""))
    all_obs_data <- fix_monthly_input(data.portal.file)

    variables <- c("TSS", "TP", "SRP", "NO23", "TKN", "Cl", "SO4", "Si")
    yearmos <- unique(all_obs_data$YearMo)

    n.cores <- parallel::detectCores()

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
                                   s10 = numeric())



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
                                         s10 = numeric())
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
      out.year=data.frame(yearmo = numeric())
      out.year.mult = data.frame(yearmo = numeric())
      out.year.mult.bim = data.frame(yearmo = numeric())
      out.year.mult.bi = data.frame(yearmo = numeric())

      for(yearmo in yearmos){

        year <- as.numeric(substr(yearmo, 1, 4))
        mo <- as.numeric(substr(yearmo, 5, 6))
        if((lubridate::leap_year(year)) && (mo == 2)){
          last.allele <- 29
        }else{
          last.allele <- switch(mo, 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
        }

        obs_data <- dplyr::filter(all_obs_data, YearMo == yearmo)


        if(all(is.na(obs_data[,variable]))){
          print(paste("No data for", variable, "in", yearmo, sep = " "))
          filename = paste(parent_directory, "/", river, "/Output/Monthly/", variable, "/", variable, "_Error_", yearmo, ".txt", sep = '')
          writeLines(paste("No data for", variable, "in", yearmo, sep = " "), filename)
          next
        }else{
          if(sum(!is.na(obs_data[,variable])) < 3){
            print(paste("Not enough data for", variable, "in", yearmo, sep = " "))
            filename = paste(parent_directory, "/", river, "/Output/Monthly/", variable, "/", variable, "_Error_", yearmo, ".txt", sep = '')
            writeLines(paste("Not enough data for", variable, "in", yearmo, sep = " "), filename)
            next
          }
        }

        #max number of strata is determined by how many groups of 3 can be contained in the total number of days in the individual
        max_strata <- last.allele %/% 3
        try_strata = seq(1,max_strata)

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

          print(paste("Running sbeale for", variable, "...", yearmo, sep = " "))


          one_individual_mse<- c(0, strata_mses)#format for output files

        }else{

          for(strata in try_strata){
            print(paste("Processing", strata, "strata for", variable, "...", yearmo, sep = " "))
            if(strata == 1){
              popSize <- 1
              pop <- c(1,last.allele)
              strata_mses <- sbeale(obs_data$Flow, obs_data[, variable])
              strata_mses[7] <- strata_mses[7] * N^2
              best_strata_mses[1,] <- c(1, 1, strata_mses)
              best_individual_mses[1,] <- c(1, strata_mses)
            }else{#if strata >= 2

              big_combos = create_monthly_combos(strata, last.allele, n.cores)

              variable.data <- dplyr::pull(obs_data, variable)
              problem_individuals <- logical()
              parallel::clusterExport(cl, list("find_problem_individuals", "problem_individuals", "variable.data", "strata"), envir = environment())
              problem_individuals <- parallel::parRapply(cl, big_combos, find_problem_individuals)
              #eliminate problem individuals
              big_combos <- big_combos[!problem_individuals,]


              if(class(big_combos)[1] == "numeric"){
                #this means there is 1 individual, so the matrix is a vector
                #force it to be a matrix
                big_combos <- matrix(big_combos, nrow = 1)
              }

              popSize <- nrow(big_combos)

              #if there are no good individuals with 2 strata, then there won't be any for 3 or more,
              #so break out of strata for-loop and move on to next variable
              if(popSize == 0){
                print(paste("Not enough data for ", strata, " strata", sep = ""))
                break
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

              if(nrow(pop) != 1){
                pop <- pop[lowest_individual_indices,]
              }
              pop_strata_mses <- pop_strata_mses[lowest_individual_indices]

              best_individual <- as.numeric(as.vector(pop[1,]))
              best_individual <- c(best_individual, rep(NA, 12-strata-1))
              best_individuals[strata,] <- best_individual

              #best individual mse for this strata
              best_individual_mses[strata,] <- c(strata, as.numeric(as.vector(lowest_individual_mses[1,])))

              #best strata mses for this strata
              add.rows <- cbind(strata, strata, 1:strata, pop_strata_mses[[1]])
              names(add.rows)[1:2] <- c("strata", "strata.n")
              best_strata_mses <- dplyr::bind_rows(best_strata_mses, add.rows)

            }#end else
          }#strata

          one_individual_mse <- dplyr::slice_head(dplyr::arrange(best_individual_mses, MSE_kglenS), n = 1)


        }#strings of NA

        nstrata <- nrow(best_strata_mses)
        nstratabestind <- nrow(best_individuals)
        nstratabestindmses <- nrow(best_individual_mses)

        ##add data to the dataframes whether it's from the sbeale or crossover data
        individual_mses_all[nrow(individual_mses_all) + 1,] <- one_individual_mse #best_

        best_individuals_all[nrow(best_individuals_all) + nstratabestind,]
        best_individuals_all<- rbind(best_individuals_all, best_individuals) #best_individuals
        best_strata_mses_all[nrow(best_strata_mses_all) + nstrata,]
        best_strata_mses_all<- rbind(best_strata_mses_all, best_strata_mses[,c(1:12)]) #best_strata_mses
        ###Note: best_strata_mses_all will output NAs for any strata there were not enough data to run
        best_individual_mses_all[nrow(best_individual_mses_all) + nstratabestindmses,]
        best_individual_mses_all<- rbind(best_individual_mses_all, best_individual_mses) #best_individual_mses

        #create yearmo output to build final output files
        #for outputs with more than one strata/more than one potential row per yearmo, use out.year.mult
        out.year[nrow(out.year) +1,] <- yearmo
        #build output yearmos for best_strata_mses_all
        out.year.mult[nrow(out.year.mult) + nstrata,]
        input=data.frame(rep(yearmo, times=nstrata))
        out.year.mult<- rbind(out.year.mult, input)
        #build output yearmos for best ind mses
        out.year.mult.bim[nrow(out.year.mult.bim) + nstratabestindmses,]
        input.bim=data.frame(rep(yearmo, times=nstratabestindmses))
        out.year.mult.bim<- rbind(out.year.mult.bim, input.bim)
        #build output yearmos for best individuals
        out.year.mult.bi[nrow(out.year.mult.bi) + nstratabestind,]
        input.bi=data.frame(rep(yearmo, times=nstratabestind))
        out.year.mult.bi<- rbind(out.year.mult.bi, input.bi)


      }#yearmo

      #edit columns names for output
      names(out.year.mult) <- "yearmo"
      names(out.year.mult.bim) <- "yearmo"
      names(out.year.mult.bi) <- "yearmo"

      #add the yearmo into the new output files
      individual_mses_all <- cbind(out.year, individual_mses_all)
      best_individuals_all <- cbind(out.year.mult.bi, best_individuals_all)
      best_individual_mses_all <- cbind(out.year.mult.bim, best_individual_mses_all)
      best_strata_mses_all <- cbind(out.year.mult, best_strata_mses_all)

      #write file for each variable - output should have 4 total files with all of that variable's data
      filename = paste(parent_directory, "/", river, "/Output/Monthly/", variable, "/", variable, "_best_strata_mses", ".csv", sep = '')
      write.csv(best_strata_mses_all, filename, row.names = FALSE)

      filename = paste(parent_directory, "/", river, "/Output/Monthly/", variable, "/", variable, "_best_individual_mses", ".csv", sep = '')
      write.csv(best_individual_mses_all, filename, row.names = FALSE)

      filename = paste(parent_directory, "/", river, "/Output/Monthly/", variable, "/", variable, "_best", ".csv", sep = '')
      write.csv(individual_mses_all, filename, row.names = FALSE)

      filename = paste(parent_directory, "/", river, "/Output/Monthly/", variable, "/", variable, "_best_individuals", ".csv", sep = '')
      write.csv(best_individuals_all, filename, row.names = FALSE)

    }#variable
  }#river
}#function
