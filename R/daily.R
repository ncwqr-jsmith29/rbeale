#' Daily rbeale function
#'
#' Calculate daily loads. Doesn't use beale function.
#' Imports:
#' dplyr (>= 1.0.7)
#' @param parent_directory Directory where input files are and output files should go
#' @param input_rivers Vector of input rivers, river names as character objects, spelled the same as csv file
#' @keywords daily rbeale
#' @export
#' @examples
#' daily()

daily <- function(parent_directory, input_rivers){
  print("Starting daily...")
  
  #Before running, convert data portal excel files to csv
  
  rivers <- input_rivers
  
  for(river in rivers){
    
    data.portal.file <- read.csv(paste(parent_directory, "/", river, "/", river, ".csv", sep = ""))
    all_obs_data <- fix_daily_input(data.portal.file)
    
    variables <- c("TSS", "TP", "SRP", "NO23", "TKN", "Cl", "SO4", "Si")
    wat_years <- unique(all_obs_data$WatYr)
    
    options(warn = -1)
    
    #convert tkn to totn
    # flow <- all_obs_data$Flow
    # all_obs_data[,26] <- all_obs_data[,26]/flow/86.4*1000
    
    for(variable in variables){
      
      for(year in wat_years){
        
        obs_data <- dplyr::filter(all_obs_data, WatYr == year)
        
        if(all(is.na(obs_data[,variable]))){
          print(paste("No data for", variable, "in", year, sep = " "))
          filename = paste(parent_directory, "/", river, "/Output/Daily/", variable, "/", variable, "_Error_", year, ".txt", sep = '')
          writeLines(paste("No data for", variable, "in", year, sep = " "), filename)
          next
        }
        
        output <- data.frame(Date = character(),
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
        output_vars <- c("Date", "dload.bi_kgd", "dload.ub_kgd", "tload_kglenS", "degf", "lenObs", "lenStr", "MSE_kglenS", "Cl_kglenS", "flowmu_m3s", "flowav_m3s")
        
        for(i in 1:nrow(obs_data)){
          beale <- as.list(sbeale(obs_data$Flow[i], obs_data[i,variable][[1]]))
          beale <- unlist(append(beale, as.character(obs_data$Date[i]), after = 0))
          names(beale) <- output_vars
          output <- rbind.data.frame(output, beale)
          colnames(output) <- output_vars
        }
        
        filename = paste(parent_directory, "/", river, "/Output/Daily/", variable, "/", variable, "_", year, ".csv", sep = '')
        write.csv(output, filename, row.names = FALSE)
      }#year
    }#variable
  }#river
}#function