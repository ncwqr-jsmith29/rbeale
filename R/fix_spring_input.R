#' Fix spring data
#'
#' Prepare input data
#' Imports:
#' dplyr (>= 1.0.7)
#' lubridate (>= 1.8.0)
#' zoo (>= 1.8.9)
#' tidyr (>= 1.1.3)
#' @param spring_input Input data as data frame. Input file HTLP data portal file, read in as csv.
#' @keywords fix spring data
#' @examples
#' fix_spring_input()

fix_spring_input <- function(spring_input){
  #Convert data from output from data portal to something that works for rbeale
  all_obs_data <- dplyr::select(spring_input, -contains("Qualifiers"))
  all_obs_data <- dplyr::rename(all_obs_data, Date = 1, Flow = 2, TSS = 3, TP = 4, SRP = 5, NO23 = 6, TKN = 7, Cl = 8, SO4 = 9, Si = 10, Cond = 11)
  all_obs_data <- dplyr::select(all_obs_data, -Cond)
  all_obs_data <- dplyr::mutate(all_obs_data, Date = as.Date(Date, format = "%m/%d/%Y %H:%M"))

  #Gets data prepared. Averages variables if there are multiple observations for a single day and fills in missing dates with NA.
  start.date <- all_obs_data$Date[1]
  if(lubridate::month(start.date) < 10){
    start.date <- as.Date(paste(lubridate::year(start.date)-1, "10", "01", sep = "-"))
  }else{
    start.date <- as.Date(paste(lubridate::year(start.date), "10", "01", sep = "-"))
  }
  end.date <- all_obs_data$Date[nrow(all_obs_data)]
  if(lubridate::month(end.date) < 10){
    end.date <- as.Date(paste(lubridate::year(end.date), "09", "30", sep = "-"))
  }else{
    end.date <- as.Date(paste(lubridate::year(end.date)+1, "09", "30", sep = "-"))
  }
  all.dates <- data.frame(Date = as.Date(seq.Date(start.date, end.date, by = "day")))


  all_obs_data <- dplyr::full_join(all_obs_data, all.dates)
  all_obs_data <- dplyr::arrange(all_obs_data, Date)
  all_obs_data <- dplyr::mutate(all_obs_data, YYYYMMDD = as.numeric(gsub("-", "", Date)), .before = Date)
  all_obs_data <- dplyr::mutate(all_obs_data, CalYr = as.numeric(format(Date, "%Y")), .after = Date)
  all_obs_data <- dplyr::mutate(all_obs_data, Mo = as.numeric(format(Date, "%m")), .after = CalYr)
  all_obs_data <- dplyr::mutate(all_obs_data, YearMo = as.numeric(format(Date, "%Y%m")), .after = Mo)
  all_obs_data <- dplyr::mutate(all_obs_data, WatYr = ifelse(Mo >= 10, CalYr+1, CalYr), .after = YearMo)
  all_obs_data <- dplyr::mutate_at(all_obs_data, c(7:15), as.numeric)
  all_obs_data <- dplyr::group_by(all_obs_data, Date)
  all_obs_data <- dplyr::summarise_if(all_obs_data, is.numeric, mean, na.rm = F)
  all_obs_data <- dplyr::mutate(all_obs_data, Flow = Flow * 0.0283168) #convert flow from cfs to cms

  all_obs_data$Flow <- zoo::na.approx(all_obs_data$Flow, na.rm = F) #fill missing flow with interpolation, does not fill leading or trailing NAs

  all_obs_data <- tidyr::fill(all_obs_data, Flow, .direction = "updown") #fill leading and trailing NAs

  ##If there's one NA alone, then we're going to average the sample before and after
  ##we're doing this across all numeric columns in the dataset to save both time and power.
  all_obs_data[,3:10] <- lapply(all_obs_data, function(z) {
    mtx <- cbind(c(NA, head(z, -1)), z, c(tail(z, -1), NA))
    mtx[is.na(mtx[,2]) & rowSums(is.na(mtx)) > 1,] <- NA
    out <- ifelse(is.na(mtx[,2]), rowMeans(mtx, na.rm = TRUE), mtx[,2])
    out[is.nan(out)] <- NA
    out
  })
  ##check the work - hard for large datasets
  #all_obs_data


  return(all_obs_data)
}
