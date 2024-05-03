#' Fix daily data
#'
#' Prepare input data
#' Imports:
#' dplyr (>= 1.0.7)
#' lubridate (>= 1.8.0)
#' zoo (>= 1.8.9)
#' @param daily_input Input data as data frame. Input file HTLP data portal file, read in as csv.
#' @keywords fix daily data
#' @examples
#' fix_daily_data()

fix_daily_input <- function(daily_input){
  #Convert data from output from data portal to something that works for rbeale
  all_obs_data <- dplyr::select(daily_input, -contains("Qualifiers"))
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

  all_obs_data$Flow <- zoo::na.approx(all_obs_data$Flow, na.rm = F) #fill missing data with interpolation, does not fill leading or trailing NAs
  all_obs_data$TSS <- zoo::na.approx(all_obs_data$TSS, na.rm = F)
  all_obs_data$TP <- zoo::na.approx(all_obs_data$TP, na.rm = F)
  all_obs_data$SRP <- zoo::na.approx(all_obs_data$SRP, na.rm = F)
  all_obs_data$NO23 <- zoo::na.approx(all_obs_data$NO23, na.rm = F)
  all_obs_data$TKN <- zoo::na.approx(all_obs_data$TKN, na.rm = F)
  all_obs_data$Cl <- zoo::na.approx(all_obs_data$Cl, na.rm = F)
  all_obs_data$SO4 <- zoo::na.approx(all_obs_data$SO4, na.rm = F)
  all_obs_data$Si <- zoo::na.approx(all_obs_data$Si, na.rm = F)

  #all_obs_data <- all_obs_data %>% fill(Flow, .direction = "updown") #fill leading and trailing NAs

  return(all_obs_data)
}
