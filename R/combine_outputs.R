#' combine_outputs rbeale function
#'
#' Combines output folders into one .csv for each river and one .xlsx file containing all outputs for each river.
#' Imports:
#' openxlsx (>= 4.2.7.1)
#' @param parent_directory Directory where input files are and output files should go
#' @param input_rivers Vector of input rivers, river names as character objects, spelled the same as csv file
#' @param combine_as_excel Optional excel file containing all outputs for each river to be output if TRUE
#' @keywords
#' @export
#' @examples
#' combine_outputs()


combine_outputs <- function(parent_directory, rivers, variables = c("TSS", "TP", "SRP", "NO23", "TKN", "Si", "SO4", "Cl"), combine_as_excel = TRUE) {
  # Ensure required packages are installed and loaded
  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    stop("The 'openxlsx' package is required. Please install it with install.packages('openxlsx').")
  }

  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("The 'dplyr' package is required. Please install it with install.packages('dplyr').")
  }

  # List to store combined data for the final Excel file
  combined_data_list <- list()

  # Loop through each river directory
  for (river in rivers) {
    river_dir <- file.path(parent_directory, river, "Output")

    # Check if the river directory exists
    if (!dir.exists(river_dir)) {
      warning(paste("River directory", river_dir, "does not exist. Skipping."))
      next
    }

    # Loop through each timeframe (Annual, Monthly, Daily, Spring)
    for (timeframe in c("Annual", "Spring", "Monthly", "Daily")) {
      timeframe_dir <- file.path(river_dir, timeframe)

      # Check if the timeframe directory exists
      if (!dir.exists(timeframe_dir)) {
        message(paste("Timeframe directory", timeframe_dir, "does not exist. Skipping."))
        next
      }

      # Create a list to store combined data for this timeframe
      timeframe_combined_data <- list()

      # Combine data for each variable
      for (variable in variables) {
        variable_file <- file.path(timeframe_dir, variable, paste0(variable, "_best.csv"))

        # Check if the variable_best.csv exists in the variable directory
        if (file.exists(variable_file)) {
          # Read the CSV file
          variable_data <- read.csv(variable_file, stringsAsFactors = FALSE)

          # Add a column for the variable to track which data belongs to which variable
          variable_data$variable <- variable

          # Store the variable data for the current timeframe
          timeframe_combined_data[[variable]] <- variable_data
        } else {
          message(paste("No", paste0(variable, "_best.csv"), "found in", variable_file))
        }
      }

      # Combine all variable data for the current timeframe, handling mismatched columns
      if (length(timeframe_combined_data) > 0) {
        combined_timeframe_data <- dplyr::bind_rows(timeframe_combined_data) # Handles mismatched columns

        # Save the combined timeframe data as a CSV in the river's Output folder
        combined_timeframe_csv_path <- file.path(river_dir, paste0(river, "_", timeframe, "_Output.csv"))
        write.csv(combined_timeframe_data, combined_timeframe_csv_path, row.names = FALSE)
        message(paste("Combined data saved for", river, "in", combined_timeframe_csv_path))

        # Store the combined data for this timeframe in the combined_data_list for the Excel file
        combined_data_list[[paste0(river, "_", timeframe)]] <- combined_timeframe_data
      } else {
        message(paste("No data to combine for", river, "in the", timeframe, "timeframe."))
      }
    }
  }

  # Optionally combine all data into a master Excel file
  if (combine_as_excel && length(combined_data_list) > 0) {
    master_excel_path <- file.path(parent_directory, "combined_rbeale_river_output.xlsx")
    wb <- openxlsx::createWorkbook()

    # Add each river and timeframe combination as its own sheet
    for (sheet_name in names(combined_data_list)) {
      openxlsx::addWorksheet(wb, sheet_name)
      openxlsx::writeData(wb, sheet_name, combined_data_list[[sheet_name]])
    }

    openxlsx::saveWorkbook(wb, master_excel_path, overwrite = TRUE)
    message(paste("Combined data saved as an Excel file in", master_excel_path))
  }
}
