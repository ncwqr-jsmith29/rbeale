#' create_folders rbeale function
#'
#' Create input and output folders.
#' Imports:
#' @param parent_directory Directory where input files are and output files should go
#' @param input_rivers Vector of input rivers, river names as character objects, spelled the same as csv file
#' @param input_directory Directory where raw input files are
#' @param input_files csv input files
#' @keywords
#' @export
#' @examples
#' create_folders()

create_folders <- function(parent_directory, input_rivers, variables = "ALL", input_directory, input_files){
  if(length(input_rivers) != length(input_files)){
    stop("Input rivers and input files do not have the same length.")
  }

  if(variables == "ALL"){
    variables <- c("TSS", "TP", "SRP", "NO23", "TKN", "Cl", "SO4", "Si")
  }

  # Check that parent_directory exists
  if (!dir.exists(parent_directory)) {
    stop("The parent directory does not exist.")
  }

  setwd(input_directory)

  for(i in seq_along(input_rivers)){
    river <- input_rivers[i]
    input_file <- input_files[i]

    # Create river directory (e.g., ExampleData/Maumee)
    river_dir <- file.path(parent_directory, river) # Use file.path for consistent slashes
    if (!dir.exists(river_dir)) {
      message("Creating river directory: ", river_dir)
      dir.create(river_dir, recursive = TRUE)
    } else {
      message("River directory already exists: ", river_dir)
    }

    # Ensure the river folder was created successfully
    if (!dir.exists(river_dir)) {
      stop("Failed to create river directory: ", river_dir)
    }

    # Create Output directory for each river (e.g., ExampleData/Maumee/Output)
    base_dir <- file.path(river_dir, "Output")
    if (!dir.exists(base_dir)) {
      message("Creating base Output directory: ", base_dir)
      dir.create(base_dir, recursive = TRUE)
    } else {
      message("Output directory already exists: ", base_dir)
    }

    # Ensure the base directory was created successfully
    if (!dir.exists(base_dir)) {
      stop("Failed to create base Output directory: ", base_dir)
    }

    for(interval in c("Annual", "Monthly", "Daily", "Spring")){
      interval_dir <- file.path(base_dir, interval)

      # Create interval directories (e.g., ExampleData/Maumee/Output/Annual)
      if (!dir.exists(interval_dir)) {
        message("Creating interval directory: ", interval_dir)
        dir.create(interval_dir)
      } else {
        message("Interval directory already exists: ", interval_dir)
      }

      for(variable in variables){
        variable_dir <- file.path(interval_dir, variable)

        # Create variable subdirectories (e.g., ExampleData/Maumee/Output/Annual/TSS)
        if (!dir.exists(variable_dir)) {
          message("Creating variable directory: ", variable_dir)
          dir.create(variable_dir)
        } else {
          message("Variable directory already exists: ", variable_dir)
        }
      }
    }

    # Copy files to the river folder
    message("Copying file: ", input_file, " to ", river_dir)
    file.copy(from = input_file, to = river_dir)
  }
}
