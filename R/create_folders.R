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

  setwd(input_directory)

  for(river in input_rivers){
    for(variable in varialbes){
      dir.create(paste(parent_directory, river, "Output", "Annual", variable, sep = "/"))
    }
    file.copy(from = input_files, to = paste(parent_directory, river, sep = "/"))
  }
}
