# rbeale
Welcome to the rbeale package! This package was built by team members at the National Center for Water Quality Research (NCWQR) in Tiffin, OH. The purpose of the rbeale package is to make it easy for reserachers to calculate nutrient loading in R, using a Beale Ratio Estimator.

## To Run This Package...
Install the package:
```
library(devtools)
install_github("ncwqr-jsmith29/rbeale")
library(rbeale)
```

## To Calculate Loads...
### 1. Prepare input files
This package was originally built to work with files downloaded from the [NCWQR data portal](https://ncwqr-data.org/HTLP/Portal). If you use downloads from the data portal, then you only need to separate the data into separate CSV files for each river.

If you are using your own data, then use the RiverSample.csv file as a template.

We recommend changing the names of your input files to the name of the river (i.e. Maumee.csv).

### 2. Put all input files in a single directory
Make sure you put all input files in a single folder; it doesn't matter where.

### 3. Run `create_folders()`
To create all folders needed for running subsequent functions, run the code below:
```
create_folders(parent_directory, input_rivers, variables = "ALL", input_directory, input_files)
```
Where...
`parent_directory` = The directory where you would like all output folders and input files to go. This becomes your working directory in subsequent functions.

`input_rivers` = The names of the rivers you would like to calculate loads for. This should either be a single string or a vector of strings. These names should match the file names of the input_files.

`variables` = The "ALL" option will calculate loads for TSS, TP, SRP, NO23, TKN, Cl, SO4, and Si

`input_directory` = The directory where all the input files are.

`input_files` = String or vector of strings containing the input file names. These names should match the names of the input_rivers.
### 4. Run `annual()`, `spring()`, `monthly()`, or `daily()`


# Conceptual Dcoumentation
NCWQR scientist, R. Peter Richards, wrote FORTRAN code to calculate nutrient loads using the Beale Ratio Estimator. That code was translated to R in 2016 by NCWQR scientist, Rem Confesor. That translation is what is contained in the `sbeale()` function of this package.

The `sbeale()` function calculates the load for a given timeframe (annual, spring, monthly), using all available data for that timeframe; any missing data is ignored. While you could apply the `sbeale()` function across the entirety of the dataset for a given time period, it could be more accurate to break the data into smaller chunks, calculate the load, and aggregate the chunks into an overall load for the time period. The most accurate way to break the data into chunks would be to separate the data into periods with similar load values.
