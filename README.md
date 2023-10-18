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
This package was originally built to work with files downloaded from the [NCWQR Data Portal](https://ncwqr-data.org/HTLP/Portal). If you use downloads from the data portal, then you only need to separate the data into separate CSV files for each river.

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


# Conceptual Documentation
> [!IMPORTANT]
> Keywords:
>
> Timeframe/Load Timeframe - Annual, spring, monthly, or daily
>
> Variables/Nutrient Variables - TSS, TP, SRP, NO23, TKN, Cl, SO4, Si
>
> Strata - A subset of the dataset of interest (e.g., TP values for Maumee in 2013)
>
> Allele - An integer representing the index of the beginning or end of a strata
>
> Individual - A vector of alleles
>
> Population - A matrix of individuals

NCWQR scientist, R. Peter Richards, wrote FORTRAN code to calculate nutrient loads using the Beale Ratio Estimator. That code was translated to R in 2016 by NCWQR scientist, Rem Confesor. That translation is what is contained in the `sbeale()` function of this package. The `sbeale()` function calculates the load for a given timeframe, using all available data for that timeframe; any missing data is ignored.

We will now demonstrate how the code works by using an example.

Let's say that we want to calculate the TP load for the Maumee River in 2013.

## Scenario 1: No missing data
If there is no TP data missing from the Maumee in 2013, then we can simply run `sbeale()` to calculate the load.

## Scenario 2: Single days of missing data
If there are lone NAs (or single days of missing data, surrounded by a day of non-missing data on either side), then those NAs are replaced by the average of the surrounding TP values (i.e., take the average of the TP value of the day before and the day after). After these lone NAs filled, there are no more NAs and we can run `sbeale()`.

## Scenario 3: Strings of days of missing data
If there are two or more days in a row of missing data, then further steps must be taken.

The approach of this code is to fill in missing data by breaking the dataset into strata (chunks of data) and calculate an overall load for that strata. The goal is to set up the strata to where the data within each strata is as similar as possible. Any missing data in that strata is then "filled in" with the load calculated for that strata. The load for the entire timeframe is calculated by taking a sum of the load calculations for each strata.

### Genetic Algorithm & MSE
As a demonstration, let's say we want to break the TP data for Maumee in 2013 into 4 strata.

How many different ways are there to break the data into 4 strata? Here are the rules the code follows:

1. The first strata must start with 1 (1 is the first allele for all individuals).
2. The last strata must end with 365 (365 is the last allele for all individuals, unless it's a leap year).
3. There must be at least 3 non-NA data points within each strata.
4. Strata include the left allele, but not the right. The only exception is the last strata.
5. Individuals must be strictly increasing.

These rules mean that each individual has the following setup:

1__________x__________y__________z__________365

> [!NOTE]
> Notice that for 4 strata, there are 5 alleles. For n strata there are n+1 alleles. According to rule 4, the strata would be 1 thru x-1, x thru y-1, y thru z-1, and z thru 365.

Following these rules, there are <sub>359</sub>C<sub>3</sub> possible individuals.
> x >= 4 (Rules 1 and 3)
> 
> z <= 363 (Rules 2, 3, and 4)
> 
> max(z-x) = 359
> 
> count(x,y,z) = 3
> 
> We use a combination and not a permutation because of Rule 5. An individual with alleles 1, 5, 256, 113, 365 is the same as 1, 5, 113, 256, 365 because the first individual would be sorted to become the second.



# Flowchart
![Flow chart of rbeale code](https://github.com/ncwqr-jsmith29/rbeale/blob/master/rbeale.png)
