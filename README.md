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

## Origins
NCWQR scientist, R. Peter Richards, wrote FORTRAN code to calculate nutrient loads using the Beale Ratio Estimator. That code was translated to R in 2016 by NCWQR scientist, Rem Confesor. That translation is what is contained in the `sbeale()` function of this package. The `sbeale()` function calculates the load for a given timeframe, using all available data for that timeframe; any missing data is ignored.

## How the code works...
We will now demonstrate how the code works by using an example.

Let's say that we want to calculate the TP load for the Maumee River in 2013.

### Step 1: Deal with missing data
#### Scenario 1: No missing data
If there is no TP data missing from the Maumee in 2013, then we can simply run `sbeale()` to calculate the load and skip the rest of the steps below.

#### Scenario 2: Single days of missing data
If there are lone NAs (or single days of missing data, surrounded by a day of non-missing data on either side), then those NAs are replaced by the average of the surrounding TP values (i.e., take the average of the TP value of the day before and the day after). After these lone NAs filled, there are no more NAs and we can run `sbeale()` and skip the rest of the steps below.

#### Scenario 3: Strings of days of missing data
If there are two or more days in a row of missing data, then we must move on to Step 2.

The approach of this code is to fill in missing data by breaking the dataset into strata (chunks of data) and calculate an overall load for that strata. The goal is to set up the strata to where the data within each strata is as similar as possible. Any missing data in that strata is then "filled in" with the load calculated for that strata. The load for the entire timeframe is calculated by taking a sum of the load calculations for each strata.

### Step 2: Create Population
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

We create a population of individuals with the set-up above. There are far too many possible combinations of alleles, so we take a subset of the total number of possible individuals. This is done by `create_annual_combos()`. However, even if there are at least 3 days within each strata, that doesn't mean that those days have observed TP values. We have to eliminate those individuals that do not have at least 3 observations within each strata. Those "problem individuals" are found by `find_problem_individuals()` and are eliminated in the `annual()` code.

### Step 3: Calculate Strata Loads
Once we have a population of individuals that follow the rules above, we must calculate the load for each strata. To determine how well the calculated load represents the data within each strata, we calculate a mean square error (MSE). Both the load and the MSE of each strata is calculated by calling the `sbeale()` function inside the `calc_strata_mses()` function. Once we have calculated the MSEs for each strata in each individual, we start to enter into the genetic part of the code.

### Step 4: Crossover (Breeding)
The population of individuals is sent to the `crossover()` code so the individuals can "breed." Using our genetic language, two individuals pair up and swap their alleles to create 2 - 4 new individuals, or "children." Here's an example:

Individual 1 (Parent 1): 1_____50_____117_____260_____365

Individual 2 (Parent 2): 1_____103_____256_____302_____365

We then choose a random "change point." The changepoint cannot be the first or last allele, as this would make no new individuals. Our options are:

1. Change point == Allele #2
   
   Children are created by crossing alleles after the change point.
   
   Child 1: 1_____**50**_____256_____302_____365
   
   Child 2: 1_____**103**_____117_____260_____365
   
2. Changepoint == Allele #4 (or, in the general case, allele n-1)

   Children are created by crossing alleles before the change point.

   Child 1: 1_____103_____256_____**260**_____365

   Child 2: 1_____50_____117_____**302**_____365

3. Changepoint == Allele #3 (or, in the general case, any allele that is not 2 or n-1)

   Children are created by crossing both the alleles before the change point and the alleles after.

   Child 1: 1_____50_____**117**_____302_____365

   Child 2: 1_____103_____**256**_____260_____365

   Child 3: 1_____103_____**117**_____260_____365

   Child 4: 1_____50_____**256**_____302_____365

>[!NOTE]
>Notice that the change points don't actually change position; it's the alleles before or after the change point that change positions.

Some of the children created by `crossover()` may not follow the rules for individuals listed above. If the children aren't strictly increasing, then they are sorted. If the children don't contain at least 3 non-NA values, then they are eliminated. If the children are clones of any other individuals (other children or any parents), then they are eliminated. The remaining children become part of the overall population.

We can then check to see if any new strata are present in each individual. If so, then we calculate the laod and MSE for those strata using `calc_strata_mses()`.

### Step 5: Calculate Individual Loads
Although we have MSEs for each strata for each individual, we cannot compare strata, we must compare the entire individual. We calulate the total load and aggregate the strata MSEs into an individual MSE by running the `calc_individual_mse()` function.

### Step 6: Determine the Best Individuals
When we have individual MSEs, we use those to compare individuals to each other to see which individuals represent the data the best. The individuals are sorted by lowest MSE and only those with the lowest MSEs are kept. In the genetic sense, this would represent keeping those individuals that are "most fit."

### Step 7: Repeat
After the population is left with the best individuals, we begin the process again with another crossover (Step 4). We repeat this process a maximum of 35 times.

### Step 8: Output
After repeating the code, information for the best individual for Maumee TP data in 2013 is saved. Once all the years of interest are run, the output files for all the best individuals for each year for TP are created. There are 4 output files for each variable for each river. Those output files contain information for all the time periods run in the code.

### Other Considerations
The above example was for 4 strata. The code actually tests for 1-14 strata. Of course, for 1 strata, there is no need to create a population. For two strata, there is no need to crossover, as there are a very limited number of possible individuals. The genetic algorithm is only used for strata >= 3.

The above example was also for calculating annual loads. The process is very similar for spring, but different for monthly and daily. For monthly loads, the number of possible individuals is fairly small, as the largest allele is between 28-31, depending on the month. This also means that the largest number of strata you can test is 10 (Rule 3). As a result, instead of taking a subset of the possible individuals and performing crossovers, the `monthly()` code simply creates a population of all possible individuals and performs Steps 3, 5, and 6. For daily loads, the code simply runs `sbeale()`. The result is that daily loads are just flow * concentration.

If the dataset has no observations (all NAs), then the code produces an error output for that time period. If the dataset has too few observations to follow Rule 3, then the code produces a different error output for that time period.

# Flowchart
![Flow chart of rbeale code](https://github.com/ncwqr-jsmith29/rbeale/blob/master/rbeale.png)
