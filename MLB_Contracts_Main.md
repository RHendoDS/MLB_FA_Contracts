MLB Free Agent Contract Regression
================
Ryan Henderson
2023-08-04

For this project, I am attempting to predict MLB Free Agent Contracts
from 2013-2023 using only the players’ stats leading up to the year they
signed their contract. To do this I will be using multiple different
linear regression models. One model to predict the Average Annual Value
(AAV) of the contract and one model to predict the length of the
contract. I will repeat those two models for batters, starting pitchers
and relief pitchers creating six models in total as each group has very
different desired statistical goals.

This project includes web scraping, data munging, model evaluation and
visualization in R and use regression modeling and feature selection in
Python. For the Python Code, along with the rest of my portfolio, please
visit my [Github Page.](https://RHendoDS.github.io)

**Project Libraries**

``` r
library(tidyverse)
library(dplyr)
library(data.table)
library(baseballr)
library(ggplot2)
library(RColorBrewer)
library(scales)
```

## Batter Preprocessing

**Scraping Batter Data**

For this project I will be using data scraped from
[FanGraphs](https://www.fangraphs.com/) using the
[baseballr](https://billpetti.github.io/baseballr/) package. The code
below will return all single season data for all batters who received at
least 100 plate appearances in a season from 2009 to 2020, so that I can
look at the previous four years of performance for any free agent.

``` r
fgraphBat <- fg_batter_leaders(x = 2009, y = 2022, 
                               qual = 100, ind = 1, 
                               exc_p = TRUE)
```

**Column Pruning**

The web scraper above returns over 250 columns, many of which are
redundant or statistics that are better interpreted through other
statistics. I will be using sequential feature selection to build my
linear models later, but manually removing columns will save hours of
unnecessary processing time.

``` r
# Remove redundant StatCast and Pitch Index Columns
fgraphBat <- fgraphBat %>%
  select(-ends_with("_(SC)"), -ends_with("_pi"))

# Remove pitch type data
fgraphBat <- fgraphBat %>%
  select(-starts_with("FBall"), -starts_with("FBv"), -starts_with("CT"),
         -starts_with("CB"), -starts_with("SL"), -starts_with("CH"), 
         -starts_with("SF"), -starts_with("KN"), -starts_with("XX"), 
         -starts_with("PO"))

# Remove columns from the fgraphBat that are redundant, better interpreted 
# through other stats, unlikely to be a consideration for modern baseball 
# operations, or not very correlated with contract values
fgraphBat <- fgraphBat %>%
  select(-playerid, -Age, -G, -AB, -H, -`1B`, -`2B`, -`3B`, -HR,
         -R, -RBI, -BB, -IBB, -SO, -HBP, -SH, -GDP, -SB, -CS, -AVG,
         -wFB, -wSL, -wCT, -wCB, -wCH, -wSF, -wKN, -Oppo_pct, -TTO_pct, 
         -Pace, -wFB_C, -wSL_C,  -wCT_C, -wCB_C, -wCH_C,  -wSF_C, -wKN_C, 
         -Zone_pct,-GB, -FB, -LD, -IFFB, -Pitches, -Balls, -Strikes, -IFH, -BU,
         -BUH, -OPS, -BABIP, -IFH_pct, -BUH_pct, -wRAA, -RAR, -Rep, -Dol, 
         -WPA_minus,-WPA_plus, -REW, -pLI, -phLI, -PH, -WPA_LI, -Pull_pct, 
         -Cent_pct, -AgeRng, -Lg, -wSB, -UBR, -wGDP)

# Get column names
colnames(fgraphBat)
```

    ##  [1] "#"             "Season"        "Name"          "Team"         
    ##  [5] "PA"            "BB_pct"        "K_pct"         "BB_K"         
    ##  [9] "OBP"           "ISO"           "GB_FB"         "LD_pct"       
    ## [13] "GB_pct"        "FB_pct"        "IFFB_pct"      "HR_FB"        
    ## [17] "wOBA"          "wRC"           "Bat"           "Fld"          
    ## [21] "WAR"           "Spd"           "wRC_plus"      "WPA"          
    ## [25] "RE24"          "Clutch"        "O-Swing_pct"   "Z-Swing_pct"  
    ## [29] "Swing_pct"     "O-Contact_pct" "Z-Contact_pct" "Contact_pct"  
    ## [33] "F-Strike_pct"  "SwStr_pct"     "BsR"           "Def"          
    ## [37] "Off"           "Soft_pct"      "Med_pct"       "Hard_pct"

**Adjusting for Covid Shortened 2020 Season**

My initial attempts at modeling contract showed a significant under
fitting of players who signed their contract after the 2020 season. The
main reason for this was their lower counting stats due to playing less
games. To account for this I multiplied all counting stats relative to
how many a player would have received if they held pace for a 162 game
season. This isn’t a perfect solution as it still leads outlier
performers that season to be over/undervalued, but it vastly improved
the overall model performance.

``` r
# Convert fgraphBat to a data table
fgraphBat <- as.data.table(fgraphBat)

# Adjust 2020 Counting Statistics for shortened season
factor_STL_DET <- 162/58
factor_other <- 162/60

# Multiply counting stat columns for 2020 based on Team values
fgraphBat <- within(fgraphBat, {
  PA <- ifelse(Season == 2020 & (Team == "STL" | Team == "DET"), PA * factor_STL_DET,
               ifelse(Season == 2020, PA * factor_other, PA))
  wRC <- ifelse(Season == 2020 & (Team == "STL" | Team == "DET"), wRC * factor_STL_DET,
                ifelse(Season == 2020, wRC * factor_other, wRC))
  Bat <- ifelse(Season == 2020 & (Team == "STL" | Team == "DET"), Bat * factor_STL_DET,
                ifelse(Season == 2020, Bat * factor_other, Bat))
  Fld <- ifelse(Season == 2020 & (Team == "STL" | Team == "DET"), Fld * factor_STL_DET,
                ifelse(Season == 2020, Fld * factor_other, Fld))
  WAR <- ifelse(Season == 2020 & (Team == "STL" | Team == "DET"), WAR * factor_STL_DET,
                ifelse(Season == 2020, WAR * factor_other, WAR))
  Spd <- ifelse(Season == 2020 & (Team == "STL" | Team == "DET"), Spd * factor_STL_DET,
                ifelse(Season == 2020, Spd * factor_other, Spd))
  WPA <- ifelse(Season == 2020 & (Team == "STL" | Team == "DET"), WPA * factor_STL_DET,
                ifelse(Season == 2020, WPA * factor_other, WPA))
  RE24 <- ifelse(Season == 2020 & (Team == "STL" | Team == "DET"), RE24 * factor_STL_DET,
                 ifelse(Season == 2020, RE24 * factor_other, RE24))
  BsR <- ifelse(Season == 2020 & (Team == "STL" | Team == "DET"), BsR * factor_STL_DET,
                ifelse(Season == 2020, BsR * factor_other, BsR))
  Def <- ifelse(Season == 2020 & (Team == "STL" | Team == "DET"), Def * factor_STL_DET,
                ifelse(Season == 2020, Def * factor_other, Def))
  Off <- ifelse(Season == 2020 & (Team == "STL" | Team == "DET"), Off * factor_STL_DET,
                ifelse(Season == 2020, Off * factor_other, Off))
})
```

**Importing Free Agent Contract Data**

My next step is to import the free agent contract information. I will be
using a [Google
Sheet](https://docs.google.com/spreadsheets/d/1bXUPBabVf82y0m2KaZ0F9Fno9xwZ2pmepbFvMBX_TEM/)
built and maintained by [Jeff
Euston](https://twitter.com/JeffEuston?s=20) with contract data going
all the way back to 1991. I will store each season as its own data frame
to make data munging easier later.

``` r
# Create an empty list to store the data frames
fa_signing_list <- list()

# Loop through the years 2013 to 2023
for (year in 2013:2023) {
  # Set the file path
  file_path <- file.path("~", "MLBFreeAgentSignings",
                         paste("MLBFreeAgency", year, ".csv", sep=""))
  
  # Import the CSV file and store it in a data frame
  fa_signing_list[[paste0("fa", year)]] <- fread(file_path)
}

# Keep only the desired columns
# Loop through the years 2013 to 2023
for (year in 2013:2023) {
  # Convert the fa_signing_list[[paste0("fa", year)]] to a data frame
  fa_df <- as.data.frame(fa_signing_list[[paste0("fa", year)]])
  
  # Select only the Player, Pos'n, Age, Years, and AAV columns
  fa_df <- fa_df[, c(1, 2, 3, 7, 12)]
  
  # Assign the modified data frame back to fa_signing_list
  fa_signing_list[[paste0("fa", year)]] <- fa_df
}
```

**Combining Player Stats and Contract Data**

I need to change my contract data frames from listing player names in
the form LastName, Firstname to Firstname LastName so that it matches my
baseball stats data frame.

``` r
for (year in 2013:2023) {
  # Extract the "Player" column from the original data frame
  player_column <- fa_signing_list[[paste0("fa", year)]]$Player
  
  # Split the names into first and last name
  names_split <- strsplit(player_column, ", ")
  
  # Rearrange the names to "First Name Last Name"
  names_rearranged <- sapply(names_split, function(x) paste(rev(x), 
                                                            collapse = " "))
  
  # Update the "Player" column with the rearranged names
  fa_signing_list[[paste0("fa", year)]]$Player <- names_rearranged
}
```

Next, I needed to merge the contract information and baseball
statistics. In my testing I found that taking a 4 year average of my
selected statistics to be the strongest way of building a model.

``` r
# Create an empty list to store the data frames
faStats_list <- list()

# Columns to calculate the mean
columns_to_mean <- c("PA", "BB_pct", "K_pct", "BB_K", "OBP", "ISO", "GB_FB", 
                     "LD_pct", "GB_pct", "FB_pct", "IFFB_pct","HR_FB", "wOBA", 
                     "wRC", "Bat", "Fld", "WAR", "Spd", "wRC_plus", "WPA", 
                     "RE24", "Clutch", "O-Swing_pct","Z-Swing_pct", "Swing_pct",
                     "O-Contact_pct", "Z-Contact_pct", "Contact_pct", 
                     "F-Strike_pct", "SwStr_pct", "BsR", "Def", "Off", 
                     "Soft_pct", "Med_pct", "Hard_pct")

# Loop through the years 2013 to 2023
for (year in 2013:2023) {
  # Select the most recent 4 seasons before Free Agency
  selected_years <- (year - 4):(year - 1)
  
  # Select rows from fgraphBat with the relevant names and years
  faStats <- fgraphBat[fgraphBat$Name %in% fa_signing_list[[paste0("fa", year)]]$Player &
                         fgraphBat$Season %in% selected_years, ]
  
  # Group rows by "Name" and calculate the mean of desired columns
  faStats <- faStats[, lapply(.SD, mean), by = Name, .SDcols = columns_to_mean]
  
  # Assign the data frame to the list
  faStats_list[[year]] <- faStats
}
```

**Adjusting for Contract Inflation**

MLB Contracts values have inflated overtime. To adjust for this I
multiplied all contract AAVs by the ratio of total payroll of the season
prior to the contract being signed to 2022’s total payroll. The raw data
can be found on [The Baseball
Cube](https://www.thebaseballcube.com/content/payroll/)

``` r
# Set the file path
file_path <- file.path("~", "MLBFreeAgentSignings", "MLBPayroll.csv")

# Read the CSV file
payroll_data <- read.csv(file_path)

# Filter the payroll data for the years 2012 to 2022 and include the "year" and 
# "total.payroll" columns
total_payroll_data <- payroll_data[payroll_data$year %in% 2012:2022, c("year", 
                                                                       "total.payroll")]

# Convert "total.payroll" to numeric by removing commas
total_payroll_data$total.payroll <- as.numeric(gsub(",", "", 
                                              total_payroll_data$total.payroll))

# Loop through the years 2013 to 2023
for (year in 2013:2023) {
  # Merge faStats_list[[year]] with fa_signing_list[[paste0("fa", year)]]
  merged_SP <- merge(faStats_list[[year]], 
                     fa_signing_list[[paste0("fa", year)]], 
                     by.x = "Name", by.y = "Player")
  
  # Convert the "AAV" column to numerical format
  merged_SP$AAV <- as.numeric(gsub("[$,]", "", merged_SP$AAV))
  
  # Assign the merged data frame back to faStats_list
  faStats_list[[year]] <- merged_SP
}

# Adjust AAV values in each table of faStats_list
for (year in 2013:2022) {
  # Get the previous year
  prev_year <- year - 1
  
  # Extract the AAV values from the previous year's payroll data
  prev_year_total_payroll <- total_payroll_data[total_payroll_data$year == prev_year, "total.payroll"]
  
  # Extract the AAV column from the corresponding table in faStats_list
  aav_column <- faStats_list[[year]]$AAV
  
  # Calculate the adjustment factor based on the total payroll of 2022 and the previous year's total payroll
  adjustment_factor <- total_payroll_data[total_payroll_data$year == 2022, "total.payroll"] / prev_year_total_payroll
  
  # Adjust the AAV values by multiplying with the adjustment factor
  faStats_list[[year]]$AAV <- aav_column * adjustment_factor
}
```

**Merging Contract Data and Performing Final Adjustments**

``` r
# Make one big data table
# Create an empty data table to store the merged data
merged_faStats <- data.table()

# Loop through the years 2013 to 2023
for (year in 2013:2023) {
  # Append the data from each year to the merged_faStats data table
  merged_faStats <- rbind(merged_faStats, faStats_list[[year]], fill=TRUE)
}
```

Starting in 2021 my contract data showed Age as a float instead of an
integer, as well as changed the name of the column. So I floored the Age
and merged them into one column.

``` r
merged_faStats$Age <- ifelse(is.na(merged_faStats$`Age 7/1/21`), 
                             merged_faStats$Age, 
                             floor(as.numeric(merged_faStats$`Age 7/1/21`)))
merged_faStats$Age <- ifelse(is.na(merged_faStats$`Age 7/1/22`), 
                             merged_faStats$Age, 
                             floor(as.numeric(merged_faStats$`Age 7/1/22`)))
merged_faStats$Age <- ifelse(is.na(merged_faStats$`Age 7/1/23`), 
                             merged_faStats$Age, 
                             floor(as.numeric(merged_faStats$`Age 7/1/23`)))

# Remove last three columns from merged_faStats
merged_faStats <- merged_faStats[, 1:(ncol(merged_faStats) - 3)]
```

Some players are listed at multiple positions, so I changed the format
of the position column to only consider a players primary position to
reduce the number of One Hot encoded columns in my model later.

``` r
# Rename 'Pos'n' column to 'position'
colnames(merged_faStats)[colnames(merged_faStats) == "Pos'n"] <- "Position"

# Remove text after a hyphen in the 'position' column
merged_faStats$Position <- sub("\\s*-.*$", "", merged_faStats$Position)
```

Lastly, I exported the data frame as a csv to build my models in Python

``` r
write.csv(merged_faStats, file = "Batters_Training_Data.csv", row.names = FALSE)
```

## Starting Pitcher Preprocessing

Next I need to repeat the same steps for starting pitchers. I will be
repeating most of the steps done except for a few things that don’t need
to be repeated.

``` r
# Scrape Fangraphs Pitching Leaders 2009-2022
# 40 Minimum IP, SP Only, Single Seasons
fgraphSP <- fg_pitcher_leaders(x = 2009, y = 2022, league = "all",
                               qual = 40,   pitcher_type = "sta",
                               ind = 1)

# Remove redundant StatCast and Pitch Index Columns
fgraphSP <- fgraphSP %>%
  select(-ends_with("_(SC)"), -ends_with("_pi"))

# Remove pitch info data
fgraphSP <- fgraphSP %>%
  select(-starts_with("FBall"), -starts_with("FBv"), -starts_with("CT"),
         -starts_with("CB"), -starts_with("SL"), -starts_with("CH"), 
         -starts_with("SF"), -starts_with("KN"), -starts_with("XX"), 
         -starts_with("PO"))

# Remove columns from the fgraphSP that are redundant, better interpreted 
# through other stats, unlikely to be a consideration for modern baseball 
# operations, or not very correlated with contract values
fgraphSP <- fgraphSP %>%
  select(-playerid, -"#", -"G", -"SV", -"BS", -"Relieving", -"Relief_IP", 
         -"RAR", -"Dollars", -"tERA", -"HLD", -"Pace", -"AgeRng", -"kwERA", 
         -"Dol", -"H", -"R", -"ER", -"HR", -"BB", -"IBB", -"HBP", -"WP", 
         -"BK", -"SO", -"GB", -"FB", -"LD", -"IFFB", -"IFH", -"BU", -"BUH", 
         -"BABIP", -"SD", -"MD", -"Start_IP")

# Get column names
colnames(fgraphSP)
```

    ##  [1] "Season"        "Name"          "Team"          "Age"          
    ##  [5] "W"             "L"             "ERA"           "GS"           
    ##  [9] "CG"            "ShO"           "IP"            "TBF"          
    ## [13] "Balls"         "Strikes"       "Pitches"       "RS"           
    ## [17] "K_9"           "BB_9"          "K_BB"          "H_9"          
    ## [21] "HR_9"          "AVG"           "WHIP"          "LOB_pct"      
    ## [25] "FIP"           "GB_FB"         "LD_pct"        "GB_pct"       
    ## [29] "FB_pct"        "IFFB_pct"      "HR_FB"         "IFH_pct"      
    ## [33] "BUH_pct"       "Starting"      "WAR"           "xFIP"         
    ## [37] "WPA"           "WPA_minus"     "WPA_plus"      "RE24"         
    ## [41] "REW"           "pLI"           "inLI"          "gmLI"         
    ## [45] "exLI"          "Pulls"         "WPA_LI"        "Clutch"       
    ## [49] "wFB"           "wSL"           "wCT"           "wCB"          
    ## [53] "wCH"           "wSF"           "wKN"           "wFB_C"        
    ## [57] "wSL_C"         "wCT_C"         "wCB_C"         "wCH_C"        
    ## [61] "wSF_C"         "wKN_C"         "O-Swing_pct"   "Z-Swing_pct"  
    ## [65] "Swing_pct"     "O-Contact_pct" "Z-Contact_pct" "Contact_pct"  
    ## [69] "Zone_pct"      "F-Strike_pct"  "SwStr_pct"     "ERA-"         
    ## [73] "FIP-"          "xFIP-"         "K_pct"         "BB_pct"       
    ## [77] "SIERA"         "RS_9"          "E-F"           "RA9-WAR"      
    ## [81] "BIP-Wins"      "LOB-Wins"      "FDP-Wins"      "K-BB_pct"     
    ## [85] "Pull_pct"      "Cent_pct"      "Oppo_pct"      "Soft_pct"     
    ## [89] "Med_pct"       "Hard_pct"      "TTO_pct"

``` r
# Convert fgraphSP to a data table
fgraphSP <- as.data.table(fgraphSP)

# Adjust 2020 Statistics for shortened season
factor_STL_DET <- 162/58
factor_other <- 162/60

#Adjust Counting Stats for 2020 season
fgraphSP <- within(fgraphSP,{
  W <- ifelse(Season == 2020 & (Team == "STL" | Team == "DET"), W * factor_STL_DET,
              ifelse(Season == 2020, W * factor_other, W))
  L <- ifelse(Season == 2020 & (Team == "STL" | Team == "DET"), L * factor_STL_DET,
              ifelse(Season == 2020, L * factor_other, L))
  GS <- ifelse(Season == 2020 & (Team == "STL" | Team == "DET"), GS * factor_STL_DET,
               ifelse(Season == 2020, GS * factor_other, GS))
  CG <- ifelse(Season == 2020 & (Team == "STL" | Team == "DET"), CG * factor_STL_DET,
               ifelse(Season == 2020, CG * factor_other, CG))
  ShO <- ifelse(Season == 2020 & (Team == "STL" | Team == "DET"), ShO * factor_STL_DET,
                ifelse(Season == 2020, ShO * factor_other, ShO))
  IP <- ifelse(Season == 2020 & (Team == "STL" | Team == "DET"), IP * factor_STL_DET,
               ifelse(Season == 2020, IP * factor_other, IP))
  TBF <- ifelse(Season == 2020 & (Team == "STL" | Team == "DET"), TBF * factor_STL_DET,
                ifelse(Season == 2020, TBF * factor_other, TBF))
  Balls <- ifelse(Season == 2020 & (Team == "STL" | Team == "DET"), Balls * factor_STL_DET,
                  ifelse(Season == 2020, Balls * factor_other, Balls))
  Strikes <- ifelse(Season == 2020 & (Team == "STL" | Team == "DET"), Strikes * factor_STL_DET,
                    ifelse(Season == 2020, Strikes * factor_other, Strikes))
  Pitches <- ifelse(Season == 2020 & (Team == "STL" | Team == "DET"), Pitches * factor_STL_DET,
                    ifelse(Season == 2020, Pitches * factor_other, Pitches))
  RS <- ifelse(Season == 2020 & (Team == "STL" | Team == "DET"), RS * factor_STL_DET,
               ifelse(Season == 2020, RS * factor_other, RS))
  WAR <- ifelse(Season == 2020 & (Team == "STL" | Team == "DET"), WAR * factor_STL_DET,
                ifelse(Season == 2020, WAR * factor_other, WAR))
  WPA <- ifelse(Season == 2020 & (Team == "STL" | Team == "DET"), WPA * factor_STL_DET,
                ifelse(Season == 2020, WPA * factor_other, WPA))
  WPA_minus <- ifelse(Season == 2020 & (Team == "STL" | Team == "DET"), WPA_minus * factor_STL_DET,
                      ifelse(Season == 2020, WPA_minus * factor_other, WPA_minus))
  WPA_plus <- ifelse(Season == 2020 & (Team == "STL" | Team == "DET"), WPA_plus * factor_STL_DET,
                     ifelse(Season == 2020, WPA_plus * factor_other, WPA_plus))
  RE24 <- ifelse(Season == 2020 & (Team == "STL" | Team == "DET"), RE24 * factor_STL_DET,
                 ifelse(Season == 2020, RE24 * factor_other, RE24))
  REW <- ifelse(Season == 2020 & (Team == "STL" | Team == "DET"), REW * factor_STL_DET,
                ifelse(Season == 2020, REW * factor_other, REW))
  wFB <- ifelse(Season == 2020 & (Team == "STL" | Team == "DET"), wFB * factor_STL_DET,
                ifelse(Season == 2020, wFB * factor_other, wFB))
  wSL <- ifelse(Season == 2020 & (Team == "STL" | Team == "DET"), wSL * factor_STL_DET,
                ifelse(Season == 2020, wSL * factor_other, wSL))
  wCT <- ifelse(Season == 2020 & (Team == "STL" | Team == "DET"), wCT * factor_STL_DET,
                ifelse(Season == 2020, wCT * factor_other, wCT))
  wCB <- ifelse(Season == 2020 & (Team == "STL" | Team == "DET"), wCB * factor_STL_DET,
                ifelse(Season == 2020, wCB * factor_other, wCB))
  wCH <- ifelse(Season == 2020 & (Team == "STL" | Team == "DET"), wCH * factor_STL_DET,
                ifelse(Season == 2020, wCH * factor_other, wCH))
  wSF <- ifelse(Season == 2020 & (Team == "STL" | Team == "DET"), wSF * factor_STL_DET,
                ifelse(Season == 2020, wSF * factor_other, wSF))
  wKN <- ifelse(Season == 2020 & (Team == "STL" | Team == "DET"), wKN * factor_STL_DET,
                ifelse(Season == 2020, wKN * factor_other, wKN))
  `RA9-WAR` <- ifelse(Season == 2020 & (Team == "STL" | Team == "DET"), `RA9-WAR` * factor_STL_DET,
                      ifelse(Season == 2020, `RA9-WAR` * factor_other, `RA9-WAR`))
  `BIP-Wins` <- ifelse(Season == 2020 & (Team == "STL" | Team == "DET"), `BIP-Wins` * factor_STL_DET,
                       ifelse(Season == 2020, `BIP-Wins` * factor_other, `BIP-Wins`))
  `LOB-Wins` <- ifelse(Season == 2020 & (Team == "STL" | Team == "DET"), `LOB-Wins` * factor_STL_DET,
                       ifelse(Season == 2020, `LOB-Wins` * factor_other, `LOB-Wins`))
  `FDP-Wins` <- ifelse(Season == 2020 & (Team == "STL" | Team == "DET"), `FDP-Wins` * factor_STL_DET,
                       ifelse(Season == 2020, `FDP-Wins` * factor_other, `FDP-Wins`))
})

# Select rows from fgraphSP of free agents for the four seasons prior to their deal and find the mean of the relevant columns
# Create an empty list to store the data frames
SP_Stats_list <- list()

# Define the columns to calculate the mean
columns_to_mean <- c("W", "L", "ERA", "GS", "CG", "ShO", "IP", "TBF", "Balls", "Strikes", "Pitches", "RS", "K_9", "BB_9",
                     "K_BB", "H_9", "HR_9", "AVG", "WHIP", "LOB_pct", "FIP", "GB_FB", "LD_pct", "GB_pct", "FB_pct", 
                     "IFFB_pct", "HR_FB", "IFH_pct", "BUH_pct", "WAR", "xFIP", "WPA", "WPA_minus", "WPA_plus", "RE24",
                     "REW", "pLI", "inLI", "gmLI", "exLI", "wFB", "wSL", "wCT", "wCB", "wCH", "wSF", "wKN", "O-Swing_pct",
                     "Z-Swing_pct", "Swing_pct", "O-Contact_pct", "Z-Contact_pct", "Contact_pct", "Zone_pct", 
                     "F-Strike_pct", "SwStr_pct", "ERA-", "FIP-", "xFIP-", "K_pct", "BB_pct", "SIERA", "RS_9", "E-F",
                     "RA9-WAR", "BIP-Wins", "LOB-Wins", "FDP-Wins", "K-BB_pct", "Pull_pct", "Cent_pct", "Oppo_pct",
                     "Soft_pct", "Med_pct", "Hard_pct", "TTO_pct")

# Loop through the years 2013 to 2023
for (year in 2013:2023) {
  # Select the most recent 4 seasons before Free Agency
  selected_years <- (year - 4):(year - 1)
  
  # Select rows from fgraphSP with the relevant names and years
  faStats <- fgraphSP[fgraphSP$Name %in% fa_signing_list[[paste0("fa", year)]]$Player &
                        fgraphSP$Season %in% selected_years, ]
  
  # Group rows by "Name" and calculate the mean of desired columns
  faStats <- faStats[, lapply(.SD, mean), by = Name, .SDcols = columns_to_mean]
  
  # Store the column means in the SP_Stats_list
  SP_Stats_list[[as.character(year)]] <- faStats
}

# View the list of column means for each year
print(fa_signing_list[2014])
```

    ## $<NA>
    ## NULL

``` r
# Loop through the years 2013 to 2023
for (year in 2013:2023) {
  # Merge SP_Stats_list[[year]] with fa_signing_list[[paste0("fa", year)]]
  merged_SP <- merge(SP_Stats_list[[as.character(year)]], 
                     fa_signing_list[[paste0("fa", year)]], 
                     by.x = "Name", by.y = "Player")
  
  # Convert the "AAV" column to numerical format
  merged_SP$AAV <- as.numeric(gsub("[$,]", "", merged_SP$AAV))
  
  # Assign the merged data frame back to SP_Stats_list
  SP_Stats_list[[as.character(year)]] <- merged_SP
}

# Adjust AAV values in each table of SP_Stats_list
for (year in 2013:2022) {
  # Get the previous year
  prev_year <- year - 1
  
  # Extract the AAV values from the previous year's payroll data
  prev_year_total_payroll <- total_payroll_data[total_payroll_data$year == prev_year, "total.payroll"]
  
  # Extract the AAV column from the corresponding table in SP_Stats_list
  aav_column <- SP_Stats_list[[as.character(year)]]$AAV
  
  # Calculate the adjustment factor based on the total payroll of 2022 and the previous year's total payroll
  adjustment_factor <- total_payroll_data[total_payroll_data$year == 2022, "total.payroll"] / prev_year_total_payroll
  
  # Adjust the AAV values by multiplying with the adjustment factor
  SP_Stats_list[[as.character(year)]]$AAV <- aav_column * adjustment_factor
}

# Make one big data table
# Create an empty data table to store the merged data
merged_SP_Stats <- data.table()

# Loop through the years 2013 to 2023
for (year in 2013:2023) {
  # Append the data from each year to the merged_SP_Stats data table
  merged_SP_Stats <- rbind(merged_SP_Stats, SP_Stats_list[[as.character(year)]], fill=TRUE)
}

# Floor the values in columns 'Age 7/1/21', 'Age 7/1/22', and 'Age 7/1/23' so
# that it is consistent with the rest of the data
merged_SP_Stats$Age <- ifelse(is.na(merged_SP_Stats$`Age 7/1/21`), 
                              merged_SP_Stats$Age, 
                              floor(as.numeric(merged_SP_Stats$`Age 7/1/21`)))
merged_SP_Stats$Age <- ifelse(is.na(merged_SP_Stats$`Age 7/1/22`), 
                              merged_SP_Stats$Age, 
                              floor(as.numeric(merged_SP_Stats$`Age 7/1/22`)))
merged_SP_Stats$Age <- ifelse(is.na(merged_SP_Stats$`Age 7/1/23`), 
                              merged_SP_Stats$Age, 
                              floor(as.numeric(merged_SP_Stats$`Age 7/1/23`)))

# Remove last three columns from merged_SP_Stats
merged_SP_Stats <- merged_SP_Stats[, 1:(ncol(merged_SP_Stats) - 3)]

# Rename 'Pos'n' column to 'position'
colnames(merged_SP_Stats)[colnames(merged_SP_Stats) == "Pos'n"] <- "Position"

# Export merged_SP_Stats to CSV
write.csv(merged_SP_Stats, file = "SP_Training_Data.csv", row.names = FALSE)
```

## Relief Pitcher Preprocessing

Now I will repeat the same steps for relief pitchers.

``` r
# Scrape Fangraphs Pitching Leaders 2009-2022
# 10 Minimum IP, RP Only, Single Seasons
fgraphRP <- fg_pitcher_leaders(x = 2009, y = 2022, league = "all",
                               qual = 10,   pitcher_type = "rel",
                               ind = 1)

# Remove redundant StatCast and Pitch Index Columns
fgraphRP <- fgraphRP %>%
  select(-ends_with("_(SC)"), -ends_with("_pi"))

# Remove pitch info data
fgraphRP <- fgraphRP %>%
  select(-starts_with("FBall"), -starts_with("FBv"), -starts_with("CT"),
         -starts_with("CB"), -starts_with("SL"), -starts_with("CH"), 
         -starts_with("SF"), -starts_with("KN"), -starts_with("XX"), 
         -starts_with("PO"))

# Remove columns from the fgraphRP that are redundant, better interpreted 
# through other stats, unlikely to be a consideration for modern baseball 
# operations, or not very correlated with contract values
fgraphRP <- fgraphRP %>%
  select(-playerid, -"#", -"G", -"GS", -"CG", -"Relieving", -"Relief_IP", 
         -"RAR", -"Dollars", -"tERA", -"HLD", -"Pace", -"AgeRng", -"kwERA", 
         -"Dol", -"H", -"R", -"ER", -"HR", -"BB", -"IBB", -"HBP", -"WP", 
         -"BK", -"SO", -"GB", -"FB", -"LD", -"IFFB", -"IFH", -"BU", -"BUH", 
         -"BABIP", -"SD", -"MD", -"Start_IP")

# Get column names
colnames(fgraphRP)
```

    ##  [1] "Season"        "Name"          "Team"          "Age"          
    ##  [5] "W"             "L"             "ERA"           "ShO"          
    ##  [9] "SV"            "BS"            "IP"            "TBF"          
    ## [13] "Balls"         "Strikes"       "Pitches"       "RS"           
    ## [17] "K_9"           "BB_9"          "K_BB"          "H_9"          
    ## [21] "HR_9"          "AVG"           "WHIP"          "LOB_pct"      
    ## [25] "FIP"           "GB_FB"         "LD_pct"        "GB_pct"       
    ## [29] "FB_pct"        "IFFB_pct"      "HR_FB"         "IFH_pct"      
    ## [33] "BUH_pct"       "Starting"      "WAR"           "xFIP"         
    ## [37] "WPA"           "WPA_minus"     "WPA_plus"      "RE24"         
    ## [41] "REW"           "pLI"           "inLI"          "gmLI"         
    ## [45] "exLI"          "Pulls"         "WPA_LI"        "Clutch"       
    ## [49] "wFB"           "wSL"           "wCT"           "wCB"          
    ## [53] "wCH"           "wSF"           "wKN"           "wFB_C"        
    ## [57] "wSL_C"         "wCT_C"         "wCB_C"         "wCH_C"        
    ## [61] "wSF_C"         "wKN_C"         "O-Swing_pct"   "Z-Swing_pct"  
    ## [65] "Swing_pct"     "O-Contact_pct" "Z-Contact_pct" "Contact_pct"  
    ## [69] "Zone_pct"      "F-Strike_pct"  "SwStr_pct"     "ERA-"         
    ## [73] "FIP-"          "xFIP-"         "K_pct"         "BB_pct"       
    ## [77] "SIERA"         "RS_9"          "E-F"           "RA9-WAR"      
    ## [81] "BIP-Wins"      "LOB-Wins"      "FDP-Wins"      "K-BB_pct"     
    ## [85] "Pull_pct"      "Cent_pct"      "Oppo_pct"      "Soft_pct"     
    ## [89] "Med_pct"       "Hard_pct"      "TTO_pct"

``` r
# Convert fgraphRP to a data table
fgraphRP <- as.data.table(fgraphRP)

# Adjust 2020 Statistics for shortened season
factor_STL_DET <- 162/58
factor_other <- 162/60

#Adjust Counting Stats for 2020 season
fgraphRP <- within(fgraphRP,{
  W <- ifelse(Season == 2020 & (Team == "STL" | Team == "DET"), W * factor_STL_DET,
              ifelse(Season == 2020, W * factor_other, W))
  L <- ifelse(Season == 2020 & (Team == "STL" | Team == "DET"), L * factor_STL_DET,
              ifelse(Season == 2020, L * factor_other, L))
  SV <- ifelse(Season == 2020 & (Team == "STL" | Team == "DET"), SV * factor_STL_DET,
               ifelse(Season == 2020, SV * factor_other, SV))
  BS <- ifelse(Season == 2020 & (Team == "STL" | Team == "DET"), BS * factor_STL_DET,
               ifelse(Season == 2020, BS * factor_other, BS))
  ShO <- ifelse(Season == 2020 & (Team == "STL" | Team == "DET"), ShO * factor_STL_DET,
                ifelse(Season == 2020, ShO * factor_other, ShO))
  IP <- ifelse(Season == 2020 & (Team == "STL" | Team == "DET"), IP * factor_STL_DET,
               ifelse(Season == 2020, IP * factor_other, IP))
  TBF <- ifelse(Season == 2020 & (Team == "STL" | Team == "DET"), TBF * factor_STL_DET,
                ifelse(Season == 2020, TBF * factor_other, TBF))
  Balls <- ifelse(Season == 2020 & (Team == "STL" | Team == "DET"), Balls * factor_STL_DET,
                  ifelse(Season == 2020, Balls * factor_other, Balls))
  Strikes <- ifelse(Season == 2020 & (Team == "STL" | Team == "DET"), Strikes * factor_STL_DET,
                    ifelse(Season == 2020, Strikes * factor_other, Strikes))
  Pitches <- ifelse(Season == 2020 & (Team == "STL" | Team == "DET"), Pitches * factor_STL_DET,
                    ifelse(Season == 2020, Pitches * factor_other, Pitches))
  RS <- ifelse(Season == 2020 & (Team == "STL" | Team == "DET"), RS * factor_STL_DET,
               ifelse(Season == 2020, RS * factor_other, RS))
  WAR <- ifelse(Season == 2020 & (Team == "STL" | Team == "DET"), WAR * factor_STL_DET,
                ifelse(Season == 2020, WAR * factor_other, WAR))
  WPA <- ifelse(Season == 2020 & (Team == "STL" | Team == "DET"), WPA * factor_STL_DET,
                ifelse(Season == 2020, WPA * factor_other, WPA))
  WPA_minus <- ifelse(Season == 2020 & (Team == "STL" | Team == "DET"), WPA_minus * factor_STL_DET,
                      ifelse(Season == 2020, WPA_minus * factor_other, WPA_minus))
  WPA_plus <- ifelse(Season == 2020 & (Team == "STL" | Team == "DET"), WPA_plus * factor_STL_DET,
                     ifelse(Season == 2020, WPA_plus * factor_other, WPA_plus))
  RE24 <- ifelse(Season == 2020 & (Team == "STL" | Team == "DET"), RE24 * factor_STL_DET,
                 ifelse(Season == 2020, RE24 * factor_other, RE24))
  REW <- ifelse(Season == 2020 & (Team == "STL" | Team == "DET"), REW * factor_STL_DET,
                ifelse(Season == 2020, REW * factor_other, REW))
  wFB <- ifelse(Season == 2020 & (Team == "STL" | Team == "DET"), wFB * factor_STL_DET,
                ifelse(Season == 2020, wFB * factor_other, wFB))
  wSL <- ifelse(Season == 2020 & (Team == "STL" | Team == "DET"), wSL * factor_STL_DET,
                ifelse(Season == 2020, wSL * factor_other, wSL))
  wCT <- ifelse(Season == 2020 & (Team == "STL" | Team == "DET"), wCT * factor_STL_DET,
                ifelse(Season == 2020, wCT * factor_other, wCT))
  wCB <- ifelse(Season == 2020 & (Team == "STL" | Team == "DET"), wCB * factor_STL_DET,
                ifelse(Season == 2020, wCB * factor_other, wCB))
  wCH <- ifelse(Season == 2020 & (Team == "STL" | Team == "DET"), wCH * factor_STL_DET,
                ifelse(Season == 2020, wCH * factor_other, wCH))
  wSF <- ifelse(Season == 2020 & (Team == "STL" | Team == "DET"), wSF * factor_STL_DET,
                ifelse(Season == 2020, wSF * factor_other, wSF))
  wKN <- ifelse(Season == 2020 & (Team == "STL" | Team == "DET"), wKN * factor_STL_DET,
                ifelse(Season == 2020, wKN * factor_other, wKN))
  `RA9-WAR` <- ifelse(Season == 2020 & (Team == "STL" | Team == "DET"), `RA9-WAR` * factor_STL_DET,
                      ifelse(Season == 2020, `RA9-WAR` * factor_other, `RA9-WAR`))
  `BIP-Wins` <- ifelse(Season == 2020 & (Team == "STL" | Team == "DET"), `BIP-Wins` * factor_STL_DET,
                       ifelse(Season == 2020, `BIP-Wins` * factor_other, `BIP-Wins`))
  `LOB-Wins` <- ifelse(Season == 2020 & (Team == "STL" | Team == "DET"), `LOB-Wins` * factor_STL_DET,
                       ifelse(Season == 2020, `LOB-Wins` * factor_other, `LOB-Wins`))
  `FDP-Wins` <- ifelse(Season == 2020 & (Team == "STL" | Team == "DET"), `FDP-Wins` * factor_STL_DET,
                       ifelse(Season == 2020, `FDP-Wins` * factor_other, `FDP-Wins`))
})

# Select rows from fgraphRP of free agents for the four seasons prior to their 
# deal and find the mean of the relevant columns
#Create an empty list to store the data frames
RP_Stats_list <- list()

# Define the columns to calculate the mean
columns_to_mean <- c("W", "L", "ERA", "SV", "BS", "ShO", "IP", "TBF", "Balls", "Strikes", "Pitches", "RS", "K_9", "BB_9",
                     "K_BB", "H_9", "HR_9", "AVG", "WHIP", "LOB_pct", "FIP", "GB_FB", "LD_pct", "GB_pct", "FB_pct", 
                     "IFFB_pct", "HR_FB", "IFH_pct", "BUH_pct", "WAR", "xFIP", "WPA", "WPA_minus", "WPA_plus", "RE24",
                     "REW", "pLI", "inLI", "gmLI", "exLI", "wFB", "wSL", "wCT", "wCB", "wCH", "wSF", "wKN", "O-Swing_pct",
                     "Z-Swing_pct", "Swing_pct", "O-Contact_pct", "Z-Contact_pct", "Contact_pct", "Zone_pct", 
                     "F-Strike_pct", "SwStr_pct", "ERA-", "FIP-", "xFIP-", "K_pct", "BB_pct", "SIERA", "RS_9", "E-F",
                     "RA9-WAR", "BIP-Wins", "LOB-Wins", "FDP-Wins", "K-BB_pct", "Pull_pct", "Cent_pct", "Oppo_pct",
                     "Soft_pct", "Med_pct", "Hard_pct", "TTO_pct")

# Loop through the years 2013 to 2023
for (year in 2013:2023) {
  # Select the most recent 4 seasons before Free Agency
  selected_years <- (year - 4):(year - 1)
  
  # Select rows from fgraphRP with the relevant names and years
  faStats <- fgraphRP[fgraphRP$Name %in% fa_signing_list[[paste0("fa", year)]]$Player &
                        fgraphRP$Season %in% selected_years, ]
  
  # Group rows by "Name" and calculate the mean of desired columns
  faStats <- faStats[, lapply(.SD, mean), by = Name, .SDcols = columns_to_mean]
  
  # Store the column means in the RP_Stats_list
  RP_Stats_list[[as.character(year)]] <- faStats
}

# Loop through the years 2013 to 2023
for (year in 2013:2023) {
  # Merge RP_Stats_list[[year]] with fa_signing_list[[paste0("fa", year)]]
  merged_RP <- merge(RP_Stats_list[[as.character(year)]], 
                     fa_signing_list[[paste0("fa", year)]], 
                     by.x = "Name", by.y = "Player")
  
  # Convert the "AAV" column to numerical format
  merged_RP$AAV <- as.numeric(gsub("[$,]", "", merged_RP$AAV))
  
  # Assign the merged data frame back to RP_Stats_list
  RP_Stats_list[[as.character(year)]] <- merged_RP
}

# Adjust AAV values in each table of RP_Stats_list
for (year in 2013:2022) {
  # Get the previous year
  prev_year <- year - 1
  
  # Extract the AAV values from the previous year's payroll data
  prev_year_total_payroll <- total_payroll_data[total_payroll_data$year == prev_year, "total.payroll"]
  
  # Extract the AAV column from the corresponding table in RP_Stats_list
  aav_column <- RP_Stats_list[[as.character(year)]]$AAV
  
  # Calculate the adjustment factor based on the total payroll of 2022 and the previous year's total payroll
  adjustment_factor <- total_payroll_data[total_payroll_data$year == 2022, "total.payroll"] / prev_year_total_payroll
  
  # Adjust the AAV values by multiplying with the adjustment factor
  RP_Stats_list[[as.character(year)]]$AAV <- aav_column * adjustment_factor
}

# Make one big data table
# Create an empty data table to store the merged data
merged_RP_Stats <- data.table()

# Loop through the years 2013 to 2023
for (year in 2013:2023) {
  # Append the data from each year to the merged_RP_Stats data table
  merged_RP_Stats <- rbind(merged_RP_Stats, RP_Stats_list[[as.character(year)]], fill=TRUE)
}

# Floor the values in columns 'Age 7/1/21', 'Age 7/1/22', and 'Age 7/1/23' so
# that it is consistent with the rest of the data
merged_RP_Stats$Age <- ifelse(is.na(merged_RP_Stats$`Age 7/1/21`), 
                              merged_RP_Stats$Age, 
                              floor(as.numeric(merged_RP_Stats$`Age 7/1/21`)))
merged_RP_Stats$Age <- ifelse(is.na(merged_RP_Stats$`Age 7/1/22`), 
                              merged_RP_Stats$Age, 
                              floor(as.numeric(merged_RP_Stats$`Age 7/1/22`)))
merged_RP_Stats$Age <- ifelse(is.na(merged_RP_Stats$`Age 7/1/23`), 
                              merged_RP_Stats$Age, 
                              floor(as.numeric(merged_RP_Stats$`Age 7/1/23`)))

# Remove last three columns from merged_RP_Stats
merged_RP_Stats <- merged_RP_Stats[, 1:(ncol(merged_RP_Stats) - 3)]

# Rename 'Pos'n' column to 'position'
colnames(merged_RP_Stats)[colnames(merged_RP_Stats) == "Pos'n"] <- "Position"

# Kevin Gausman, Patrick Corbin, Brett Anderson and Mike Minor played primarily 
# as starters their whole careers, teams that tendered them a contract used them
# primarily as starters and therefore not indicative of their value as RPs
merged_RP_Stats <- subset(merged_RP_Stats, !(Name == "Kevin Gausman" | 
                                               Name == "Patrick Corbin" |
                                               Name == "Brett Anderson" |
                                               Name == "Mike Minor"))

# Export merged_RP_Stats to CSV
write.csv(merged_RP_Stats, file = "RP_Training_Data.csv", row.names = FALSE)
```

## Prediction Modeling

As I mentioned above I chose to use Python to carry out the machine
learning portion of this project. Please find that code on my [Github
Page.](https://RHendoDS.github.io)

## Model Analysis

``` r
# Set option to prevent axes from being in exponent form
options(scipen = 999)

# Read in the CSV file
batter_predictions <- read.csv("Batter_Model_Predictions.csv")

# Calculate Mean Absolute Error, Median Absolute Error, and Root Mean Squared Error for AAV columns
MAE <- mean(abs(batter_predictions$AAV - batter_predictions$Predicted_AAV))
MedAE <- median(abs(batter_predictions$AAV - batter_predictions$Predicted_AAV))
RMSE <- sqrt(mean((batter_predictions$AAV - batter_predictions$Predicted_AAV)^2))

# Convert numeric values to dollar format
MAE_dollar <- paste("$", format(MAE, big.mark = ",", digits = 2), sep = "")
MedAE_dollar <- paste("$", format(MedAE, big.mark = ",", digits = 2), sep = "")
RMSE_dollar <- paste("$", format(RMSE, big.mark = ",", digits = 2), sep = "")

# Display the results
cat("RP AAV Mean Absolute Error:", MAE_dollar, "\n")
```

    ## RP AAV Mean Absolute Error: $3,064,143

``` r
cat("RP AAV Median Absolute Error:", MedAE_dollar, "\n")
```

    ## RP AAV Median Absolute Error: $2,242,623

``` r
cat("RP AAV Root Mean Squared Error:", RMSE_dollar, "\n")
```

    ## RP AAV Root Mean Squared Error: $4,072,447

``` r
# Calculate Mean Absolute Error, Median Absolute Error, and Root Mean Squared Error for Years column
MAE_Years <- mean(abs(batter_predictions$Years - batter_predictions$Predicted_Years))
MedAE_Years <- median(abs(batter_predictions$Years - batter_predictions$Predicted_Years))
RMSE_Years <- sqrt(mean((batter_predictions$Years - batter_predictions$Predicted_Years)^2))

# Print the results for Years column
cat("Batter Years Mean Absolute Error:", MAE_Years, "\n")
```

    ## Batter Years Mean Absolute Error: 0.8860059

``` r
cat("Batter Years Median Absolute Error:", MedAE_Years, "\n")
```

    ## Batter Years Median Absolute Error: 0.6498917

``` r
cat("Batter Years Root Mean Squared Error:", RMSE_Years, "\n")
```

    ## Batter Years Root Mean Squared Error: 1.240903

``` r
# Use RdYlGn color palette from RColorBrewer
custom_colors <- brewer.pal(11, "RdYlGn")

# Create the plot
ggplot(batter_predictions, aes(x = AAV, y = Predicted_AAV, color = factor(-round(Years_Difference)))) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "blue") +
  labs(title = "Batter Contract Predictions",
       x = "Contract AAV (2023 USD)", y = "Predicted AAV (2023 USD)",
       color = "Years Above Expected") +
  scale_x_continuous(labels = dollar_format(prefix = "$"), expand = c(0, 0)) +
  scale_y_continuous(labels = dollar_format(prefix = "$"), expand = c(0, 0)) +
  coord_cartesian(xlim = c(-5000000, 45000000), ylim = c(-5000000, 45000000)) +
  scale_color_manual(values = custom_colors, breaks = rev(levels(factor(-round(batter_predictions$Years_Difference))))) +
  geom_text(data = filter(batter_predictions, 
                          AAV > 3000000 & 
                            (rank(AAV_Difference) <= nrow(batter_predictions) *  .03 |
                               rank(desc(AAV_Difference)) <= nrow(batter_predictions) * 0.03)),
        aes(label = Name), color = "black", vjust = -0.7, hjust = 0.5, size = 3) +
  theme_minimal()
```

![](MLB_Contracts_Main_files/figure-gfm/Batter_Graph-1.png)<!-- -->

``` r
# Read in the CSV file
SP_predictions <- read.csv("SP_Model_Predictions.csv")

# Calculate Mean Absolute Error, Median Absolute Error, and Root Mean Squared Error for AAV columns
MAE <- mean(abs(SP_predictions$AAV - SP_predictions$Predicted_AAV))
MedAE <- median(abs(SP_predictions$AAV - SP_predictions$Predicted_AAV))
RMSE <- sqrt(mean((SP_predictions$AAV - SP_predictions$Predicted_AAV)^2))

# Convert numeric values to dollar format
MAE_dollar <- paste("$", format(MAE, big.mark = ",", digits = 2), sep = "")
MedAE_dollar <- paste("$", format(MedAE, big.mark = ",", digits = 2), sep = "")
RMSE_dollar <- paste("$", format(RMSE, big.mark = ",", digits = 2), sep = "")

# Display the results
cat("RP AAV Mean Absolute Error:", MAE_dollar, "\n")
```

    ## RP AAV Mean Absolute Error: $3,964,592

``` r
cat("RP AAV Median Absolute Error:", MedAE_dollar, "\n")
```

    ## RP AAV Median Absolute Error: $3,413,728

``` r
cat("RP AAV Root Mean Squared Error:", RMSE_dollar, "\n")
```

    ## RP AAV Root Mean Squared Error: $5,033,230

``` r
# Calculate Mean Absolute Error, Median Absolute Error, and Root Mean Squared Error for Years column
MAE_Years <- mean(abs(SP_predictions$Years - SP_predictions$Predicted_Years))
MedAE_Years <- median(abs(SP_predictions$Years - SP_predictions$Predicted_Years))
RMSE_Years <- sqrt(mean((SP_predictions$Years - SP_predictions$Predicted_Years)^2))

# Print the results for Years column
cat("SP Years Mean Absolute Error:", MAE_Years, "\n")
```

    ## SP Years Mean Absolute Error: 0.8579012

``` r
cat("SP Years Median Absolute Error:", MedAE_Years, "\n")
```

    ## SP Years Median Absolute Error: 0.6794747

``` r
cat("SP Years Root Mean Squared Error:", RMSE_Years, "\n")
```

    ## SP Years Root Mean Squared Error: 1.114894

``` r
# Use RdYlGn color palette from RColorBrewer
custom_colors <- brewer.pal(9, "RdYlGn")

# Create the plot
ggplot(SP_predictions, aes(x = AAV, y = Predicted_AAV, color = factor(-round(Years_Difference)))) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "blue") +
  labs(title = "Starting Pitchers Contract Predictions",
       x = "Contract AAV (2023 USD)", y = "Predicted AAV (2023 USD)",
       color = "Contract Years Above Expected") +
  scale_x_continuous(labels = dollar_format(prefix = "$"), expand = c(0, 0)) +
  scale_y_continuous(labels = dollar_format(prefix = "$"), expand = c(0, 0)) +
  coord_cartesian(xlim = c(-5000000, 55000000), ylim = c(-5000000, 55000000)) +
  scale_color_manual(values = custom_colors, breaks = rev(levels(factor(-round(SP_predictions$Years_Difference))))) +
  geom_text(data = filter(SP_predictions, 
                          AAV > 3000000 & 
                            (rank(AAV_Difference) <= nrow(SP_predictions) * 0.02 |
                               rank(desc(AAV_Difference)) <= nrow(SP_predictions) * 0.02)),
            aes(label = Name), color = "black", vjust = -0.7, hjust = 0.5, size = 3) +
  theme_minimal()
```

![](MLB_Contracts_Main_files/figure-gfm/SP_Graph-1.png)<!-- -->

``` r
# Read in the CSV file
RP_predictions <- read.csv("RP_Model_Predictions.csv")

# Calculate Mean Absolute Error, Median Absolute Error, and Root Mean Squared Error for AAV columns
MAE <- mean(abs(RP_predictions$AAV - RP_predictions$Predicted_AAV))
MedAE <- median(abs(RP_predictions$AAV - RP_predictions$Predicted_AAV))
RMSE <- sqrt(mean((RP_predictions$AAV - RP_predictions$Predicted_AAV)^2))

# Convert numeric values to dollar format
MAE_dollar <- paste("$", format(MAE, big.mark = ",", digits = 2), sep = "")
MedAE_dollar <- paste("$", format(MedAE, big.mark = ",", digits = 2), sep = "")
RMSE_dollar <- paste("$", format(RMSE, big.mark = ",", digits = 2), sep = "")

# Display the results
cat("RP AAV Mean Absolute Error:", MAE_dollar, "\n")
```

    ## RP AAV Mean Absolute Error: $2,539,138

``` r
cat("RP AAV Median Absolute Error:", MedAE_dollar, "\n")
```

    ## RP AAV Median Absolute Error: $2,046,638

``` r
cat("RP AAV Root Mean Squared Error:", RMSE_dollar, "\n")
```

    ## RP AAV Root Mean Squared Error: $3,387,297

``` r
# Calculate Mean Absolute Error, Median Absolute Error, and Root Mean Squared Error for Years column
MAE_Years <- mean(abs(RP_predictions$Years - RP_predictions$Predicted_Years))
MedAE_Years <- median(abs(RP_predictions$Years - RP_predictions$Predicted_Years))
RMSE_Years <- sqrt(mean((RP_predictions$Years - RP_predictions$Predicted_Years)^2))

# Print the results for Years column
cat("RP Years Mean Absolute Error:", MAE_Years, "\n")
```

    ## RP Years Mean Absolute Error: 0.5249089

``` r
cat("RP Years Median Absolute Error:", MedAE_Years, "\n")
```

    ## RP Years Median Absolute Error: 0.4241345

``` r
cat("RP Years Root Mean Squared Error:", RMSE_Years, "\n")
```

    ## RP Years Root Mean Squared Error: 0.6756552

``` r
# Use RdYlGn color palette from RColorBrewer
custom_colors <- brewer.pal(5, "RdYlGn")

# Create the plot
ggplot(RP_predictions, aes(x = AAV, y = Predicted_AAV, color = factor(-round(Years_Difference)))) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "blue") +
  labs(title = "Relief Pitchers Contract Predictions",
       x = "Contract AAV (2023 USD)", y = "Predicted AAV (2023 USD)",
       color = "Contract Years Above Expected") +
  scale_x_continuous(labels = dollar_format(prefix = "$"), expand = c(0, 0)) +
  scale_y_continuous(labels = dollar_format(prefix = "$"), expand = c(0, 0)) +
  coord_cartesian(xlim = c(-2000000, 30000000), ylim = c(-2000000, 30000000)) +
  scale_color_manual(values = custom_colors, breaks = rev(levels(factor(-round(RP_predictions$Years_Difference))))) +
  geom_text(data = filter(RP_predictions, 
                          AAV > 3000000 & 
                            (rank(AAV_Difference) <= nrow(RP_predictions) * 0.02 |
                               rank(desc(AAV_Difference)) <= nrow(RP_predictions) * 0.06)),
            aes(label = Name), color = "black", vjust = -0.7, hjust = 0.5, size = 3) +
  theme_minimal()
```

![](MLB_Contracts_Main_files/figure-gfm/RP_Graph-1.png)<!-- -->

## Conclusion

Overall, I am pretty happy with the results of my model. As I expect the
relief pitchers model performed best, likely due to the smaller
variations in their contracts as a whole. It also makes sense that the
batters model performed relatively better than the starting pitchers as
they are less likely to have major injury concern, which is something my
models were not designed to accommodate for.
