source("R_scripts/load_species.R")
remaining <- read.csv("Phys_tidy/tidy_P_Remaining.csv")

nrow(remaining)
# [1] 103
length(unique(remaining$Collection_Number))
# [1] 87

### Colorado
## Montrose County
length(grep("11/04/17", total_physaria$ID))
# [1] 22
length(grep("11/11/17", total_physaria$ID))
# [1] 11
## Mesa County
length(grep("11/12/17", total_physaria$ID))
# [1] 12
## Remaining CO
length(grep("Colorado", remaining$State))
# [1] 15


### Utah - started 12/16/17
length(grep("Utah", remaining$State))
# [1] 165

## Daggett County
length(grep("Daggett", total_physaria$County))
# [1] 35
length(grep("12/16/17", total_physaria$ID))
# [1] 7
length(grep("12/17/17", total_physaria$ID))
# [1] 28


## Duchesne County
length(grep("Duchesne", remaining$County))
# [1] 1
length(grep("1/20/18", total_physaria$ID))
# [1] 14
length(grep("01/27/18", total_physaria$ID))
# [1] 15


## Grand, Utah
length(grep("Grand", remaining$County))
# [1] 0
length(grep("02/17/18", total_physaria$ID))
# [1] 13
length(grep("02/18/18", total_physaria$ID))
# [1] 14
length(grep("02/24/18", total_physaria$ID))
# [1] 19


## San Juan, Utah
length(grep("San Juan", remaining$County))
# [1] 5
length(grep("02/25/18", total_physaria$ID))
# [1] 10
length(grep("03/17/18", total_physaria$ID))
# [1] 7
length(grep("03/18/18", total_physaria$ID))
# [1] 12
length(grep("03/24/18", total_physaria$ID))
# [1] 14


### Uintah, Utah
length(grep("03/03/18", total_physaria$ID))
# [1] 16
length(grep("03/04/18", total_physaria$ID))
# [1] 24
## Totals
length(grep("03/0[3-4]/18", total_physaria$ID))
# [1] 40
length(grep("03/09/18", total_physaria$ID))
# [1] 5


### Carbon, Utah
length(grep("Carbon", remaining$County))
# [1] 1
length(grep("03/24/18", total_physaria$ID))
# [1] 18
length(grep("03/25/18", total_physaria$ID))
# [1] 8


### Salt Lake, Utah
length(grep("Salt Lake", remaining$County))
# [1] 0
### Utah, Utah
length(grep("Utah", remaining$County))
# [1] 0
### Cache, Utah
length(grep("Cache", remaining$County))
# [1] 1
length(grep("04/04/18", total_physaria$ID))
# [1] 11
length(grep("04/06/18", total_physaria$ID))
# [1] 11

