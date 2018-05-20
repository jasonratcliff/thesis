### Helpful code ###

# Reference a specific git commit:
# > https://github.com/<owner>/<project>/commit/<hash>

### Remove lists of workspace objects:
# Remove all workspace objects matching "P_*" regular expression.
rm(list = ls(pattern = "P_*"))

# Remove all workspace objects matching "^taxa_a_priori*" regular expression.
rm(list = ls(pattern = "^a_priori*"))

# Remove all workspace objects matching "^tidy_" regular expression.
rm(list = ls(pattern = "^tidy_"))

# Remove objects from MAPPED_ENV.
rm(x, envir=mapped_env)

### Counter to index names of SPECIES_DATA elements.
i <- 1

for (species in species_data) { 
  print(names(species_data)[i])
  i <- i + 1 
}
# [1] "P_acutifolia"
# [1] "P_didymocarpa"
# [1] "P_eburniflora"
# [1] "P_integrifolia"
# [1] "P_Remaining"
# [1] "P_saximontana"
# [1] "P_vitulifera_CO"
# [1] "P_vitulifera_WY"

rm(i, species) # Remove iters.


### Date Formatting

# Excel initially converted dates to 5-digit numbers.
grep("[0-9]{5}", date_test$Date, invert = TRUE )
# [1]  8  9 11 18 33 35 37 73 74 85 86


find_dates <- function(species) {
  # Regular expression matches formatted dates.
  species$Date[grep("[0-1][0-9]/[0-3][0-9]/[1-2][0-9][0-9][0-9]",
                    species$Date, invert = TRUE)] # In $Date column, identify non-matching strings.
}


as.Date(remaining$Date, format = "%m/%d/%Y")


abnormal_dates <- grep("[0-1][0-9]/[0-3][0-9]/[1-2][0-9][0-9][0-9]", species$P_acutifolia$Date)



### DEBUG of lines 90:117 in load_species.R
# Problem - number of columns unequal between data frames.

# +   # Assign 'tidy' data frame with split a priori ID lists to Global Environment.
#   +   assign(gsub("a_priori_", "tidy_", taxa_id_a_priori),
#              +          species_key, envir = .GlobalEnv) # Assign new data frame from split list of a priori IDs.
# + }
#@! Error in `[.data.frame`(get(taxon, envir = .GlobalEnv), , 2:55) : 
#@!   undefined columns selected

test_sp <- ls(pattern = "^P_")

test_sp
# [1] "P_acutifolia"    "P_didymocarpa"   "P_eburniflora"   "P_integrifolia"  "P_Remaining"    
# [6] "P_saximontana"   "P_vitulifera_CO" "P_vitulifera_WY"

class(lapply(test_sp, get))
# [1] "list"

sapply(lapply(test_sp, get), ncol)
# [1] 55 55 55 54 55 55 55 55

### Awk code to identify question marks.
# Write first 6 columns to .csv file in "Phys_maps/".
write.csv(total_physaria_mapped[,1:6], file="Phys_maps/total_physaria_mapped.csv", row.names=FALSE)
# Regular expression to read .csv files.
# Use command flag to set the field separator.
awk -F "\"*,\"*" '/\?/ {print NR-1, $4}' Phys_maps/total_physaria_mapped.csv | head
# 73 Physaria acutifolia?
# 74 Physaria acutifolia - eburniflora?
# 80 Physaria acutifolia - lanata?
# 82 Physaria acutifolia - lanata?
# 148 Physaria acutifolia ?
# 149 Physaria acutifolia ?
# 150 Physaria acutifolia ?
# 155 Physaria ?
# 156 Physaria ?
# 161 Physaria ?

# Write AWK output to file.
awk -F "\"*,\"*" '/\?/ {print NR-1, $4}' Phys_maps/total_physaria_mapped.csv > Phys_maps/questioned_taxa.txt

total_physaria_mapped[c(73:74,80), c(4,5:6)]
# Taxon_a_posteriori     Collector Collection_Number
# 73               Physaria acutifolia? R. L. Hartman             38283
# 74 Physaria acutifolia - eburniflora? K. H. Dueholm              7850
# 80      Physaria acutifolia - lanata?       R. Dorn              7931

# Return unique a posteriori identifications from TOTAL_PHYSARIA
unique(total_physaria$Taxon_a_posteriori[grep("\\?", 
                                              total_physaria$Taxon_a_posteriori, 
                                              invert = TRUE)])

# Return questionable identifications from TOTAL_PHYSARIA
unique(total_physaria$Taxon_a_posteriori[grep("\\?", 
                                              total_physaria$Taxon_a_posteriori, 
                                              invert = FALSE)])

# R Markdown
knit(input = "README.Rmd", output = "README.md")


# COunties with ID'd eburniflora in Wyoming
unique(total_physaria[grep("eburniflora", 
                           total_physaria$Physaria_a_priori_1), 
                      "County"])
# > 
# [1] "Fremont" "Natrona" "Carbon"


# Counties with ID'd saximontana in Wyoming
unique(total_physaria[intersect(grep("saximontana", 
                                       total_physaria$Physaria_a_priori_1), 
                                  grep("Wyoming", 
                                       total_physaria$State)), 
                        "County"])
# >
# [1] "Hot Springs" "Fremont"   


# Counties with ID'd saximontana in Montana
unique(total_physaria[intersect(grep("saximontana", 
                                     total_physaria$Physaria_a_priori_1), 
                                grep("Montana", 
                                     total_physaria$State)), 
                      "County"]) 
# > 
# [1] "Carbon"        "Flathead"      "Glacier"       "Pondera"      
# [5] "Teton"         "Lewis & Clark" "Beaverhead"    "Madison"      
# [9] "Meagher"       "Gallatin"      "Park"          "Stillwater"   
# [13] "Hill"          "Fergus"   



cat(paste(as.character(unique(total_physaria$County[grep("Wyoming", 
                                                         total_physaria$State)])),
          collapse = '", "'))
     
as.Date(wy_vitulifera$Date, '%m/%d/%Y')

length(grep("^2$", total_physaria$ID))


> str(sat_base)
chr [1:1280, 1:1280] "#707064" "#6B6B60" "#686858" "#6C6C5C" "#6C6C5C" ...
- attr(*, "class")= chr [1:2] "ggmap" "raster"
- attr(*, "bb")='data.frame':	1 obs. of  4 variables:
  ..$ ll.lat: num 43
..$ ll.lon: num -109
..$ ur.lat: num 45.5
..$ ur.lon: num -105
- attr(*, "source")= chr "google"
- attr(*, "maptype")= chr "satellite"
- attr(*, "zoom")= num 8
> attr(sat_base, "bb")
ll.lat    ll.lon   ur.lat    ur.lon
1 42.9754 -108.7551 45.49372 -105.2394
> class(attr(sat_base, "bb"))
[1] "data.frame"
> dim(attr(sat_base, "bb"))
[1] 1 4
> names(attr(sat_base, "bb"))
[1] "ll.lat" "ll.lon" "ur.lat" "ur.lon"
> names(attr(sat_base, "bb"))[1]
[1] "ll.lat"
> class(names(attr(sat_base, "bb"))[1])
[1] "character"
