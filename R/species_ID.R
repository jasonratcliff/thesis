### Keeping track of IDs to the finish line:
# Extend to plot the distribution (categorical ID i.e. !/C/? ?)

# IDs for 4/27-30/17
grep("4/2[7-9]/17|4/30/17", total_physaria$ID)
# > length(grep("4/2[7-9]/17|4/30/17", total_physaria$ID))
# [1] 73

# IDs for 5/2/17
grep("5/2/17", total_physaria$ID)
# > length(grep("5/2/17", total_physaria$ID))
# [1] 22

# IDs for 5/4-5/17
grep("5/[4-6]/17", total_physaria$ID)
# > length(grep("5/[4-5]/17", total_physaria$ID))
# [1] 3

# IDs for 5/9/17
grep("5/9/17", total_physaria$ID)
# > length(grep("5/9/17", total_physaria$ID))
# [1] 14

# IDs for 5/12/17
grep("5/12/17", total_physaria$ID)
# > length(grep("5/12/17", total_physaria$ID))
# [1] 13

# IDs for 5/13/17
grep("5/13/17", total_physaria$ID)
# > length(grep("5/13/17", total_physaria$ID))
# [1] 8

# IDs for 5/14/17
grep("5/14/17", total_physaria$ID)
# > length(grep("5/14/17", total_physaria$ID))
# [1] 12


## Dealing with whitespace in IDs
# > grep("[!,C,?]  [0-9]", total_physaria$ID)
# [1]    9  243  249  252  253  254  255  257  393  406  407  524  527  616  617  618  619  621
# [19] 1451 1452 1453 1454 1455 1456 1457 1458 1459 1460 1462 1475 1476 1478 1480 1481 1482 1483
# [37] 1485 1486 1545
# > total_physaria$ID[9]
# [1] "!  05/13/17"


# IDs for 5/17/17
grep("5/17/17", total_physaria$ID)
# > length(grep("5/17/17", total_physaria$ID))
# [1] 13

# IDs for 5/18/17
grep("5/18/17", total_physaria$ID)
# > length(grep("5/18/17", total_physaria$ID))
# [1] 19

# IDs for 5/19/17
grep("5/19/17", total_physaria$ID)
# > length(grep("5/19/17", total_physaria$ID))
# [1] 17

# IDs for 5/20/17
grep("5/20/17", total_physaria$ID)
# > length(grep("5/20/17", total_physaria$ID))
# [1] 19

# IDs for 5/24/17
grep("5/24/17", total_physaria$ID)
# > length(grep("5/24/17", total_physaria$ID))
# [1] 21

# IDs for 5/25/17
grep("5/25/17", total_physaria$ID)
# > length(grep("5/25/17", total_physaria$ID))
# [1] 29

# > length(grep("5/2[4-8]/17", total_physaria$ID))
# [1] 51

# > length(grep("5/26/17", total_physaria$ID))
# [1] 31

# > length(grep("5/28/17", total_physaria$ID))
# [1] 15
 
# > length(grep("06/03/17", total_physaria$ID))
# [1] 18
 
# > length(grep("06/04/17", total_physaria$ID))
# [1] 23

# > length(grep("06/08/17", total_physaria$ID))
# [1] 21

# > length(grep("06/09/17", total_physaria$ID))
# [1] 15

# > length(grep("06/10/17", total_physaria$ID))
# [1] 8

# Month of May
length(grep("05/[0-3][0-9]/17", total_physaria$ID))

for(id in total_physaria$ID) {
  print (id)
}

ggplot(data = total_physaria, 
       mapping = aes(x = ID, y = Taxon_a_posteriori), 
       geom = "point")


dim(
  total_physaria[intersect(which(total_physaria$ID !=2), 
                           which(is.na(total_physaria$ID) == FALSE)), ] )




length(grep("09/09/17", total_physaria$ID))
# [1] 21


length(grep("09/10/17", total_physaria$ID))
# [1] 22

length(grep("09/16/17", total_physaria$ID))
# [1] 46

length(grep("09/17/17", total_physaria$ID))
# [1] 13

length(grep("09/23/17", total_physaria$ID))
# [1] 24

length(grep("09/24/17", total_physaria$ID))


### Statistics of remaining specimens
remaining_df <- read.csv("Phys_tidy/tidy_P_Remaining.csv")

# Total number of unique remaining specimens
length(unique(remaining_df$Collection_Number))
# [1] 291

# Remaining counties per GREP'ed state
unique(remaining_df[grep("Colorado", remaining_df$State), "County"])
# [1] Archuleta   Delta       Gunnison    Hinsdale    Mesa        Montezuma  
# [7] Montezuma   Montrose    Ouray       Rio Blanco  Saguache    Clear Creek
# [13] Custer      Mineral     Park        ?           Chaffee  

# Number of unique specimens per GREP'ed county
length(unique(
  remaining_df[grep("Montezuma", remaining_df$County),
                    "Collection_Number"]))
# [1] 23





