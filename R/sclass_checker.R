#title: Sclass Checker
#author: Kori Blankenship
#date: 2022-08-07

#Run a series of checks to make sure sclass combinations are classed correctly.
#Outputs are written to CSV files and require checking.

## SETUP
# 1. Set the choose_region variable in the join_attributes2vat.r helper script.

# 2. Make a list of BPS_MODEL codes for which you want to check the reference rules.
#A table of the classified cells will be output.
#Check output against the reference rules by hand.
bps_list <- as.list(c("10360_1_2", "10390_1_2_3_7", "11780_1_2_7"))

# 3. Run helper to get the combine dbf with the joined attributes
source("R/join_attributes2vat.R") 
## SETUP END

## LIBRARIES
library(tidyverse)

## BRING IN THE NON REFERENCE SCLASS RULES
nonrefrules <- read.csv("./IN/LANDFIRESuccessionClassMappingRules_07202022_nonreference.csv")

# *************************************************************
# CHECK SELECTED REFERENCE RULES
# *************************************************************
#this will create a table with classified combinations for all BpS
#in the bps_list created in the SETUP
bpsrulescheck <- filter(join, BPS_MODEL %in% bps_list)

# *************************************************************
# SUM COUNT OF CELLS IN EACH SCLASS
# *************************************************************
sclasscount <- join %>%
  group_by(SCLASS) %>%
  summarise(SCLASSCOUNT = sum(COUNT))

# *************************************************************
# SUM COUNT IN EVT PHYS
# *************************************************************
physcount <- join %>%
  group_by(EVT_PHYS)%>%
  summarise(PHYSCOUNT = sum(COUNT))

# *************************************************************
# SPARSE BPS CHECK
# *************************************************************
## Filter sparse BpS
#the list of sparse values is for CONUS, HI has no sparse BpS
sparsebpscheck <- filter(join, BPS_MODEL %in% c(10010, 10020,10030, 10040, 
                                                10060,10070, 13410, 14980)) 

# *************************************************************
# NON REFERENCE RULES CHECK - NON REF RULES VS. MAPPED SCLASS
# *************************************************************
## Join the non reference rules to the "join" table
#filter for 2020 EVT values (values less than 7000 are for earlier versions)
nonrefrules <- nonrefrules %>%
  filter(EVT.VALUE>6999) 
#rename the columns for the join 
#evt_value is assigned in the join_attributes2vat.R helper script
colnames(nonrefrules) <- c(evt_value, "EVT_rules", "SCLASS_rules") 
#join nonref rules to "join" by EVT value
nonrefcheck <- left_join(nonrefrules, join, by = evt_value) 

## Check that the SCLASS_rules (from rules table) match the SCLASS (mapped value) 
#"CHECK" column is true if they match
nonrefcheck <- nonrefcheck  %>% 
  mutate(CHECK = (SCLASS_rules == "Agriculture" & SCLASS == "Agriculture" |
                    SCLASS_rules == "Barren or Sparse" & SCLASS == "Barren or Sparse" |
                    SCLASS_rules == "Snow / Ice" & SCLASS == "Snow/Ice" |
                    SCLASS_rules == "Water" & SCLASS == "Water" |
                    SCLASS_rules == "UE" & SCLASS == "UE" |
                    #developed trumps UE so I check it last
                    SCLASS_rules == "Developed" & SCLASS == "Developed"))

## Remove Barren or Sparse BpS
nonrefcheck2 <- nonrefcheck %>% 
  filter(!BPS_MODEL %in% c(10010, 10020,10030, 10040, 
                           10060,10070, 13410, 14980))

## Filter for false values and check them by hand
nonrefcheck3 <- filter(nonrefcheck2, CHECK == "FALSE")

# *************************************************************
# NON REFERENCE RULES CHECK -- MAPPED SCLASS VS. EVT_PHYS
# *************************************************************
## Check that the SCLASS (mapped value) matches the EVT_PHYS  
#"CHECK" column is true if they match
#start with a filter to find when EVT_PHYS is a non reference value
evtphyscheck <- join  %>% 
  filter(EVT_PHYS %in%  c("Sparsely Vegetated", "Open Water",  
                          "Quarries-Strip Mines-Gravel Pits-Well and Wind Pads", 
                          "Developed-Low Intensity", "Developed-Medium Intensity", 
                          "Developed-High Intensity", "Developed-Roads",
                          "Developed-Open Space", "Snow-Ice", "Agricultural", 
                          "Developed", "Exotic Herbaceous", "Exotic Tree-Shrub")) %>% 
  mutate(CHECK = (SCLASS == "Agriculture" & EVT_PHYS == "Agricultural" |
                    SCLASS == "Barren or Sparse" & EVT_PHYS == "Sparsely Vegetated" |
                    SCLASS == "Snow/Ice" & EVT_PHYS == "Snow-Ice" |
                    SCLASS == "Water" & EVT_PHYS == "Open Water" |
                    SCLASS == "UE" & EVT_PHYS == "Exotic Tree-Shrub" |
                    SCLASS == "UE" & EVT_PHYS == "Exotic Herbaceous" | 
                    SCLASS == "UE" & EVT_PHYS == "Developed" |
                    SCLASS == "UE" & EVT_NAME == "Ruderal" | 
                    SCLASS == "UE" & EVT_NAME == "Plantation" |
                    SCLASS == "Developed" & EVT_PHYS == "Developed" |
                    SCLASS == "Developed" & EVT_PHYS == "Developed-Low Intensity" |
                    SCLASS == "Developed" & EVT_PHYS == "Developed-Medium Intensity" |
                    SCLASS == "Developed" & EVT_PHYS == "Developed-High Intensity" |
                    SCLASS == "Developed" & EVT_PHYS == "Developed-Roads" |
                    SCLASS == "Developed" & EVT_PHYS == "Developed-Open Space" |
                    SCLASS == "Developed" & EVT_PHYS == "Quarries-Strip Mines-Gravel Pits-Well and Wind Pads"))

## Remove Barren or Sparse BpS
evtphyscheck2 <- evtphyscheck %>% 
  filter(!BPS_MODEL %in% c(10010, 10020,10030, 10040, 
                          10060,10070, 13410, 14980))

## Filter for false values and check them by hand
evtphyscheck3 <- filter(evtphyscheck2, CHECK == "FALSE")

# *************************************************************
# WRITE RESULTS TO CSV
# *************************************************************
setwd("./OUT/") #where you want results
write.csv(bpsrulescheck, "bpsrulescheck.csv") 
write.csv(sclasscount, "sclasscount.csv")   
write.csv(physcount, "physcount.csv")
write.csv(sparsebpscheck, "sparsebpscheck.csv") 
write.csv(nonrefcheck3, "nonrefcheck3.csv")
write.csv(evtphyscheck3, "evtphyscheck3.csv")

# *************************************************************
# CHECKING THE OUTPUT
# *************************************************************
##Oupupt tables should be checked by hand.

# bpsrulescheck.csv - check combinations against the reference rules
# sclasscount.csv - check selected classes against physcount.csv
# physcount.csv - check selected classes against sclasscount.csv
# sparsebpscheck.csv - check that sparse BpS classified correctly; 
#                    - will be blank in HI b/c there are no sparse BpS there
# nonrefcheck3.csv - will be blank if all combinations classified correctly
# evtphyscheck3.csv - will be blank if all combinations classified correctly

#Using the small sample dbf provided with this code, even the tables listed 
#above could be blank b/c the sclass combinations may not present in the 
#sample data. The criteria for these tables will be met when checking a 
#CONUS wide combined sclass table. 

