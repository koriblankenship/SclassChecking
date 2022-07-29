#Run a series of checks to make sure sclass combinations are classed correctly.
#Search for "INPUT" to find a couple cases where user can specify items.
#Outputs are written to CSV files and require checking.

## SETUP
library(dplyr)
source("joining_attributes2vat.r") #Get the combine dbf with the joined attributes
#Bring in the non reference sclass rules 
nonrefrules <- read.csv("./IN/LANDFIRESuccessionClassMappingRules_05042022_nonreference.csv")

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
# NON REFERENCE RULES CHECK - check all evts listed in the nonreference rules
# *************************************************************
nonref <- filter(nonrefrules, EVT.VALUE > 6999) #get non reference rules for 2020 EVT values
nonref = rename(nonref, CONUS_EVT = EVT.VALUE) #rename the evt value column for the join
nonref = rename(nonref, EVT_rules = EVT.CLASSNAME) #rename the sclass column for the join
nonref = rename(nonref, SCLASS_rules = SCLASS) #rename the sclass column for the join
nonrefcheck <- left_join(nonref, join, by = 'CONUS_EVT') #join nonref rules to evt 

#CHECK field is true if evt phys of ag, sparse, snow ice and water match the sclass call 
nonrefcheck1 <- nonrefcheck  %>% 
  mutate(CHECK = (SCLASS == "Agriculture" & EVT_PHYS == "Agricultural" |
                    SCLASS == "Barren or Sparse" & EVT_PHYS == "Sparsely Vegetated" |
                    SCLASS == "Barren or Sparse" & EVT_PHYS == "Quarries-Strip Mines-Gravel Pits-Well and Wind Pads" |
                    SCLASS == "Snow/Ice" & EVT_PHYS == "Snow-Ice" |
                    SCLASS == "Water" & EVT_PHYS == "Open Water" |
                    SCLASS == "UE" & EVT_PHYS == "Exotic Tree-Shrub" |
                    SCLASS == "UE" & EVT_PHYS == "Exotic Herbaceous" |
                    SCLASS == "UE" & EVT_PHYS == "Developed" | #developed trumps UE
                    SCLASS == "Urban" & EVT_PHYS == "Developed" |
                    SCLASS == "Urban" & EVT_PHYS == "Developed-Low Intensity" |
                    SCLASS == "Urban" & EVT_PHYS == "Developed-Medium Intensity" |
                    SCLASS == "Urban" & EVT_PHYS == "Developed-High Intensity" |
                    SCLASS == "Urban" & EVT_PHYS == "Developed-Roads" |
                    SCLASS == "Urban" & EVT_PHYS == "Developed-Open Space"))

#filter for false and check the remaining values by hand
nonrefcheck2 <- filter(nonrefcheck1, CHECK == "FALSE")
#split up to make tables w/ fewer rows: 1 table has mapzones <=50, the other has MZs > 50
nonrefcheck2_W <- filter(nonrefcheck2, SCLASS == "UE" & CONUS_MZ_0 <= 50)
nonrefcheck2_E <- filter(nonrefcheck2, SCLASS == "UE" & CONUS_MZ_0 > 50)

# *************************************************************
# SPARSE BPS CHECK
# *************************************************************
sparsebpscheck <- filter(join, BPS_MODEL %in% c(10010, 10020,10030, 10040, 
                                                10060,10070, 13410, 14980)) #filter sparse bps

# *************************************************************
# NON REFERENCE RULES CHECK -- if sclass is nonreference, what is the EVT (after filtering out direct matches)
# *************************************************************
agcheck <- filter(join, SCLASS == "Agriculture", !EVT_PHYS == "Agricultural") #sclass is ag, phys is not ag

sparsecheck <- join %>% #sclass is barren or sparse, phys is not 
  filter(SCLASS == "Barren or Sparse" & !EVT_PHYS %in% c('Sparsely Vegetated', 
                                                         'Quarries-Strip Mines-Gravel Pits-Well and Wind Pads')) 

snowicecheck <- filter(join, SCLASS == "Snow/Ice", !EVT_PHYS == "Snow-Ice") #sclass is snow/ice, phys is not 

uecheck <- join %>% #sclass is UE, phys is not exotic or developed
  filter(SCLASS == "UE" & !EVT_PHYS %in% c('Exotic Tree-Shrub', 'Exotic Herbaceous', 'Developed'))
uecheck2 <- filter(uecheck, !grepl('Ruderal', EVT_NAME)) #remove rows that contain ruderal in evt_name
uecheck3 <- filter(uecheck2, !grepl('Plantation', EVT_NAME)) #remove rows that contain plantation in evt_name

urbancheck <- join %>% #sclass is urban, phys is not developed
  filter(SCLASS == "Urban" & !EVT_PHYS %in% c('Developed', 
                                              'Developed-Low Intensity', 
                                              'Developed-Medium Intensity', 
                                              'Developed-High Intensity', 
                                              'Developed-Roads', 
                                              'Developed-Open Space'))

watercheck <- filter(join, SCLASS == "Water", !EVT_PHYS == "Open Water") #sclass is water, phys is not 

# *************************************************************
# GET RESULTS FOR A SPECIFIC BPS IN A TABLE
# *************************************************************
#INPUT: make a list of BPS_MODEL codes you want to check the rules for
bps_list <- as.list(c("10360_1_2", "10390_1_2_3_7", "11780_1_2_7"))
bpsrulescheck <- filter(join, BPS_MODEL %in% bps_list)

# *************************************************************
# WRITE RESULTS TO CSV
# *************************************************************
setwd("./OUT/") # INPUT: where you want results
write.csv(sclasscount, "sclasscount.csv")   
write.csv(physcount, "physcount.csv")  
write.csv(nonrefcheck2_W, "nonrefcheck2_W.csv")  
write.csv(nonrefcheck2_E, "nonrefcheck2_E.csv")  
write.csv(sparsebpscheck, "sparsebpscheck.csv")  
write.csv(agcheck, "agcheck.csv")  
write.csv(sparsecheck, "sparsecheck .csv")  
write.csv(snowicecheck, "snowicecheck.csv")  
write.csv(uecheck3, "uecheck3.csv")  
write.csv(urbancheck, "urbancheck.csv")  
write.csv(watercheck, "watercheck.csv")  
write.csv(bpsrulescheck, "bpsrulescheck.csv")  

# *************************************************************
# CHECKING THE OUTPUT
# *************************************************************
#The output tables will be blank if the sclass rules were applied correctly except in these tables:
#sclasscount, physcount, 
#nonrefcheck2_W, nonrefcheck2_E, 
#sparsebpscheck, bpsrulescheck.

#Using the small sample dbf provided with this code, even the tables listed above could be blank b/c
#the criteria were not met. The criteria for these tables will be met when checking a CONUS wide combineed
#sclass table. 

