#title: Join sclass attributes
#author: Kori Blankenship
#date: 2022-08-07

#Read in an Sclass combine table (BpS, EVT, EVC, EVH, Sclass) and join the attributes. 
#This is used to understand the sclass combinations and rules classification. 
#Under "SETUP" choose the region to process data for: CONUS or HI

## SETUP
# CHOOSE TO PROCESS DATA FOR CONUS OR HI 
#choose_region <- "HI"
choose_region <- "CONUS"
## SETUP END

## LOAD LIBRARIES
library(tidyverse)
library(foreign)

## BRING IN THE REQUIRED DATA
#combine dbf
combine <- read.dbf("./IN/combinetable.dbf")

#individual layer VATs
bps <- read.csv("./IN/LF_200BPS_09172020.csv")
evt <- read.csv("./IN/LF20_EVT_220.csv")
evc <- read.csv("./IN/LF20_EVC_220.csv")
evh <- read.csv("./IN/LF20_EVH_220.csv")
sclass <- read.csv("./IN/LF16_SCla_200_developed.csv")

## MAKE A VARIABLE FOR THE VALUE FIELD OF EACH GRID IN THE SCLASS COMBINE
#This allows you to join the attributes based on the value field.
#The value fields are different for CONUS vs. HI.
#choose_region is used to set variables appropriately.

if (choose_region == "HI") {
  bps_value <- "HI_BPS16"
  evt_value <- "HI_EVT20"
  evc_value <- "HI_EVC22"
  evh_value <- "HI_EVH22"
  sclass_value <- "HI_SCLASS"
} else {
  bps_value <- "LC16_BPS_2"
  evt_value <- "CONUS_EVT"
  evc_value <- "CONUS_EVC"
  evh_value <- "CONUS_EVH"
  sclass_value <- "CONUS_SCLA"
}

## JOIN
#join bps
join <- merge(combine, bps, by.x = bps_value, by.y = "VALUE")
#remove fields I don't need; this varies by region 
if (choose_region == "HI") {
  join <- join[ -c(9,12:26)] 
} else {
  join <- join[ -c(9,10,13:27)]
}

#join evt
join <- merge(join, evt, by.x = evt_value, by.y = "VALUE") 
join <- join[ -c(12:14, 17:28)] #remove fields I don't need

#join evc
join <- merge(join, evc, by.x = evc_value, by.y = "VALUE") 
join <- join[ -c(15:20)] #remove fields I don't need
join <- rename(join, "COVER" = "CLASSNAMES") #rename the evc column

#join evh
join <- merge(join, evh, by.x = evh_value, by.y = "VALUE") 
join <- join[ -c(16:21)] #remove fields I don't need
join <- rename(join, "HEIGHT" = "CLASSNAMES") #rename the evh column

#join sclass
join <- merge(join, sclass, by.x = sclass_value, by.y = "VALUE") 
join <- join[ -c(17:23)] #remove fields I don't need
join <- rename(join, "SCLASS" = "LABEL") #rename the sclass label column

# WRITE DBF W/ JOINED ATTRIBUTES 
#write.dbf(join, "sclasscomb2020joined.dbf")  
