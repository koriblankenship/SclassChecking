#Read in an sclass combine table (BpS, EVT, EVC, EVH, sclass) and join the attributes.
#This is used to understand the sclass combinations and rules classification. 

library(foreign)

#BRING IN THE REQUIRED DATA
#combine dbf
combine <- read.dbf("./IN/combinetable.dbf")

#individual layer VATs
bps <- read.csv("./IN/LF_200BPS_09172020.csv")
evt <- read.csv("./IN/LF20_EVT_220.csv")
evc <- read.csv("./IN/LF20_EVC_220.csv")
evh <- read.csv("./IN/LF20_EVH_220.csv")
sclass <- read.csv("./IN/LF16_SCla_200.csv")

## JOIN
join <- merge(x = combine, y = bps, by.x = "LC16_BPS_2", by.y = "VALUE")
join <- join[ -c(9,10,13:27)] #remove fields I don't need

join <- merge(x = join, y = evt, by.x = "CONUS_EVT", by.y = "VALUE")
join <- join[ -c(12:14, 17:28)] #remove fields I don't need

join <- merge(x = join, y = evc, by.x = "CONUS_EVC", by.y = "VALUE")
join <- join[ -c(15:20)] #remove fields I don't need
names(join)[names(join)=="CLASSNAMES"] <- "COVER" #rename the evc column

join <- merge(x = join, y = evh, by.x = "CONUS_EVH", by.y = "VALUE")
join <- join[ -c(16:21)] #remove fields I don't need
names(join)[names(join)=="CLASSNAMES"] <- "HEIGHT" #rename the evh column

join <- merge(x = join, y = sclass, by.x = "CONUS_SCLA", by.y = "VALUE")
join <- join[ -c(17:23)] #remove fields I don't need
names(join)[names(join)=="LABEL"] <- "SCLASS" #rename the evh column

# WRITE DBF W/ JOINED ATTRIBUTES 
#write.dbf(join, "sclasscomb2020joined.dbf")  