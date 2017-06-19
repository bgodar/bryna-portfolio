# DATA PROCESSING
# Read in data
setwd("~/Coursera/Storm")
st <- read.csv("repdata%2Fdata%2FStormData.csv.bz2", stringsAsFactors = FALSE)

# Subset data to pull out state, event type, begin date, fatalities, injuries, 
# crop damage and property damage
st1 <- st[, c(1, 2, 7, 8, 23:28)]

# Convert BGN_DATE to Date class and subset data after Jan. 1996
# The NWS didn't start recording all events until 1996.
library(dplyr)
st1$BGN_DATE <- as.Date(st1$BGN_DATE, "%m/%d/%Y %H:%M:%S")
st1 <- st1 %>% mutate(Year = format(st1$BGN_DATE, "%Y")) %>%
        filter(Year >= 1996)

# Temporarily removed this:
# Get fips decoding
# library(xlsx)
# fips <- read.xlsx2("fips_codes_website.xls", sheetIndex = 1)
# Filter data so only the 52 state codes in fips are included (look into this...)
# st1 <- filter(st1, STATE %in% fips$State.Abbreviation)

# Make all EVTYPE upper case
st1$EVTYPE <- lapply(st1$EVTYPE, function(v){
        if (is.character(v)) return(toupper(v))
        else return(v)
    })
# Remove summary EVTYPE data and the spaces at the start of each line
msk <- grepl("SUMMARY", st1$EVTYPE)
st1 <- st1[!msk, ]
st1$EVTYPE <- gsub("^ ", "", st1$EVTYPE)

# Across the US, which types of events (EVTYPE) are most harmful with respect
# to population health?

# Find which event types had fatalities and subset st1 for only those with more
# than 24 fatalities (the mean is 24.27)
deaths <- st1 %>% group_by(EVTYPE) %>% summarise(fatalities = 
            sum(FATALITIES)) %>% filter(fatalities > 24)
q1 <- filter(st1, EVTYPE %in% deaths$EVTYPE)

# Replace unofficial event types with the official name. Landslide was rennamed 
# to "Debris Flow"; "Fog" is either Dense Fog or Freezing Fog, didn't have 
# enough information to alter; "Urban/sml stream fld" looks like a flash flood, 
# but leaving as is for now.
q1$EVTYPE <- gsub("EXTREME COLD$", "EXTREME COLD/WIND CHILL", q1$EVTYPE)
q1$EVTYPE <- gsub("HEAVY SURF/HIGH SURF", "HIGH SURF", q1$EVTYPE)
q1$EVTYPE <- gsub("HURRICANE$", "HURRICANE/TYPHOON", q1$EVTYPE)
q1$EVTYPE <- gsub("RIP CURRENTS", "RIP CURRENT", q1$EVTYPE)
q1$EVTYPE <- gsub("WINTER WEATHER/MIX", "WINTER WEATHER", q1$EVTYPE)
q1$EVTYPE <- gsub("TSTM WIND", "THUNDERSTORM WIND", q1$EVTYPE)
q1$EVTYPE <- gsub("LANDSLIDE", "DEBRIS FLOW", q1$EVTYPE)
q1$EVTYPE <- gsub("URBAN/SML STREAM FLD", "FLASH FLOOD", q1$EVTYPE)

# ANALYSIS
# Excessive Heat, Tornadoes and Flash Floods have the highest number of
# fatalities from 1996 to 2011.
q1 <- group_by(q1, EVTYPE)
fi <- q1 %>% summarise(Fatalities = sum(FATALITIES), Injuries = 
            sum(INJURIES), Frequency = n()) %>% arrange(desc(Fatalities)) %>%
            rename("EventType" = EVTYPE)

head(fi, 10)
# Tornadoes, Floods and Excessive Heat meanwhile have the highest number of
# injuries from 1996 to 2011.
fi <- arrange(fi, desc(Injuries))
head(fi, 10)

# When examining the impact per occurence of each event, excessive heat still 
# has the highest rate of fatalities, at an average of 1.09 per occurence. Other 
# events rise into the second and third positions, however, with rip current and 
# avalanche coming in second and third with an average 0.79 and 0.59 fatalities 
# per occurence, respectively.
fi2 <- fi %>% mutate(FatalityRate = Fatalities/Frequency, InjuryRate = 
                         Injuries/Frequency) %>% arrange(desc(FatalityRate))
head(fi2, 10)

# Plot top 5 fatality rates FINISH THIS
library(ggplot2)
fi5 <- fi2[1:5, ]

# Looking at injury rates, tsunamis, hurricanes and typhoons, and excessive heat
# cause the most injuries per event.
fi2 <- arrange(fi2, desc(InjuryRate))
head(fi2, 10)

# Across the US, which types of events have the greatest economic consequences?

# Create new columns with total propdmg and cropdmg using exponent column
# To calcuate the exponents, create two vectors p and c with the number
# corresponding to the exponent letter.
exp <- function(v) {
    B <- grepl("B", v)
    K <- grepl("K", v)
    M <- grepl("M", v)
    v[B] <- 1000000000
    v[K] <- 1000
    v[M] <- 1000000
    v[!B & !K & !M] <- 1
    as.numeric(v)
}
p <- exp(st1$PROPDMGEXP)
c <- exp(st1$CROPDMGEXP)
st1 <- mutate(st1, TotalPROPDMG = p*PROPDMG, TotalCROPDMG = c*CROPDMG)

# Find which event types had property and crop damage and subset st1 for only 
# those with greater than the mean property or crop damage (1.008e+09 and
# 9.547e+07, respectively).
pdamage <- st1 %>% group_by(EVTYPE) %>% summarise(propdmg = 
                sum(TotalPROPDMG)) %>% filter(propdmg > 1.008e+09)
cdamage <- st1 %>% group_by(EVTYPE) %>% summarise(cropdmg = 
                sum(TotalCROPDMG)) %>% filter(cropdmg > 9.547e+07)
q2 <- filter(st1, EVTYPE %in% pdamage$EVTYPE | EVTYPE %in% cdamage$EVTYPE)

# Replace unofficial event types with the official name. Landslide was rennamed 
# to "Debris Flow"; "Fog" is either Dense Fog or Freezing Fog, didn't have 
# enough information to alter; "Urban/sml stream fld" looks like a flash flood, 
# but leaving as is for now.
q2$EVTYPE <- gsub("EXTREME COLD$", "EXTREME COLD/WIND CHILL", q2$EVTYPE)
q2$EVTYPE <- gsub("^FREEZE", "FROST/FREEZE", q2$EVTYPE)
q2$EVTYPE <- gsub("HURRICANE$", "HURRICANE/TYPHOON", q2$EVTYPE)
q2$EVTYPE <- gsub("STORM SURGE$", "STORM SURGE/TIDE", q2$EVTYPE)
q2$EVTYPE <- gsub("WILD/FOREST FIRE", "WILDFIRE", q2$EVTYPE)
q2$EVTYPE <- gsub("TSTM WIND", "THUNDERSTORM WIND", q2$EVTYPE)

# ANALYSIS
damage <- q2 %>% group_by(EVTYPE) %>% summarise(PropertyDmg = sum(TotalPROPDMG),
            CropDmg = sum(TotalCROPDMG), Frequency = n()) %>%
            mutate(TotalDamage = CropDmg + PropertyDmg) %>%
            arrange(desc(TotalDamage)) %>% rename("EventType" = EVTYPE)
head(damage)

# Floods, Hurricane/Typhoons and Storm Surge/Tides have caused the greatest
# property damage from 1996 to 2011.
damageP <- arrange(damage, desc(PropertyDmg))
head(damageP, 10)
ptop5 <- damageP[1:5, ]
g <- ggplot(ptop5, aes(x = EventType, y = PropertyDmg))
g + geom_bar(stat = "identity", fill = "#99d8c9") + labs(x = "Event Type", 
                            y = "Property Damage (in dollars)") +
    theme(axis.text.x = element_text(angle = 15, size = 7)) +
    ggtitle('Top 5 Storm Causes of Property Damage')

# Drought has meanwhile caused the most crop damage, with Hurricane/Typhoons
# and floods still in the top three.
damageC <- arrange(damage, desc(CropDmg))
head(damageC, 10)
ctop5 <- damageC[1:5, ]
h <- ggplot(ctop5, aes(x = EventType, y = CropDmg))
h + geom_bar(stat = "identity", fill = "steel blue") + labs(x = "Event Type", 
    y = "Crop Damage (in dollars)") +
    theme(axis.text.x = element_text(angle = 15, size = 7)) +
    ggtitle('Top 5 Storm Causes of Crop Damage')


# When examining the impact per occurence of each event, floods drop down to
# fourth, with Hurricane/Typoohns and Storm Surge/Tides taking 1 and 2. Tropical
# Storms also bump up to the top three.
damage <- damage %>% mutate(TotalDmgRate = TotalDamage/Frequency) %>% 
                        arrange(desc(TotalDmgRate))
head(damage)

