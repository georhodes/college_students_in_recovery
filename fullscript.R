

library("rmarkdown")
library("foreign")
library("gmodels")
library("dplyr")

#import data
data157 <- read.csv("~/Documents/School/UW/MSW/506/research/Rcsrad/Rcsrad/CCHPdata157.csv")



#EXPLORING DATA
table(data157$ARECOVERY)
table(data157$ACOLLEGE)
CrossTable(data157$ARECOVERY, data157$ACOLLEGE)
CrossTable(data157$ARECOVERY, data157$AGENDER)
CrossTable(data157$ARECOVERY, data157$ASEX)
CrossTable(data157$ARECOVERY, data157$ABIRTHSEX)

CrossTable(datadiasex$ARECOVERY, datadiasex$ASEX)


#CLEANING
##TWO GROUPS BASED ON ARECOVERY 0=NO, 1=YES

##DEMOGRAPHICS

##MENTAL HEALTH
### AGAD1 AND AGAD2
chitest_AGAD1 <- chisq.test(data157$ARECOVERY, data157$AGAD1, correct=FALSE)
chitest_AGAD1
chitest_AGAD1 <- chisq.test(data157$ARECOVERY, data157$AGAD1, correct=TRUE)
chitest_AGAD1


chitest_AGAD2 <- chisq.test(data157$ARECOVERY, data157$AGAD2, correct=FALSE)
chitest_AGAD2


##PHYSICAL HEALTH


##SERVICES
###need to clean services... combine into single variable
CrossTable(data157$ARECOVERY, data157$ASERVICES1)



CrossTable(data157$ARECOVERY, data157$AWITHDRAW7)
table(data157$AAGE)

table(data157$AEDSTATUS)
CrossTable(data157$ARECOVERY, data157$AEDSTATUS)
CrossTable(data157$ARECOVERY, data157$ACOLLEGE)
CrossTable(data157$ARECOVERY, data157$AEDSTATUS)

#SPLIT DATA BY ARECOVERY
recovery_yes <- data157 %>%
  filter(ARECOVERY == 1)

recovery_no <- data157 %>%
  filter(ARECOVERY == 0)


#stats
##  Chi_Squared chisq.test()

# Chi-sq test
chitest2 <- chisq.test(data157$ARECOVERY, data157$AWITHDRAW7, correct=FALSE)
chitest2

##chronbach's alpha: Are individuals consistently rating/interpretting likert scare similarly. 

#MOSAICS
##Withdraw 
mosaic_withdraw7 <- data157%>% #KEY POPULATION
  select(ARECOVERY, AWITHDRAW7)   #Key VARIABLES

mosaicplot(table(mosaic_withdraw7), shade = TRUE, las = 4)



##SERVICES found no color.
###FILTER OUT NA'S FROM SERVICES, LEAVES 508 TOATAL, 50 IN RECOVERY
data_filtered<- data157 %>%
  filter(ASERVICES1 <= 3) %>%
  filter(ASERVICES2 <= 3) %>%
  filter(ASERVICES3 <= 3) %>%
  filter(ASERVICES4 <= 3) %>%
  filter(ASERVICES5 <= 3) %>%
  filter(ASERVICES6 <= 3) %>%
  filter(ASERVICES7 <= 3) %>%
  filter(ASERVICES8 <= 3) %>%
  filter(ASERVICES9 <= 3) %>%
  filter(ASERVICES10 <= 3) %>%
  filter(ASERVICES11 <= 3)

CrossTable(data_filtered$ARECOVERY, data_filtered$ASERVICES1)

###SERVICES1
mosaic_services1 <- data_filtered %>% #KEY POPULATION
  select(ARECOVERY, ASERVICES1)   #Key VARIABLES

mosaicplot(table(mosaic_services1), shade = TRUE, las = 4)

###SERVICES2
mosaic_services2 <- data_filtered %>% #KEY POPULATION
  select(ARECOVERY, ASERVICES2)   #Key VARIABLES

mosaicplot(table(mosaic_services2), shade = TRUE, las = 4)

###SERVICES3
mosaic_services3<- data_filtered %>% #KEY POPULATION
  select(ARECOVERY, ASERVICES3)   #Key VARIABLES

mosaicplot(table(mosaic_services3), shade = TRUE, las = 4)

###SERVICES4
mosaic_services4 <- data_filtered %>% #KEY POPULATION
  select(ARECOVERY, ASERVICES4)   #Key VARIABLES

mosaicplot(table(mosaic_services4), shade = TRUE, las = 4)

#SERVICES5
mosaic_services5 <- data_filtered %>% #KEY POPULATION
  select(ARECOVERY, ASERVICES5)   #Key VARIABLES

mosaicplot(table(mosaic_services5), shade = TRUE, las = 4)

###SERVICES6
mosaic_services6 <- data_filtered %>% #KEY POPULATION
  select(ARECOVERY, ASERVICES6)   #Key VARIABLES

mosaicplot(table(mosaic_services6), shade = TRUE, las = 4)

###SERVICES7
mosaic_services7 <- data_filtered %>% #KEY POPULATION
  select(ARECOVERY, ASERVICES7)   #Key VARIABLES

mosaicplot(table(mosaic_services7), shade = TRUE, las = 4)

###SERVICES8
mosaic_services8 <- data_filtered %>% #KEY POPULATION
  select(ARECOVERY, ASERVICES8)   #Key VARIABLES

mosaicplot(table(mosaic_services8), shade = TRUE, las = 4)

###SERVICES9
mosaic_services9 <- data_filtered %>% #KEY POPULATION
  select(ARECOVERY, ASERVICES9)   #Key VARIABLES

mosaicplot(table(mosaic_services9), shade = TRUE, las = 4)

###SERVICES10
mosaic_services10 <- data_filtered %>% #KEY POPULATION
  select(ARECOVERY, ASERVICES10)   #Key VARIABLES

mosaicplot(table(mosaic_services10), shade = TRUE, las = 4)

##SERVICES11
mosaic_services11 <- data_filtered %>% #KEY POPULATION
  select(ARECOVERY, ASERVICES11)   #Key VARIABLES

mosaicplot(table(mosaic_services11), shade = TRUE, las = 4)

chisq.test(data157$ARECOVERY, data157$AGENDER)


CrossTable(data157$ARECOVERY, data157$AGENDER)
CrossTable(data157$ARECOVERY, data157$AGENDER, expected = TRUE, chisq = TRUE)
view(data157)
