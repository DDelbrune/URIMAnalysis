#New attempt at matching Programs

##Using data set with 4 pages (Edited2019uropairing.xlsx)
#Page1 Uro Pairing sheet (MS with Residency)
#Page2 2019 MS who appplied Uro from each program
#Page3 2019 Demographics of Med students who applied to each program
#Page4 Individual Program Resident Demographics
#Page5 2013-2019 Residents who matched demographics
#Page 6 = 2020 PD Demographics
#Page 7 = Faculty.Race.by.School.and.Year
#Page 8 = Schools.Public.Private.AUA
#Page 9 = ChairsbySexandRace 
#Page 10 = Entire Demographics of MS who applied from each school by year
#Page 11 = Entire demographics of MS who applied Per Program
#Page 12 = current Resident demographics per year 
#page 13= Entire entering residents Demo
#Page 14= Completed residency demo



git config:: user.email "dfdelbrune@gmail.com"
git config:: user.name "Devante Delbrune"



#Packages to Install
install.packages("XLConnect")
install.packages("tidyverse")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("htmlwidgets")
install.packages ("readxl")


#Run packages
library("XLConnect")
library("tidyverse")
library("dplyr")
library("ggplot2")
library("htmlwidgets")
library("readxl")

#import data sets
#Uro Pairing sheet (MS with Residency) page 1
MS.PG.Pair <- read_excel("EDITED2019UROPairing.xlsx", 1)
View(MS.PG.Pair)



#2019 MS who appplied Uro from each program
INDMSPerPRogram2019MS <- read_excel("EDITED2019UROPairing.xlsx", 2)
View(INDMSPerPRogram2019MS)



#Page3 2019 Demographics of Med students who applied to each program
Apply2019MS <- read_excel("EDITED2019UROPairing.xlsx", 3)
View(Apply2019MS)


#Individual Program Resident Demographics
INDPGRES <- read_excel("EDITED2019UROPairing.xlsx", 4)
View(INDPGRES)


#Page5 2013-2019 Residents who matched demographics
Res2013.2019Demo <- read_excel("EDITED2019UROPairing.xlsx", 5)
View(Res2013.2019Demo)


#Page 6 = 2020 PD Demographics
PDDEMO2020 <- read_excel("EDITED2019UROPairing.xlsx", 6)
View(PDDEMO2020)


#Page 7 = Faculty.Race.by.School.and.Year
Faculty.Race.School.Yr <- read_excel("EDITED2019UROPairing.xlsx", 7)
View(Faculty.Race.School.Yr)

#Page 8 = Schools.Public.Private.AUA
School.pri.pub <- read_excel("EDITED2019UROPairing.xlsx", 8)
View(School.pri.pub)

#Page 9 = ChairsbySexandRace 
Chair.sex.Race <- read_excel("EDITED2019UROPairing.xlsx", 9)
View(Chair.sex.Race)


#Page 10 = Entire Demographics of MS who applied from each school by year
Entire.MS.Apply.Per.Sch <- read_excel("EDITED2019UROPairing.xlsx", 10)
View(Entire.MS.Apply.Per.Sch)

#Page 11 = Entire demographics of MS who applied Per Program
Entire.MS.Apply.Res <- read_excel("EDITED2019UROPairing.xlsx", 11)
View(Entire.MS.Apply.Res)

#Page 12 = current Resident demographics per year 
Res.Demo.Per.Year <- read_excel("EDITED2019UROPairing.xlsx", 12)
View(Res.Demo.Per.Year)

#page 13= Entire entering residents Demo
Entering.Res.Demo <- read_excel("EDITED2019UROPairing.xlsx", 13)
View(Entering.Res.Demo)

#Page 14= Completed residency demo
Comp.Residents <- read_excel("EDITED2019UROPairing.xlsx", 14)
View(Comp.Residents)










#Tier programs from 2019 based on # of black applicants
# use data apply2019
# see if statistically significant difference in amount of black students applying to programs
# look at average black applicant v.s total number of applicants
library(dplyr)
group_by(Apply2019MS, Total) %>%
  summarise(
    count = n(),
    mean = mean(Black, na.rm = TRUE),
    sd = sd(Black, na.rm = TRUE)
  )

# Compute the analysis of variance
res.aov <- aov(Black ~ Total, data = Apply2019MS)
# Summary of the analysis
summary(res.aov)




# Compare how many applicants were black based on resident demographics
# use Apply2019MS
#Use INDPGRes
# Matched them to the residency program
matching_ApplyMS.Res <- inner_join(Apply2019MS, INDPGRES, by = "Program")
print(matching_ApplyMS.Res)
View(matching_ApplyMS.Res)'

#Black.x= medical student applying'
#Black.y = Residents
# looking for linear regression

lmBlackMSPerRes = lm(Black.x~Black.y, data = matching_ApplyMS.Res) #Create the linear regression


View(lmBlackMSPerRes)

##Looking at MS dependent upon residents
lmBlackResPerMS = lm(Black.y~Black.x, data = matching_ApplyMS.Res)
View(lmBlackResPerMS)

#Graphing linear regression
library(ggplot2)

PlotRegressionBlackMSPerRes <- 
  ggplot(matching_ApplyMS.Res, aes(x = Black.x, y = Black.y)) + 
  geom_point() +
  stat_smooth(method = "lm", col = "red") 


# Renamed plot for adjusting title, variables
BMSPerBRes <- PlotRegressionBlackMSPerRes + 
  scale_x_continuous(name = "# of Black Residency Applicants") +
  scale_y_continuous(name = "# of Black Residents") +
  ggtitle("# Black Residency Applicants per # of Black Residents")

plot(BMSPerBRes)  
  

  
 

#Look at Demographic make up of top programs



## look at MSm -> Residents overtime -
# Look at all demographics- seperate (based on urim at points)
# URIM - definition AAMC- (look up)
# Black, Alaskan, Hawaiian, Latinx/Hispanic,
# Exclude unknown/multiple from comparison-
# November 1st- 11am EST
# Look up recent publications, Like beget like- secondary outcome of paper
# look at other specialties, work within field what we are adding.

