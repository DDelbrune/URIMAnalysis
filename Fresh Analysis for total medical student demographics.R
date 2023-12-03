# Group Students based on URM status
# Based on year and see if difference in URM application total

##Using data set with 6 pages (Edited2019uropairing.xlsx)
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


#Packages to Install
install.packages("XLConnect")
install.packages("tidyverse")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("htmlwidgets")
install.packages ("readxl")
install.packages("arsenal")


#Run packages
library("XLConnect")
library("tidyverse")
library("dplyr")
library("ggplot2")
library("htmlwidgets")
library("readxl")
library("arsenal")



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



#Look at Demographics of Applying MS per Res look at URM v.s Non
#Group URM and non URM
# URM = Black, Hawaiian, Hispanic, Indian (American Indian),
#Page 11 = Entire demographics of MS who applied Per Program
Entire.MS.Apply.Res <- read_excel("EDITED2019UROPairing.xlsx", 11)
View(Entire.MS.Apply.Res)

# Group by on multiple columns
URM.Entire.MS.Apply.Res <- Entire.MS.Apply.Res %>% 
  group_by(Black,Indian,Hawaiian,Hispanic,Year,School) %>% 
  summarise(total_apply.URM=sum(Total))
view(URM.Entire.MS.Apply.Res)


Non.URM.Entire.MS.Apply.Res<- Entire.MS.Apply.Res %>% 
  group_by(White,Asian,Year,School) %>% 
  summarise(total_apply.nonURM=sum(Total))
view(Non.URM.Entire.MS.Apply.Res)



#Linear regression of URM MS by Year
lmyear.MS = lm(total_apply.URM~Year, data = URM.Entire.MS.Apply.Res) #Create the linear regression
summary(lmyear.MS) #Review the results
plot(lmyear.MS)



#Linear regression of URM by School
lmschool.MS = lm(total_apply.URM~School, data = URM.Entire.MS.Apply.Res) #Create the linear regression
summary(lmschool.MS)
plot(lmschool.MS)

#Linear regression of URM by School and year
lmschool.Year.MS = lm(total_apply.URM~School+Year, data = URM.Entire.MS.Apply.Res) #Create the linear regression
summary(lmschool.Year.MS)
plot(lmschool.Year.MS)

cor(School,Year, method= "pearson")
