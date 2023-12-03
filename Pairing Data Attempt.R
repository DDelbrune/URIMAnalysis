#New attempt at matching Programs

##Using data set with 6 pages (Edited2019uropairing.xlsx)
#Page1 Uro Pairing sheet (MS with Residency)
#Page2 2019 MS who appplied Uro from each program
#Page3 2019 Demographics of Med students who applied to each program
#Page4 Individual Program Resident Demographics
#Page5 2013-2019 Residents who matched demographics
#Page 6 = 2020 PD Demographics



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
dr5t88888888888888888888i9tfrrrrrr

View(lmBlackMSPerRes)
plot(lmBlackMSPerRes)

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


