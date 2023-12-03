# Git hub iD
git config --global user.email "dfdelbrune@gmail.com"
git config --global user.name "DDelbrune"

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


####Data sets for work getting individual sheets
#Medical Schools by Race and Sex
MSRaceSex <- read_excel("Urology ERAS Applicants by Medical School and Program.xlsx", 1)
View(MSRaceSex)
  
#Programs by Race and Sex
PGRaceSex <- read_excel("Urology ERAS Applicants by Medical School and Program.xlsx", 2)
View(PGRaceSex)  

# Entering Residents Race and sex
EntResRaceSex <- read_excel("Identified Urology Programs and Residents.xlsx", 1)
View(EntResRaceSex)  

#Completed Residents Race and sex
CompResRaceSex <-read_excel("Identified Urology Programs and Residents.xlsx", 2)
View(CompResRaceSex)  

# Active Residents Race and Sex
ActResRaceSex <- read_excel("Deidentified Urology Programs and Residents.xlsx", 1)
View(ActResRaceSex)

#Transfers into urology Race and Sex
TranResRaceSex <- read_excel("Deidentified Urology Programs and Residents.xlsx", 2)
View(TranResRaceSex)

#Program Directors Urology 2020
ProDRaceSex <- read_excel("Deidentified Urology Programs and Residents.xlsx", 3)
View(ProDRaceSex)

  










