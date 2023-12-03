install.packages(c("ggplot2", "ggpubr", "tidyverse", "broom", "AICcmodavg"))

#Looking at Distribution of Program Director Race and sex
#Program Directors Urology 2020
ProDRaceSex <- read_excel("Deidentified Urology Programs and Residents.xlsx", 3)
View(ProDRaceSex)



##Using edited data set to simplify labels
#Edited Medical Schools by Race and Sex 
EDMSRaceSex <- read_excel("EditedAppMSandPG.xlsx", 1)
View(EDMSRaceSex)

#Edited Programs by Race and Sex
EDPGRaceSex <- read_excel("EditedAppMSandPG.xlsx", 2)
View(EDPGRaceSex)  


#Looking at Medical schools with black applicants
#Subset first


SubsetMS <-subset(EDMSRaceSex, "Black" == 1)
View(SubsetMS)
sum(SubsetMS)


#Looking at based on date 2019-2020 ( because have PD data from 2020) (most recent pre covid)
EDMSRaceSex %>%
  filter(year == "2019-2020")%>%
  filter(Black > 0) %>%
  count()

# Looking at based on race and total applicants
PracticeEDMSRaceSex <- read_excel("EditedAppMSandPG.xlsx", 1) %>%
  select(Black, Total)

plot(PracticeEDMSRaceSex)
stat(PracticeEDMSRaceSex)


# Looking at based on race and year applicants
YearEDMSRaceSex <- read_excel("EditedAppMSandPG.xlsx", 1) %>%
  select(Black, Year)


# How many programs by year had Black Applicants 0,1,2,3
table(EDMSRaceSex$Black, EDMSRaceSex$Year)
summary(EDMSRaceSex$Black, EDMSRaceSex$Year)
View(EDMSRaceSex$Black, EDMSRaceSex$Year)
View(YearEDMSRaceSex)

 
#Anova of Black v.s Year
one.way <- aov(YearEDMSRaceSex$Year ~ YearEDMSRaceSex$Black, data = TRUE )

summary(one.way)


  ##Want rows where Black > %>%
count(EDMSRaceSex)
