# Getting all data on ome spreadsheet and pairing them
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

# Data set URIMPrograms
#1. Matching.Residency.Schools.Leng
#2. Active.Residents.Per.year
#3. MS.Apply.Per.Residency 
#4. Entering.Residents (grouped ACGME ID also combined years)
#5. Completed.Residency.Demo.groupe (grouped ACGME ID also combined years)
#6 Edited.Master.Sheet of school INFO
#7 Faculty Associated with Medical School demographics


# Starting with pairing based on program
read_excel("URIMPrograms.xlsx")

#Page 1 = #1. Matching.Residency.Schools.Leng
MainpagewithID<- read_excel("URIMPrograms.xlsx", 1)
View(MainpagewithID)

#2. Active.Residents.Per.year
Active.Residents.Per.Year <- read_excel("URIMPrograms.xlsx", 2)
View(Active.Residents.Per.Year)



#3. MS.Apply.Per.Residency 
Medical.Student.Apply.Per.Res <- read_excel("URIMPrograms.xlsx", 3)
View(Medical.Student.Apply.Per.Res)


#Page 6 = #6.Edited.Master.Sheet of school INFO
Edited.MainpagewithID<- read_excel("URIMPrograms.xlsx", 6)
View(Edited.MainpagewithID)


#7 Faculty Associated with Medical School demographics
Faculty.MS.Demo<- read_excel("URIMPrograms.xlsx", 7)
View(Faculty.MS.Demo)



#Merge data base on Res ID
MergedMain.Active.Res<-merge(Edited.MainpagewithID,Active.Residents.Per.Year,
                             by.x = "Res.Program.ID")
view(MergedMain.Active.Res)

## Can use to section based on AUA section (look at resident demo-breakup)


#Must change MS Years 2005-2006 = 2005


#Merge Main+Res+Medical Students
Merged.MS.Active.Main <- merge(MergedMain.Active.Res,
                               Medical.Student.Apply.Per.Res,
                               all.x = TRUE, all.y = TRUE )
view(Merged.MS.Active.Main)



#Merge Main+Res+Medical Students+ Faculty associated with School
Merged.MS.Active.Faculty <- merge(Merged.MS.Active.Main,
                                  Faculty.MS.Demo,
                                  all.x = TRUE, all.y = TRUE )
view(Merged.MS.Active.Faculty)


# Adding a new colum combining Resident demographics with Faculty
#Adding a new column combing Faculty and Resident Percentage URIM
Merged.MS.Active.Faculty$Combined.Faculty.Res <- with(Merged.MS.Active.Faculty, Percent.Resident.URM + URIM.Faculty.Proportion)
View(Merged.MS.Active.Faculty)



# Make new column for each table , (Percentage currently a proportion) x100


# Look at correlation of medical student applicants to programs based on
##Resident Demographics *** Is this matched per group without Fauclty
#Linear Regression
lmRes.MS.Apply = lm(Percent.Applicant.URM~Percent.Resident.URM, data =Merged.MS.Active.Main)
summary(lmRes.MS.Apply) #Review the results

# Look at do Percentage of URM applicants differ based on Programs
Multiple.lm.Res.MS.Apply <- lm(cbind(Percent.Applicant.URM) ~ 
                                 Percent.Resident.URM + AUA.REGION + Year + LENGTH.OF.PROGRAM + RESEARCH.TRACK, 
                               data = Merged.MS.Active.Main)
summary(Multiple.lm.Res.MS.Apply)




# Look at Mutliple vairables including combined Faculty with residents
Multiple2.lm.Res.MS.Apply <- lm(cbind(Percent.Applicant.URM) ~ 
                                  Combined.Faculty.Res+ Percent.Resident.URM + AUA.REGION + Year + LENGTH.OF.PROGRAM + RESEARCH.TRACK, 
                                data = Merged.MS.Active.Faculty)
summary(Multiple2.lm.Res.MS.Apply)


# Linear regresison for combined program demographics and impact on where students
# apply for residency
Combined.FacultylmRes.MS.Apply = lm(Percent.Applicant.URM~Combined.Faculty.Res, data = Merged.MS.Active.Faculty)
summary(Combined.FacultylmRes.MS.Apply) #Review the results
