# Looking at association with residency demographics and 
#Demographics of medical students that apply urology from that institution.

#if there is an association between racial representation of the 
#applicants FROM a medical school and racial representation of the 
#residents at their affiliated urology residency program.


#Run packages
library("XLConnect")
library("tidyverse")
library("dplyr")
library("ggplot2")
library("htmlwidgets")
library("readxl")
library("arsenal")


#Data set URIM.Schools.Pipeline
# Page 1= Deidentifed Schools w/ID and Affil medical school
# Page 2= Resdient demographics
# Page 3 Medical Students Per School'
# Page 4 Faculty Per Medical School
# Page 5 School info pub vs private and section
# Page 6 edited Master Deidentified Schools W/ Affil.MS.ID

# Starting with pairing based on program
read_excel("URIM.Schools.Pipeline.xlsx")

# Page 1= Deidentifed Schools w/ID and Affil medical school
DeID.Page.Schools<- read_excel("URIM.Schools.Pipeline.xlsx", 1)
View(DeID.Page.Schools)

# Page 2= Resident demographics
Res.Demo.Program<- read_excel("URIM.Schools.Pipeline.xlsx", 2)
View(Res.Demo.Program)


# Page 3 Medical Students Per School
MS.Per.School <- read_excel("URIM.Schools.Pipeline.xlsx", 3)
View(MS.Per.School)

# Page 4 Faculty Per Medical School
Faculty.Per.School <- read_excel("URIM.Schools.Pipeline.xlsx", 4)
View(Faculty.Per.School)

# Page 5 School info pub vs private and section
School.Info <- read_excel("URIM.Schools.Pipeline.xlsx", 5)
View(School.Info )

# Page 6 edited Master Deidentified Schools W/ Affil.MS.ID
Edited.Master.DeID <- read_excel("URIM.Schools.Pipeline.xlsx", 6)
View(Edited.Master.DeID)

#Combining Sheets based on School affil ID
Merg.Main.School<-merge(Edited.Master.DeID, MS.Per.School,
                             by.x = "Affil.MS.ID")
view(Merg.Main.School)


#Merging MS+Master with Residents
#Combining Sheets based on School affil ID
#Merge Main+Res+Medical Students
Merged.MS.School.Res <- merge(Merg.Main.School,
                               Res.Demo.Program,
                               all.x = TRUE, all.y = TRUE )
view(Merged.MS.School.Res)


#Merging Schools+MS+Res with faculty demo now
Merged.MS.School.Res.Fact <- merge(Merged.MS.School.Res,
                                   Faculty.Per.School,
                              all.x = TRUE, all.y = TRUE )
view(Merged.MS.School.Res.Fact)



#Merging Schools+MS+Res+Faculty with AUA/Pub/Priv
Merged.MS.School.Res.Fact.Pub.AUA <- merge(Merged.MS.School.Res.Fact,
                                           School.Info,
                                   all.x = TRUE, all.y = TRUE )
view(Merged.MS.School.Res.Fact.Pub.AUA)



#Adding a new column combing Faculty and Resident Percentage URIM
Merged.MS.School.Res.Fact.Pub.AUA$Faculty.Res <- with(Merged.MS.School.Res.Fact.Pub.AUA, URIM.Res.Percentage + URIM.Faculty.Percentage)
View(Merged.MS.School.Res.Fact.Pub.AUA$Faculty.Res)

View(Merged.MS.School.Res.Fact.Pub.AUA)




#Linear regression of MS URIM percentage and Residents from school
lmRes.MS.Res.School = lm(URIM.Percentage.MS~URIM.Res.Percentage, data =Merged.MS.School.Res.Fact.Pub.AUA)
summary(lmRes.MS.Res.School) #Review the results



#Linear Regression MS URIM and Faculty
lmRes.MS.Faculty.School = lm(URIM.Percentage.MS~URIM.Faculty.Percentage, data =Merged.MS.School.Res.Fact.Pub.AUA)
summary(lmRes.MS.Faculty.School) #Review the results



# Linear Regression comparing entire Facutly+Res with Students applying
lmRes.MS.Entire.School = lm(URIM.Percentage.MS~Faculty.Res, data =Merged.MS.School.Res.Fact.Pub.AUA)
summary(lmRes.MS.Entire.School) #Review the results






