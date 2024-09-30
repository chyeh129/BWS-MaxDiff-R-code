####Preparing the raw data####
library(readxl)
library(writexl)
library(dplyr)
library(tidyr)
library(factoextra)
library(psych)
library(support.BWS)
library(crossdes)
library(dfidx)
library(mlogit)

#Importing the raw data file

dataRaw<-read_xlsx(file.choose()) #Appropriate cleaned file

#Write file name you are working on: France Group 1_raw data

colnames(dataRaw)# Check column names before sub-setting the data!!!

dataRaw.clean<-dataRaw %>% 
  select(sys_RespNum,lang,c(MXD1_1_b:MXD2_6_w), c(Nationality:ShoppingLocation_10), sys_MXDVersion_MXD1, sys_MXDVersion_MXD2) %>% 
  filter(!is.na(MXD1_1_b)) %>% 
  filter(!is.na(MXD2_6_w)) %>% 
  mutate(UID=paste(lang,sys_RespNum, sep="_"))

#This data should have 58 variables (columns)

#Save the clean data set after checking the path location and writing the file name manually

write_xlsx(dataRaw.clean, "C:/Users/Tatic/sciebo/Tatic Hartmann/BWS/Data/France/France Group 1_clean.xlsx")

#Split the data into two, one for each BWS task and name them accordingly
colnames(dataRaw.clean)

##Store the first BWS experiment and add unique product identifier to UID
dataBWS1<-dataRaw.clean %>% 
  select(c(MXD1_1_b:MXD1_6_w), sys_MXDVersion_MXD1, UID) %>% 
  mutate(UIDP=paste(UID, "Cheese", sep="_"))

##Store the second BWS experiment and add unique product identifier to UID
dataBWS2<-dataRaw.clean %>% 
  select(c(MXD2_1_b:MXD2_6_w), sys_MXDVersion_MXD2, UID) %>% 
  mutate(UIDP=paste(UID, "F.meat", sep="_"))

#Adding id column as the first column to the left

##Creating id vector
id<-c(1:nrow((dataRaw.clean)))
id

##Writing the vector to the BWS subsets
dataBWS1<-cbind.data.frame(id, dataBWS1)
dataBWS2<-cbind.data.frame(id, dataBWS2)

#Renaming columns

c.names<-c("id", "b1", "w1", "b2", "w2", "b3", "w3", "b4", "w4", "b5", "w5", "b6", "w6", "design", "UID", "UIDP")
colnames(dataBWS1)<-c.names
colnames(dataBWS2)<-c.names

write_xlsx(dataBWS1, "C:/Users/Tatic/sciebo/Tatic Hartmann/BWS/Data/France/France Group 1_BWS_CHEESE.xlsx")
write_xlsx(dataBWS2, "C:/Users/Tatic/sciebo/Tatic Hartmann/BWS/Data/France/France Group 1_BWS_F.MEAT.xlsx")
