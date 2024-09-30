####Conducting the BWS analysis####
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
library(gmnl)

####Loading the clean dataset
data<-read_xlsx(file.choose()) #Open the appropriate BWS_product file
#Write the file name: 

data<-data.frame(data)
is.data.frame(data)

#Getting the design file
design<-read_xlsx(file.choose()) #Please choose Design.xlsx
design<-data.frame(design)
is.data.frame(data)

####Splitting the data and design
data_list<-split(data, f = data$design)
design_list<-split(design[,1:5], f = design$version)

####Conversion X 40####
rp <- colnames(data)[2:13]

d_1<-bws.dataset(
    data = data_list[[1]], # data frame containing response dataset
    response.type = 2, # format of response variables
    choice.sets = design_list[[1]], # choice sets
    design.type = 2, # BIBD
    item.names = NULL, # this could be a vector with item names; NULL gives default ITEM1:ITEMn
    id = "id", # the name of respondent id variable
    response = rp, # the names of response variables
    model = "maxdiff",# the type of dataset created by the function
  )

d_2<-bws.dataset(
  data = data_list[[2]], # data frame containing response dataset
  response.type = 2, # format of response variables
  choice.sets = design_list[[2]], # choice sets
  design.type = 2, # BIBD
  item.names = NULL, # this could be a vector with item names; NULL gives default ITEM1:ITEMn
  id = "id", # the name of respondent id variable
  response = rp, # the names of response variables
  model = "maxdiff",# the type of dataset created by the function
)

d_3<-bws.dataset(
  data = data_list[[3]], # data frame containing response dataset
  response.type = 2, # format of response variables
  choice.sets = design_list[[3]], # choice sets
  design.type = 2, # BIBD
  item.names = NULL, # this could be a vector with item names; NULL gives default ITEM1:ITEMn
  id = "id", # the name of respondent id variable
  response = rp, # the names of response variables
  model = "maxdiff",# the type of dataset created by the function
)

d_4<-bws.dataset(
  data = data_list[[4]], # data frame containing response dataset
  response.type = 2, # format of response variables
  choice.sets = design_list[[4]], # choice sets
  design.type = 2, # BIBD
  item.names = NULL, # this could be a vector with item names; NULL gives default ITEM1:ITEMn
  id = "id", # the name of respondent id variable
  response = rp, # the names of response variables
  model = "maxdiff",# the type of dataset created by the function
)

d_5<-bws.dataset(
  data = data_list[[5]], # data frame containing response dataset
  response.type = 2, # format of response variables
  choice.sets = design_list[[5]], # choice sets
  design.type = 2, # BIBD
  item.names = NULL, # this could be a vector with item names; NULL gives default ITEM1:ITEMn
  id = "id", # the name of respondent id variable
  response = rp, # the names of response variables
  model = "maxdiff",# the type of dataset created by the function
)

d_6<-bws.dataset(
  data = data_list[[6]], # data frame containing response dataset
  response.type = 2, # format of response variables
  choice.sets = design_list[[6]], # choice sets
  design.type = 2, # BIBD
  item.names = NULL, # this could be a vector with item names; NULL gives default ITEM1:ITEMn
  id = "id", # the name of respondent id variable
  response = rp, # the names of response variables
  model = "maxdiff",# the type of dataset created by the function
)

d_7<-bws.dataset(
  data = data_list[[7]], # data frame containing response dataset
  response.type = 2, # format of response variables
  choice.sets = design_list[[7]], # choice sets
  design.type = 2, # BIBD
  item.names = NULL, # this could be a vector with item names; NULL gives default ITEM1:ITEMn
  id = "id", # the name of respondent id variable
  response = rp, # the names of response variables
  model = "maxdiff",# the type of dataset created by the function
)

d_8<-bws.dataset(
  data = data_list[[8]], # data frame containing response dataset
  response.type = 2, # format of response variables
  choice.sets = design_list[[8]], # choice sets
  design.type = 2, # BIBD
  item.names = NULL, # this could be a vector with item names; NULL gives default ITEM1:ITEMn
  id = "id", # the name of respondent id variable
  response = rp, # the names of response variables
  model = "maxdiff",# the type of dataset created by the function
)

d_9<-bws.dataset(
  data = data_list[[9]], # data frame containing response dataset
  response.type = 2, # format of response variables
  choice.sets = design_list[[9]], # choice sets
  design.type = 2, # BIBD
  item.names = NULL, # this could be a vector with item names; NULL gives default ITEM1:ITEMn
  id = "id", # the name of respondent id variable
  response = rp, # the names of response variables
  model = "maxdiff",# the type of dataset created by the function
)

d_10<-bws.dataset(
  data = data_list[[10]], # data frame containing response dataset
  response.type = 2, # format of response variables
  choice.sets = design_list[[10]], # choice sets
  design.type = 2, # BIBD
  item.names = NULL, # this could be a vector with item names; NULL gives default ITEM1:ITEMn
  id = "id", # the name of respondent id variable
  response = rp, # the names of response variables
  model = "maxdiff",# the type of dataset created by the function
)

d_11<-bws.dataset(
  data = data_list[[11]], # data frame containing response dataset
  response.type = 2, # format of response variables
  choice.sets = design_list[[11]], # choice sets
  design.type = 2, # BIBD
  item.names = NULL, # this could be a vector with item names; NULL gives default ITEM1:ITEMn
  id = "id", # the name of respondent id variable
  response = rp, # the names of response variables
  model = "maxdiff",# the type of dataset created by the function
)

d_12<-bws.dataset(
  data = data_list[[12]], # data frame containing response dataset
  response.type = 2, # format of response variables
  choice.sets = design_list[[12]], # choice sets
  design.type = 2, # BIBD
  item.names = NULL, # this could be a vector with item names; NULL gives default ITEM1:ITEMn
  id = "id", # the name of respondent id variable
  response = rp, # the names of response variables
  model = "maxdiff",# the type of dataset created by the function
)

d_13<-bws.dataset(
  data = data_list[[13]], # data frame containing response dataset
  response.type = 2, # format of response variables
  choice.sets = design_list[[13]], # choice sets
  design.type = 2, # BIBD
  item.names = NULL, # this could be a vector with item names; NULL gives default ITEM1:ITEMn
  id = "id", # the name of respondent id variable
  response = rp, # the names of response variables
  model = "maxdiff",# the type of dataset created by the function
)

d_14<-bws.dataset(
  data = data_list[[14]], # data frame containing response dataset
  response.type = 2, # format of response variables
  choice.sets = design_list[[14]], # choice sets
  design.type = 2, # BIBD
  item.names = NULL, # this could be a vector with item names; NULL gives default ITEM1:ITEMn
  id = "id", # the name of respondent id variable
  response = rp, # the names of response variables
  model = "maxdiff",# the type of dataset created by the function
)

d_15<-bws.dataset(
  data = data_list[[15]], # data frame containing response dataset
  response.type = 2, # format of response variables
  choice.sets = design_list[[15]], # choice sets
  design.type = 2, # BIBD
  item.names = NULL, # this could be a vector with item names; NULL gives default ITEM1:ITEMn
  id = "id", # the name of respondent id variable
  response = rp, # the names of response variables
  model = "maxdiff",# the type of dataset created by the function
)

d_16<-bws.dataset(
  data = data_list[[16]], # data frame containing response dataset
  response.type = 2, # format of response variables
  choice.sets = design_list[[16]], # choice sets
  design.type = 2, # BIBD
  item.names = NULL, # this could be a vector with item names; NULL gives default ITEM1:ITEMn
  id = "id", # the name of respondent id variable
  response = rp, # the names of response variables
  model = "maxdiff",# the type of dataset created by the function
)

d_17<-bws.dataset(
  data = data_list[[17]], # data frame containing response dataset
  response.type = 2, # format of response variables
  choice.sets = design_list[[17]], # choice sets
  design.type = 2, # BIBD
  item.names = NULL, # this could be a vector with item names; NULL gives default ITEM1:ITEMn
  id = "id", # the name of respondent id variable
  response = rp, # the names of response variables
  model = "maxdiff",# the type of dataset created by the function
)

d_18<-bws.dataset(
  data = data_list[[18]], # data frame containing response dataset
  response.type = 2, # format of response variables
  choice.sets = design_list[[18]], # choice sets
  design.type = 2, # BIBD
  item.names = NULL, # this could be a vector with item names; NULL gives default ITEM1:ITEMn
  id = "id", # the name of respondent id variable
  response = rp, # the names of response variables
  model = "maxdiff",# the type of dataset created by the function
)

d_19<-bws.dataset(
  data = data_list[[19]], # data frame containing response dataset
  response.type = 2, # format of response variables
  choice.sets = design_list[[19]], # choice sets
  design.type = 2, # BIBD
  item.names = NULL, # this could be a vector with item names; NULL gives default ITEM1:ITEMn
  id = "id", # the name of respondent id variable
  response = rp, # the names of response variables
  model = "maxdiff",# the type of dataset created by the function
)

d_20<-bws.dataset(
  data = data_list[[20]], # data frame containing response dataset
  response.type = 2, # format of response variables
  choice.sets = design_list[[20]], # choice sets
  design.type = 2, # BIBD
  item.names = NULL, # this could be a vector with item names; NULL gives default ITEM1:ITEMn
  id = "id", # the name of respondent id variable
  response = rp, # the names of response variables
  model = "maxdiff",# the type of dataset created by the function
)

d_21<-bws.dataset(
  data = data_list[[21]], # data frame containing response dataset
  response.type = 2, # format of response variables
  choice.sets = design_list[[21]], # choice sets
  design.type = 2, # BIBD
  item.names = NULL, # this could be a vector with item names; NULL gives default ITEM1:ITEMn
  id = "id", # the name of respondent id variable
  response = rp, # the names of response variables
  model = "maxdiff",# the type of dataset created by the function
)

d_22<-bws.dataset(
  data = data_list[[22]], # data frame containing response dataset
  response.type = 2, # format of response variables
  choice.sets = design_list[[22]], # choice sets
  design.type = 2, # BIBD
  item.names = NULL, # this could be a vector with item names; NULL gives default ITEM1:ITEMn
  id = "id", # the name of respondent id variable
  response = rp, # the names of response variables
  model = "maxdiff",# the type of dataset created by the function
)

d_23<-bws.dataset(
  data = data_list[[23]], # data frame containing response dataset
  response.type = 2, # format of response variables
  choice.sets = design_list[[23]], # choice sets
  design.type = 2, # BIBD
  item.names = NULL, # this could be a vector with item names; NULL gives default ITEM1:ITEMn
  id = "id", # the name of respondent id variable
  response = rp, # the names of response variables
  model = "maxdiff",# the type of dataset created by the function
)

d_24<-bws.dataset(
  data = data_list[[24]], # data frame containing response dataset
  response.type = 2, # format of response variables
  choice.sets = design_list[[24]], # choice sets
  design.type = 2, # BIBD
  item.names = NULL, # this could be a vector with item names; NULL gives default ITEM1:ITEMn
  id = "id", # the name of respondent id variable
  response = rp, # the names of response variables
  model = "maxdiff",# the type of dataset created by the function
)

d_25<-bws.dataset(
  data = data_list[[25]], # data frame containing response dataset
  response.type = 2, # format of response variables
  choice.sets = design_list[[25]], # choice sets
  design.type = 2, # BIBD
  item.names = NULL, # this could be a vector with item names; NULL gives default ITEM1:ITEMn
  id = "id", # the name of respondent id variable
  response = rp, # the names of response variables
  model = "maxdiff",# the type of dataset created by the function
)

d_26<-bws.dataset(
  data = data_list[[26]], # data frame containing response dataset
  response.type = 2, # format of response variables
  choice.sets = design_list[[26]], # choice sets
  design.type = 2, # BIBD
  item.names = NULL, # this could be a vector with item names; NULL gives default ITEM1:ITEMn
  id = "id", # the name of respondent id variable
  response = rp, # the names of response variables
  model = "maxdiff",# the type of dataset created by the function
)

d_27<-bws.dataset(
  data = data_list[[27]], # data frame containing response dataset
  response.type = 2, # format of response variables
  choice.sets = design_list[[27]], # choice sets
  design.type = 2, # BIBD
  item.names = NULL, # this could be a vector with item names; NULL gives default ITEM1:ITEMn
  id = "id", # the name of respondent id variable
  response = rp, # the names of response variables
  model = "maxdiff",# the type of dataset created by the function
)

d_28<-bws.dataset(
  data = data_list[[28]], # data frame containing response dataset
  response.type = 2, # format of response variables
  choice.sets = design_list[[28]], # choice sets
  design.type = 2, # BIBD
  item.names = NULL, # this could be a vector with item names; NULL gives default ITEM1:ITEMn
  id = "id", # the name of respondent id variable
  response = rp, # the names of response variables
  model = "maxdiff",# the type of dataset created by the function
)

d_29<-bws.dataset(
  data = data_list[[29]], # data frame containing response dataset
  response.type = 2, # format of response variables
  choice.sets = design_list[[29]], # choice sets
  design.type = 2, # BIBD
  item.names = NULL, # this could be a vector with item names; NULL gives default ITEM1:ITEMn
  id = "id", # the name of respondent id variable
  response = rp, # the names of response variables
  model = "maxdiff",# the type of dataset created by the function
)

d_30<-bws.dataset(
  data = data_list[[30]], # data frame containing response dataset
  response.type = 2, # format of response variables
  choice.sets = design_list[[30]], # choice sets
  design.type = 2, # BIBD
  item.names = NULL, # this could be a vector with item names; NULL gives default ITEM1:ITEMn
  id = "id", # the name of respondent id variable
  response = rp, # the names of response variables
  model = "maxdiff",# the type of dataset created by the function
)

d_31<-bws.dataset(
  data = data_list[[31]], # data frame containing response dataset
  response.type = 2, # format of response variables
  choice.sets = design_list[[31]], # choice sets
  design.type = 2, # BIBD
  item.names = NULL, # this could be a vector with item names; NULL gives default ITEM1:ITEMn
  id = "id", # the name of respondent id variable
  response = rp, # the names of response variables
  model = "maxdiff",# the type of dataset created by the function
)

d_32<-bws.dataset(
  data = data_list[[32]], # data frame containing response dataset
  response.type = 2, # format of response variables
  choice.sets = design_list[[32]], # choice sets
  design.type = 2, # BIBD
  item.names = NULL, # this could be a vector with item names; NULL gives default ITEM1:ITEMn
  id = "id", # the name of respondent id variable
  response = rp, # the names of response variables
  model = "maxdiff",# the type of dataset created by the function
)

d_33<-bws.dataset(
  data = data_list[[33]], # data frame containing response dataset
  response.type = 2, # format of response variables
  choice.sets = design_list[[33]], # choice sets
  design.type = 2, # BIBD
  item.names = NULL, # this could be a vector with item names; NULL gives default ITEM1:ITEMn
  id = "id", # the name of respondent id variable
  response = rp, # the names of response variables
  model = "maxdiff",# the type of dataset created by the function
)

d_34<-bws.dataset(
  data = data_list[[34]], # data frame containing response dataset
  response.type = 2, # format of response variables
  choice.sets = design_list[[34]], # choice sets
  design.type = 2, # BIBD
  item.names = NULL, # this could be a vector with item names; NULL gives default ITEM1:ITEMn
  id = "id", # the name of respondent id variable
  response = rp, # the names of response variables
  model = "maxdiff",# the type of dataset created by the function
)

d_35<-bws.dataset(
  data = data_list[[35]], # data frame containing response dataset
  response.type = 2, # format of response variables
  choice.sets = design_list[[35]], # choice sets
  design.type = 2, # BIBD
  item.names = NULL, # this could be a vector with item names; NULL gives default ITEM1:ITEMn
  id = "id", # the name of respondent id variable
  response = rp, # the names of response variables
  model = "maxdiff",# the type of dataset created by the function
)

d_36<-bws.dataset(
  data = data_list[[36]], # data frame containing response dataset
  response.type = 2, # format of response variables
  choice.sets = design_list[[36]], # choice sets
  design.type = 2, # BIBD
  item.names = NULL, # this could be a vector with item names; NULL gives default ITEM1:ITEMn
  id = "id", # the name of respondent id variable
  response = rp, # the names of response variables
  model = "maxdiff",# the type of dataset created by the function
)

d_37<-bws.dataset(
  data = data_list[[37]], # data frame containing response dataset
  response.type = 2, # format of response variables
  choice.sets = design_list[[37]], # choice sets
  design.type = 2, # BIBD
  item.names = NULL, # this could be a vector with item names; NULL gives default ITEM1:ITEMn
  id = "id", # the name of respondent id variable
  response = rp, # the names of response variables
  model = "maxdiff",# the type of dataset created by the function
)

d_38<-bws.dataset(
  data = data_list[[38]], # data frame containing response dataset
  response.type = 2, # format of response variables
  choice.sets = design_list[[38]], # choice sets
  design.type = 2, # BIBD
  item.names = NULL, # this could be a vector with item names; NULL gives default ITEM1:ITEMn
  id = "id", # the name of respondent id variable
  response = rp, # the names of response variables
  model = "maxdiff",# the type of dataset created by the function
)

d_39<-bws.dataset(
  data = data_list[[39]], # data frame containing response dataset
  response.type = 2, # format of response variables
  choice.sets = design_list[[39]], # choice sets
  design.type = 2, # BIBD
  item.names = NULL, # this could be a vector with item names; NULL gives default ITEM1:ITEMn
  id = "id", # the name of respondent id variable
  response = rp, # the names of response variables
  model = "maxdiff",# the type of dataset created by the function
)

d_40<-bws.dataset(
  data = data_list[[40]], # data frame containing response dataset
  response.type = 2, # format of response variables
  choice.sets = design_list[[40]], # choice sets
  design.type = 2, # BIBD
  item.names = NULL, # this could be a vector with item names; NULL gives default ITEM1:ITEMn
  id = "id", # the name of respondent id variable
  response = rp, # the names of response variables
  model = "maxdiff",# the type of dataset created by the function
)

####All converted####
DATA.full<-rbind(d_1,d_2,d_3,d_4,d_5,d_6,d_7,d_8,d_9,d_10,
            d_11,d_12,d_13,d_14,d_15,d_16,d_17,d_18,d_19,d_20,
            d_21,d_22,d_23,d_24,d_25,d_26,d_27,d_28,d_29,d_30,
            d_31,d_32,d_33,d_34,d_35,d_36,d_37,d_38,d_39,d_40)

str(DATA.full)


####Counting method####

c_1<- bws.count(d_1, cl = 2)
c_2<- bws.count(d_2, cl = 2)
c_3<- bws.count(d_3, cl = 2)
c_4<- bws.count(d_4, cl = 2)
c_5<- bws.count(d_5, cl = 2)
c_6<- bws.count(d_6, cl = 2)
c_7<- bws.count(d_7, cl = 2)
c_8<- bws.count(d_8, cl = 2)
c_9<- bws.count(d_9, cl = 2)
c_10<- bws.count(d_10, cl = 2)
c_11<- bws.count(d_11, cl = 2)
c_12<- bws.count(d_12, cl = 2)
c_13<- bws.count(d_13, cl = 2)
c_14<- bws.count(d_14, cl = 2)
c_15<- bws.count(d_15, cl = 2)
c_16<- bws.count(d_16, cl = 2)
c_17<- bws.count(d_17, cl = 2)
c_18<- bws.count(d_18, cl = 2)
c_19<- bws.count(d_19, cl = 2)
c_20<- bws.count(d_20, cl = 2)
c_21<- bws.count(d_21, cl = 2)
c_22<- bws.count(d_22, cl = 2)
c_23<- bws.count(d_23, cl = 2)
c_24<- bws.count(d_24, cl = 2)
c_25<- bws.count(d_25, cl = 2)
c_26<- bws.count(d_26, cl = 2)
c_27<- bws.count(d_27, cl = 2)
c_28<- bws.count(d_28, cl = 2)
c_29<- bws.count(d_29, cl = 2)
c_30<- bws.count(d_30, cl = 2)
c_31<- bws.count(d_31, cl = 2)
c_32<- bws.count(d_32, cl = 2)
c_33<- bws.count(d_33, cl = 2)
c_34<- bws.count(d_34, cl = 2)
c_35<- bws.count(d_35, cl = 2)
c_36<- bws.count(d_36, cl = 2)
c_37<- bws.count(d_37, cl = 2)
c_38<- bws.count(d_38, cl = 2)
c_39<- bws.count(d_39, cl = 2)
c_40<- bws.count(d_40, cl = 2)

C.full<-rbind(c_1,c_2,c_3,c_4,c_5,c_6,c_7,c_8,c_9,c_10,
      c_11,c_12,c_13,c_14,c_15,c_16,c_17,c_18,c_19,c_20,
      c_21,c_22,c_23,c_24,c_25,c_26,c_27,c_28,c_29,c_30,
      c_31,c_32,c_33,c_34,c_35,c_36,c_37,c_38,c_39,c_40)
str(C.full)

dim(C.full)#This looks good

sum(C.full)
#summary(C.full)
#There is a problem of not reading the participant number correctly.
# Therefore, all data after rank is not correctly calculated.
#But this can be done manually using the following key for each item j:

#Bj<-sum(Bj)
#Wj<-sum(Wj)
#BWj<-Bj-Wj
#Rank at the end
#meanBj<-mean(Bj)
#meanWj<-mean(Wj)
#meanBWj<-meanB-meanW
#mean.stdBWj<-mean(sbw_Itemj)
#sqrt(BWj)<-sqrt(Bj/Wj)

####Modelling approach####

####MNL###

#Case 1 BWS analysis on the basis of maxdiff model. 

#mlogit() function is applied, the function similar to lm() regression function. 

#Specifiying the model - baseline is item 9
mf <- RES ~ ITEM1 + ITEM2 + ITEM3 + ITEM4 + ITEM5 + ITEM6 + ITEM7 + 
            ITEM8 + ITEM10 + ITEM11 + ITEM12 + ITEM13 + ITEM14 - 1

# Data set for the maxdiff model
md.data <- dfidx(data = DATA.full, 
                 idx = list(c("STR", "id"), "PAIR"), 
                 choice = "RES")

#Model and output
md.out <- mlogit(formula = mf, data = md.data)
summary(md.out)

#To see the extent of relative importance among the seven characteristics, 
#the share of preferences for them are calculated using bws.sp():
sp.md <- bws.sp(md.out, base = "ITEM9")
sp.md

####RPL###

rpl1 <- mlogit(
  formula = RES ~ ITEM1 + ITEM2 + ITEM3 + ITEM4 + ITEM5 + ITEM6 + ITEM7 +
                  ITEM8 + ITEM10 + ITEM11 + ITEM12 + ITEM13 + ITEM14 - 1 | 0,
  data = md.data, 
  rpar = c(ITEM1 = "n", ITEM2 = "n", ITEM3 = "n", ITEM4 = "n", ITEM5 = "n",
           ITEM6 = "n", ITEM7 = "n", ITEM8 = "n", ITEM10 = "n", ITEM11 = "n",
           ITEM12 = "n", ITEM13 = "n", ITEM14 = "n"),
  R = 100, halton = NA, panel = TRUE)
summary(rpl1)

#The shares of preferences for the 13 items calculated using the estimated mean 
#coefficients are as follows:

sp.rpl<-bws.sp(object = rpl1, base = "ITEM9",
       coef = c("ITEM1", "ITEM2", "ITEM3", "ITEM4","ITEM5", "ITEM6", "ITEM7",
                "ITEM8","ITEM10","ITEM11","ITEM12", "ITEM13", "ITEM14"))
sp.rpl

#The function bws.sp() can also calculate the shares of preferences for items 
#by individual on the basis of the individual-specific parameters as follows:

indsp.rpl <- bws.sp(
  object = rpl1$indpar[, -1], # Variable id is removed
  base = "ITEM9")
head(indsp.rpl)

boxplot(indsp.rpl)

####MXL####

#1)Creating the dataset

data_mixl<- mlogit.data(data = DATA.full, choice = "RES", shape = "long",
                        alt.var = "PAIR", chid.var = "STR", id.var = "id")


rpl2 <- gmnl(RES ~ ITEM1 + ITEM2 + ITEM3 + ITEM4 + ITEM5 + ITEM6 + ITEM7 + ITEM8 +
                    ITEM10 + ITEM11 + ITEM12 + ITEM13 + ITEM14 - 1 | 0,
             data = data_mixl, 
             ranp = c(ITEM1 = "n", ITEM2 = "n", ITEM3 = "n", ITEM4 = "n", ITEM5 = "n",
                      ITEM6 = "n", ITEM7 = "n", ITEM8 = "n",ITEM10 = "n", ITEM11 = "n",
                      ITEM12 = "n", ITEM13 = "n", ITEM14 = "n"),
             model = "mixl",
             R = 100, halton = NA, panel = TRUE)

summary(rpl2)

sp.rpl2<-bws.sp(object = rpl2, base = "ITEM9",
               coef = c("ITEM1", "ITEM2", "ITEM3", "ITEM4", "ITEM5", "ITEM6", "ITEM7", 
                        "ITEM8", "ITEM10", "ITEM11", "ITEM12", "ITEM13", "ITEM14"))
sp.rpl2

indpar.gmnl <- effect.gmnl(rpl2, effect = "ce")$mean
head(indpar.gmnl)

indsp.gmnl <- bws.sp(indpar.gmnl, base = "ITEM9")
boxplot(indsp.gmnl)

                    