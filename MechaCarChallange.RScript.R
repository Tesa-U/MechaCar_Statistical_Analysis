#Deliverable 1
#read the file
library(dplyr)
mechaCar <- read.csv(file='MechaCar_mpg.csv',check.names=F,stringsAsFactors = F)

#perform linear regression

lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD,data= mechaCar)

#summary
summary(lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD,data= mechaCar))

#Deliverable 2
#read the suspension_coil.csv file
suspension <- read.csv(file='Suspension_Coil.csv',check.names=F,stringsAsFactors = F)

#total_summary dataframe
total_summary <- suspension %>% summarize(Mean=mean(PSI),Median=(PSI),Variance=var(PSI),SD=sd(PSI)) 

# lot_summary dataframe
lot_summary <- suspension %>% group_by(Manufacturing_Lot) %>% summarize(Mean=mean(PSI),Median=(PSI),Variance=var(PSI),SD=sd(PSI))

#Deliverable 3
# t.test function
t.test(suspension$PSI,mu = 1500)
# t.test No.1
t.test(subset(suspension,Manufacturing_Lot=="Lot1")$PSI,mu = 1500)
# t.test No.2
t.test(subset(suspension,Manufacturing_Lot=="Lot2")$PSI,mu = 1500)
# t.test No.3
t.test(subset(suspension,Manufacturing_Lot=="Lot3")$PSI,mu = 1500)



