
rm(list=ls())  #removes all variables stored previously
library(Hmisc)  #import

data <- read_csv("COVID19_line_list_data.csv")
describe(data)  #Hmisc command)

#cleaned up data
#if the death column does not = 0, then the person died.
data$death_dummy <- as.integer(data$death != 0)

#calculate death rate
sum(data$death_dummy) / nrow(data)

#AGE
#claim: people who die are older
dead = subset(data, death_dummy == 1) #made variable named dead with all the ropws of the ones that died
alive = subset(data, death_dummy == 0) #all the rows of those who survived
mean(dead$age, na.rm = TRUE)
mean(alive$age, na.rm = TRUE)
#Is this statistically significant?
t.test(alive$age, dead$age, alternative = "two.sided", conf.level = 0.99)
#normally, if p-value < 0.05, we reject null hypothesis
#here, p-value ~ 0, so we reject the null hypothesis
# 2.2e-16 is 0 which means there is a 0% chance that the ages are the same for those dead and who survived covid


#GENDER
#claim: Gender has no effect
men = subset(data, gender == "male") 
women = subset(data, gender == "female") 
mean(men$death_dummy, na.rm = TRUE) #average death rate of men is 8.5%
mean(women$death_dummy, na.rm = TRUE) #average death rate of women us 3.7%
#Is this statistically significant?
t.test(men$death_dummy, women$death_dummy, alternative = "two.sided", conf.level = 0.99)
# 99% confidence: men have from 0.8% to 8.8% higher chance of dying
# p-value = 0.002 < 0.05, so this is statistically significant
#significant

