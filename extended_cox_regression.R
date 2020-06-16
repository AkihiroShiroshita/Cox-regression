#recid dataset
recid_wide <- read.csv("C:/Users/akihi/Downloads/recid.csv")
library(survival)
survobj_wide = Surv(recid_wide$week, recid_wide$arrest)
summary(coxph(survobj_wide ~ fin + age + race + wexp + mar + paro + prio,
              data = recid_wide))
recid_wide$id = 1:nrow(recid_wide)
library(tidyr); library(dplyr)
## data: your (wide) dataset (has to be a "data.frame" or a "tibble")
## key: a new variable you'll create to store the variable names you gathered
## value: a new variable you'll create to store the values of the variables
## the last argument(s): the variables that you want to gather
gather(data=recid_wide, key="emp_wk", value="emp", emp1:emp52) 
## sub() is a base function that can substitute a character string to another
recid_long <- recid_wide %>%
  gather(emp_wk, emp, emp1:emp52) %>% #transfer wide to long
  arrange(id) %>% #sort by id
  mutate(tf = as.numeric(sub("emp", "", emp_wk))) %>% #final time
  mutate(ti = tf-1) %>% #initial time (for each time interval)
  filter(tf<=week) %>% #keep only the intervals before arrest/censoring
  mutate(event = ifelse(tf<week, 0, arrest)) #censoring indicator for each time interval
survobj_long = Surv(time=recid_long$ti, time2=recid_long$tf,
                    event=recid_long$event, type="counting")
#Note that this model allows time-varying exposure for emp 
#but assumes that the effect size of emp is the same in different interval.
summary(coxph(survobj_long ~ fin + age + race + wexp + mar + paro + prio + emp,
              data = recid_long))
#lag time
#lag() takes a vector and returns a vector with every value replaced by its “previous” value
#lead() takes a vector and returns a vector with every value replaced by its “next” value
#after you do lag/lead, you’ll get a NA in the very beginning/end
#(of each individual, in this case), which is fine.
recid_long %>%
  group_by(id) %>% #group by id i.e. for each participant
  mutate(empprev=lag(emp, n=1))
summary(coxph(survobj_long ~ fin + age + race + wexp + mar + paro + prio + emp,
              data = recid_long))
#time dependent effect
summary(coxph(survobj_long ~ fin*tf + age + race + wexp + mar + paro + prio + emp, data = recid_long))
#The interaction term is not significant in this case, 
#which means the HR of financial aid on the outcome (getting arrested) does not change over time 
#=> proportional hazard assumption holds for financial aid. 
#If that term is significant, then we would say that 
#the proportional hazard assumption does not hold for this variable.
#You will also find out that the estimates of the time variable 
#(tf, in this case) is NA, which is expected,
#because Cox PH model innately controls for the time metric you used, 
#which is basically the case for survival analysis as I have alluded earlier in lab2.

#stratified cox ph model
summary(coxph(survobj_long ~ fin + age + strata(race) + wexp + mar + paro + prio + emp,
              data = recid_long))

#time-fixed effect
#estimate the survival function assuming that the covariate is a fixed value over time.
PH1 = coxph(survobj_wide ~ fin + prio, data = recid_wide)
covs1 = data.frame(fin = 1, prio = 0)
summary(survfit(PH1, newdata=covs1))
plot(survfit(PH1, newdata=covs1))
#time-dependent effect
#NOT fin:tf
recid_long$fin_t = recid_long$fin * recid_long$tf
PH2 = coxph(Surv(ti, tf, event) ~ fin + fin_t + prio, data = recid_long)
last = recid_long$id[which.max(recid_long$tf)]
intervals = recid_long[recid_long$id==last, c("ti", "tf", "event")]
covs2 = data.frame(fin = 1, prio = 0, intervals)
covs2$fin_t = covs2$fin * covs2$tf
summary(survfit(PH2, newdata=covs2, individual=T))
plot(survfit(PH2, newdata=covs2, individual=T))
