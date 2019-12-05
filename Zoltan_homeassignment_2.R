#Loading packages
library(lsr)
library(psych)
library(tidyverse)
library(olsrr)
library(ggfortify)
library(car)

#Import dataset and assign to variable
data_1 = read.csv("https://raw.githubusercontent.com/kekecsz/PSYP13_Data_analysis_class-2019/master/home_sample_1.csv ")

##Clean data
#Make copy
data_2 <- data_1
#Check for coding errors
View(data_2)
#Change STAI data from '3.5' to '35' for ID 18
data_2 <- mutate(data_2,
                STAI_trait = as.numeric(as.character(replace(STAI_trait, STAI_trait == "3.5", 35))))
#Remove negative household income row
data_2 <- data_2[-c(49),]

##Build new model from commentary
full_mod <- lm(pain ~ age + sex + STAI_trait + mindfulness + pain_cat + cortisol_serum + weight + household_income + IQ, data = data_2)

##Check for outliers
#Use Cook's D to identify outliers
ols_plot_cooksd_bar(full_mod)

#Create leverage to check
lev_1 = hat(model.matrix(full_mod))

#Check with Mahalanobis Distance for 3 cases
N = nrow(data_2)
mahad=(N-1)*(lev_1-1 / N)
tail(sort(mahad),3)
order(mahad,decreasing=T)[c(3,2,1)]

##Full Model = Initial Model
#Check full model for normality of the residuals
ols_test_normality(full_mod)

#Check full model for linearity
residualPlots(full_mod)

##Check full model for homogeneity
#Plot
win.graph()
plot(x = full_mod, which = 3)

#Do statistical test for homogeneity
ncvTest(full_mod)

#Check full model for multicollinearity 
vif(full_mod)

#Summarize full model
summary(full_mod)

##Perform backwards regression
step(object = full_mod,
     direction = "backward")

#Create new mod with predictors retained from backwards regression
backward_mod <- lm(pain ~ age + sex + mindfulness + pain_cat + cortisol_serum + weight, data = data_2)

#Check backwards model for normality of the residuals
ols_test_normality(backward_mod)

#Check backwards model for linearity
residualPlots(backward_mod)

##Check backwards model for homogeneity
#Plot
plot(x = backward_mod, which = 3)

#Do statistical test for homogeneity
ncvTest(backward_mod)

#Check backwards model for multicollinearity 
vif(backward_mod)

#Print backwards model
backward_mod

#Summarize backwards model
summary(backward_mod)

#Load custom functions
coef_table = function(backward_mod){
  require(lm.beta)
  mod_sum = summary(backward_mod)
  mod_sum_p_values = as.character(round(mod_sum$coefficients[,4], 3))
  mod_sum_p_values[mod_sum_p_values != "0" & mod_sum_p_values != "1"] = substr(mod_sum_p_values[mod_sum_p_values != "0" & mod_sum_p_values != "1"], 2, nchar(mod_sum_p_values[mod_sum_p_values != "0" & mod_sum_p_values != "1"]))
  mod_sum_p_values[mod_sum_p_values == "0"] = "<.001"
  
  
  mod_sum_table = cbind(as.data.frame(round(cbind(coef(backward_mod), confint(backward_mod), c(0, lm.beta(backward_mod)$standardized.coefficients[c(2:length(backward_mod$coefficients))])), 2)), mod_sum_p_values)
  names(mod_sum_table) = c("b", "95%CI lb", "95%CI ub", "Std.Beta", "p-value")
  mod_sum_table["(Intercept)","Std.Beta"] = "0"
  return(mod_sum_table)
}

#Report backwards model coefficients table
coef_table(backward_mod)

##Create theory-based model from assignment #1
theory_mod <- lm(pain ~ sex + age + STAI_trait + mindfulness + pain_cat + cortisol_serum, data = data_2)

##Compare Models
#Calculate AIC of full_mod and backward_mod
AIC(full_mod)
AIC(backward_mod)

#Calculate ANOVAS for full and backwards models
anova(full_mod, backward_mod)

#Calculate AIC of theory_mod and backward_mod
AIC(theory_mod)
AIC(backward_mod)

##Import second dataset
data_3 <- read.csv("https://raw.githubusercontent.com/kekecsz/PSYP13_Data_analysis_class-2019/master/home_sample_2.csv")

#Check for coding errors
View(data_3)
summary(data_3)

#Assign prediction variables
pred_theory <- predict(theory_mod, data_3)
pred_new <- predict(backward_mod, data_3)

#Calculate Sum of Squares
sum((data_3$pain - pred_theory)^2)
sum((data_3$pain - pred_new)^2)

