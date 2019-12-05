#Package
library(lsr) ; library(psych); library(tidyverse); library(olsrr); library(ggfortify)

#Import dataset and assign to variable
data1 = read.csv("https://raw.githubusercontent.com/kekecsz/PSYP13_Data_analysis_class-2019/master/home_sample_1.csv ")

##Clean data
#Make copy
data2 <- data1
#Check for coding errors
View(data2)
#Change STAI data from '3.5' to '35' for ID 18
data2 <- mutate(data2,
       STAI_trait = as.numeric(as.character(replace(STAI_trait, STAI_trait == "3.5", 35))))
#Remove row with household income as a negative
data2_clean <- data2[-c(49),]
##Make first model with age and sex as predictors of pain
mod1_clean <- lm(pain ~ sex + age, data = data2_clean)

##Check for outliers
#Use Cook's D to identify outliers
ols_plot_cooksd_bar(mod1_clean)

#Create leverage to check
lev1 = hat(model.matrix(mod1_clean))

#Check with Mahalanobis Distance for 3 cases
N = nrow(data2_clean)
mahad=(N-1)*(lev1-1 / N)
tail(sort(mahad),3)
order(mahad,decreasing=T)[c(3,2,1)]

#Check first model for normality of the residuals
ols_test_normality(mod1_clean)

#Check first model for linearity
residualPlots(mod1_clean)

##Check first model for homogeneity
#Plot
plot(x = mod1_clean, which = 3)

#Do statistical test for homogeneity
ncvTest(mod1_clean)

#Check first model for multicollinearity 
vif(mod1_clean)

#Load custom functions
coef_table = function(mod1_clean){
  require(lm.beta)
  mod_sum = summary(mod1_clean)
  mod_sum_p_values = as.character(round(mod_sum$coefficients[,4], 3))
  mod_sum_p_values[mod_sum_p_values != "0" & mod_sum_p_values != "1"] = substr(mod_sum_p_values[mod_sum_p_values != "0" & mod_sum_p_values != "1"], 2, nchar(mod_sum_p_values[mod_sum_p_values != "0" & mod_sum_p_values != "1"]))
  mod_sum_p_values[mod_sum_p_values == "0"] = "<.001"
  
  
  mod_sum_table = cbind(as.data.frame(round(cbind(coef(mod1_clean), confint(mod1_clean), c(0, lm.beta(mod1_clean)$standardized.coefficients[c(2:length(mod1_clean$coefficients))])), 2)), mod_sum_p_values)
  names(mod_sum_table) = c("b", "95%CI lb", "95%CI ub", "Std.Beta", "p-value")
  mod_sum_table["(Intercept)","Std.Beta"] = "0"
  return(mod_sum_table)
}

#Reporting Coefficients
coef_table(mod1_clean)

#Display mod1
mod1_clean

#Display results of first model
summary(mod1_clean)

##Make second model with age, sex, STAI, pain_cat, cortisol measures
mod2_clean <- lm(pain ~ sex + age + STAI_trait + mindfulness + pain_cat + cortisol_serum + cortisol_saliva, data = data2_clean)

##Check for outliers
#Use Cook's D to identify outliers
ols_plot_cooksd_bar(mod2_clean)

#Create leverage to check
lev2 = hat(model.matrix(mod2_clean))
summary(mod2_clean)

#Check with Mahalanobis Distance for 5 cases
N = nrow(data2_clean)
mahad=(N-1)*(lev2-1 / N)
tail(sort(mahad),3)
order(mahad,decreasing=T)[c(3,2,1)]

#Due to a potential outlier, check chi-square value
qchisq(.001, 7, lower.tail = F)
#Make copy of data
data3_clean <- data2_clean

#Make new model with cleaned dataset
mod2_clean <- lm(pain ~ sex + age + STAI_trait + mindfulness + pain_cat + cortisol_serum + cortisol_saliva, data = data3_clean)

#Check second model for normality of the residuals
ols_test_normality(mod2_clean)

#Check second model for linearity
residualPlots(mod2_clean)

##Check second model for homogeneity
#Plot
plot(x = mod2_clean, which = 3)

#Do statistical test for homogeneity
ncvTest(mod2_clean)

#Check second model for multicollinearity 
vif(mod2_clean)

##Due to saliva being high on the multicollinerarity test, recreate model without saliva
#Make new model without saliva
mod3_clean <- lm(pain ~ sex + age + STAI_trait + mindfulness + pain_cat + cortisol_serum, data = data2_clean)

#Use Cook's D to identify outliers
ols_plot_cooksd_bar(mod3_clean)

#Make copy of data
data4_clean <- data2_clean

#Create leverage to check
lev3 = hat(model.matrix(mod3_clean))

#Check with Mahalanobis Distance for 5 cases
N = nrow(data4_clean)
mahad=(N-1)*(lev3-1 / N)
tail(sort(mahad),3)
order(mahad,decreasing=T)[c(3,2,1)]

#Check third model for normality of the residuals
ols_test_normality(mod3_clean)

#Check third model for linearity
residualPlots(mod3_clean)

##Check third model for homogeneity
#Plot
plot(x = mod3_clean, which = 3)

#Do statistical test for homogeneity
ncvTest(mod3_clean)

#Check third model for multicollinearity 
vif(mod3_clean)

#Display mod3_clean
mod3_clean

#Display results of third model and first
summary(mod3_clean)
summary(mod1_clean)

#Reporting Coefficients
coef_table(mod3_clean)

#Calculate AIC of first model and third model (without saliva)
AIC(mod1_clean)
AIC(mod3_clean)

#Calculate ANOVAS for both models
anova(mod1_clean, mod3_clean)

