#Load packages
library(lsr)
library(psych)
library(tidyverse)
library(olsrr)
library(ggfortify)
library(lme4)
library(car)
library(jtools)
library(insight)
library(lmerTest)
library(sjstats)

###Recreate assignment 1
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

#####################################
##Assignment 3
#Import Dataset 3
data_3 <- read.csv("https://raw.githubusercontent.com/kekecsz/PSYP13_Data_analysis_class-2019/master/home_sample_3.csv")

#Import Dataset 4
data_4 <- read.csv("https://raw.githubusercontent.com/kekecsz/PSYP13_Data_analysis_class-2019/master/home_sample_4.csv")

##Clean dataset 3
#Give dataset 3 summary
summary(data_3)

#Check dataset structure
str(data_3)

#Replace 'Female' with 'female' and drop factor level
data_3_clean <- data_3 %>% mutate(sex = droplevels(replace(sex, sex == "Female", "female")))

#Remove ID with too high mindfullness
data_3_clean <- data_3_clean[-c(195),]

#Recheck dataset 3
summary(data_3_clean)
str(data_3_clean)                                 

##Clean dataset 4
#Give dataset 4 summary
summary(data_4)

#Check dataset 4 structure
str(data_4)

#Remove ID's with household income as a negative (#119 and #166)
data_4_clean <- data_4[-c(119,166),]

#Recheck dataset 4
summary(data_4_clean)

#Create random intercept model
mod_rintercept = lmer(pain ~ sex + age + STAI_trait + mindfulness + pain_cat + cortisol_serum + (1 | hospital), data = data_3_clean)

##Checking model for outliers
#Use Influence to identify outliers
inf_obs = influence(mod_rintercept, obs = T)$alt.fixed
influence_group = influence(mod_rintercept, group = "hospital")$alt.fixed

#Plot influence
data_plot_influence = as_tibble(influence_group) %>% 	
  gather(colnames(influence_group), value = coefficient, key = predictor)	

data_plot_influence %>% 	
  ggplot() +	
  aes(x = 1, y = coefficient, group = predictor) +	
  geom_violin() +	
  facet_wrap( ~ predictor, scales = "free")	

##Checking model assumptions
#Checking Normality
qqmath(mod_rintercept, hospital=0.05)	

#Checking Linearity
plot(mod_rintercept, arg = "pearson")

#Create residuals variable
resid <- residuals(mod_rintercept)

#Plot each predictor
data_3_clean %>% 	
  ggplot() +	
  aes(x = pain, y = resid) +	
  geom_point()

data_3_clean %>% 	
  ggplot() +	
  aes(x = age, y = resid) +	
  geom_point()	

data_3_clean %>% 	
  ggplot() +	
  aes(x = sex, y = resid) +	
  geom_point()	

data_3_clean %>% 	
  ggplot() +	
  aes(x = STAI_trait, y = resid) +	
  geom_point()	

data_3_clean %>% 	
  ggplot() +	
  aes(x = mindfulness, y = resid) +	
  geom_point()

data_3_clean %>% 	
  ggplot() +	
  aes(x = pain_cat, y = resid) +	
  geom_point()	

data_3_clean %>% 	
  ggplot() +	
  aes(x = cortisol_serum, y = resid) +	
  geom_point()	

#Homoscedasticity plot
plot(mod_rintercept, arg = "pearson")	

#Homoscedasticity stats
homoscedasticity_mod = lm(resid^2 ~ hospital, data = data_3_clean)	
summary(homoscedasticity_mod)	

#Multicollinearity
pairs.panels(data_3_clean[,c("pain", "age", "sex", "STAI_trait", "mindfulness", "pain_cat", "cortisol_serum")], col = "red", lm = T)	

#Reporting Coefficients for Assignment 1 model
coef_table(mod3_clean)

##Reporting Coefficients for Assignment 3 model
#Compute B value, CI's and p values
summ(mod_rintercept, confint = T, digits = 5)

#Compute std. beta
std_beta(mod_rintercept, ci.lvl =.95)

#Calculating Variance for fixed effects 
get_variance_fixed(mod_rintercept)

#Calculating Variance for the random intercept
get_variance_intercept(mod_rintercept)

#Calculating Variance for the residuals
get_variance_residual(mod_rintercept)

#Calculating marginal and conditional R^2
library(MuMIn)
r.squaredGLMM(mod_rintercept)

#Print model for regression equation
mod_rintercept 
#regression equation spelled out:
#y=3.41 + (.297 * sexmale) - (.056 * age) - (.012 * STAI_trait) - (.181 * mindfulness) + (.085 * pain_cat) + (.467 * cortisol_serum)

#Predict on new dataset
pred_model <- predict(mod_rintercept, newdata = data_4_clean, allow.new.levels = TRUE)
pred_model

#Compute RSS
rss = sum((data_4_clean$pain - predict(mod_rintercept, data_4_clean, allow.new.levels = TRUE))^2)
#Compute TSS
tss = sum((data_4_clean$pain - mean(data_4_clean$pain))^2)

#Print RSS and TSS
rss
tss

#Compute r2
r2 <- 1-(rss/tss)

#Print r2 for data 4 and marginal and conditional for data 3
r2
r.squaredGLMM(mod_rintercept)

#Calculate Caic for both models
library(cAIC4)
cAIC(mod_rintercept)
cAIC(mod_rintercept2)


#Check most influential predictor 
mod_rintercept

##Create new linear mixed model with cortisol_serum as only predictor
mod_rintercept2 <- lmer(pain ~ cortisol_serum + (cortisol_serum | hospital), data = data_3_clean)

#Print new model
mod_rintercept2

##Plotting
predicton_slope <- predict(mod_rintercept2)

#How cortisol predicts pain in each hospital
data_3_clean %>%
  ggplot() + aes(y = pain, x = cortisol_serum, group = hospital) + 
  geom_point(aes(color = hospital), size = 4) + 
  geom_line(color = "black", aes(y = predicton_slope, x = cortisol_serum)) + 
  facet_wrap(~hospital, ncol = 2)







