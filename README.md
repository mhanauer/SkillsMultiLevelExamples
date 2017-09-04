---
title: "Test"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
Create the data here.  Self Control is a total score with seven questions on a 7 point likert scale
```{r}
# Create CBMs
cbmSelect = c(10:80)
set.seed(12345)
cbm= as.data.frame(sample(cbmSelect, 10000, replace = TRUE)); head(cbmData)
colnames(cbm) = c("cbm"); head(cbm)

id = as.data.frame(rep(1:10, 100, each = 10)); head(id)
colnames(id) = c("id"); head(id)

# Need school, self control, and time

time = as.data.frame(rep(1:10, 100)); head(time)
colnames(time) = c("time")

scSelect = c(10:70)
set.seed(12345)
sc = as.data.frame(sample(scSelect, 10000, replace = TRUE)); head(sc)
colnames(sc) = c("sc"); head(sc)

school = as.data.frame(rep(1:500, each = 20)); head(school)
colnames(school) = c("school"); head(school)

dat = as.data.frame(cbind(id, time, school, sc, cbm)); dat
# For schools have 20 schools.  For this model make sure all students are in the same schools.  500 students will be in each school.  
```
Ok let us build the first model with just a random intercept
yij = b00 + b01 + u0j + eij

b00 is the average or mean across the schools.  The first zero stands for which level it is in.  0 indicates level two while 1 indicates level 1.  Therefore, b00 is a level two variable.  The next 0 indicates that it is is the intercept for level two.  

b01 is the average regression coefficient for the change assoicated with the sc variable.  The second 1 indicates that it is the regression in the second level.  However, this regression coeficient is interpretation in the same way as a level regression coefficeint.

u0j this is the random deviation from the mean or initial value for each school.  This is a level two error term, which is why it only has the subscript j instead of i and j.  It has a j, because j includes all of the 200 schools.  For example the unique deviation from the intercept for school one would be u01.  

eij is the indiviudal level one error for each indiviudal in each school.  

$${y_{ij} = \gamma_{00} + \gamma_{01} +u_{0j}*(sc_{ij}) + e_{ij}}$$
Center the predictors, because they can reduce collinatarly between the slopes, intercepts, and interactions terms.
```{r}
dat$scCenter = round(scale(dat$sc, center =TRUE),2)
dat$cbmCenter = round(scale(dat$cbm, center= TRUE),2)
dat= as.data.frame(dat)
head(dat)
```
To get the amount of variance in the dependent variable associated with the 
```{r}
library(nlme)
model1 = lme(fixed = cbmCenter ~ scCenter, random = ~1 | school, data = dat)
summary(model1)
1.0744 / (1.0744+0.0220)
```
Standrdization residuals = are the residuals scaled by the their standard deviation.
Random effects = These are the average values for the standard deviation for the intercept (i.e. the average amount of deviation we see from the intercept) this is (u1j) and the level one residual (eij).  The fixed effect are interpreted that same single level regression.

Corraltion (inter): The correlation between the fixed intercept and slope.  If the value was above zero, then we could say that as the intercept (i.e. intial values) increases so does the slope.  So a person who starts higher will increases as their score increases.


Using REML, because it accounts for the included parameters correctly the standard errors (i.e. making them larger to account for the less degrees of freedom). 

AIC, BIC, and logLik are all model comparison statistics.

The interclass correlation (ICC) = This is the amount of variation in the dependent variable that is accounted for by the variation among the groups.  It is the intercept (i.e. the variance among the clusters) and the residual (the variance among the indiviudals)

$${\rho_{1} = {\tau^2 / (\tau^2 +\sigma^2)} }$$
When we plug in the values, we see that 98% of the variation in the dependent variable is associated with differences, between groups.  
$${.98 = {1.0744 / (1.0744 +0.0220)} }$$





