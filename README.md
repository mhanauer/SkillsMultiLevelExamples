---
title: "Test"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
In this example, I am documenting how to formulate an equation for random intercepts and slopes in multilevel modeling along with the r code using the nlme package with interpretation of the output.  In this current data set there are four variables id, school, sex (i.e. gender), and like (i.e. a measure on a scale 1 to 8 how much the student likes school).  Here I am just tidying up the data by making the f and m 1 and 0 respectively as well as getting rid of nonsensical values such as 19.1 for the sex variable.  Overall there are 1,383 participants with 4 variables.  In this demonstration we will want to predict how much a student likes schools with the covariate sex while accounting for the nesting of students within school and make the model more complicated further into the example.
```{r}
setwd("~/Desktop")
dat = read.csv("SkillsDataSet.csv", header = TRUE)

head(dat)
dat = dat[c("id", "school", "sex", "like")]
dat = as.data.frame(apply(dat, 2, function(x){ifelse(x == "f", 1, ifelse(x == "m", 0, ifelse(x == 19.1, NA, x)))}))
dat = as.data.frame(na.omit(dat)); head(dat)
dat = write.csv(dat, "dat.csv",row.names = TRUE)
dat = read.csv("dat.csv", header = TRUE); head(dat)
dat$sc = round(rnorm(1383, 0,5),0)
dim(dat)
head(dat)
```
Here I try to break down what each component of the equation means.

b00 is the average or mean across the schools.  The first zero stands whether a parameter is slope or intercept (1 = slope and 0 = intercept).  0 indicates level two while 1 indicates level 1.  Therefore, b00 is a level two variable.  The next 0 indicates that it  is the intercept for level two.  

b01 is the average regression coefficient for the change associated with sex variable (sexij).  The second 1 indicates that it is the regression coefficient in the second level.  It is the average change we would see given a one unit change in the independent variable (in this case identifying as a female).  

u0j this is the random deviation from the intercept or initial value for each school.  This is a level two error term, which is why it only has the subscript j instead of i and j.  It has a j, because j represents all of the 200 schools (i.e. from 1 to j schools).  For example the unique deviation from the intercept for school one would be u01.  

eij is the individual level one error term for each individual in each school. 

Level 1: yij = B0j + B1jX

$$ Level~1:~~~{y_{ij} = \beta_{00} + \beta_{01}(sex_{ij}) + e_{ij}}~~~ (1.1)$$
$$ Level~2~Intercept:~~~{\beta_{00} = \gamma_{00} + u_{0j}} ~~~ (equation~1)~~~(1.2)$$
$$Level~2~Slope: ~~~{\beta_{1j} = \gamma_{00} + \gamma_{01}(sex_{ij}) +u_{0j}(sex_{ij}) + e_{ij}} ~~~ (equation~1)$$

$$Mixed~model: ~~~{y_{ij} = \gamma_{00} + \gamma_{01}(sex_{ij}) +u_{0j}(sex_{ij}) + e_{ij}} ~~~ (equation~1)$$
Here is the r-code for the multilevel model described in equation 1 using the nlme package.  There is the fixed part, which will have the average intercept and the parameter estimate for the scCenter variable.  To model the random intercepts, we use the 1 to signify that this is a random intercepts model where we want intercepts for each school.   
```{r}
library(nlme)
model1 = lme(fixed = like ~ sex, random = ~1 | school, data = dat)
sumModel1 = summary(model1); sumModel1

0.44^2/(0.44^2 +1.79^2)
```
We are using REML, because it accounts for the included parameters correctly in the standard errors (i.e. making them larger by accounting for the reduced degrees of freedom). 

AIC, BIC, and logLik are all model comparison statistics.

Random effects = The random effects demonstrate the amount of variance (they are reported in standard deviations) in the dependent variable associated with the schools (intercept) and the individuals (residual).  These variances can be used to measure the amount of variance associated between cluster and within cluster using the following equation 2:
$${\rho_{1} = {\tau^2 / (\tau^2 +\sigma^2)} }~~~ equation(2)$$
When we plug in the random effect standard deviation values we need to square them to get the variances.  We see that almost 20% of the variation in the dependent variable is associated with differences between clusters.  Therefore, there is some indication that students in schools are more alike on the like dependent variable.
$${ .197= {0.44/(0.44 +1.79)} }$$
Fixed effects: These are interpreted just as a regular single level regression.

Correlation (inter): The correlation between the fixed intercept and fixed slope for sex.  If the value is above zero, then we could say that as the intercept (i.e. initial values) increase so does the slope.  So for a positive relationship, a person who starts with a higher intercept will increase as their score increases.  In our case the opposite is true.  Therefore, as the intercept or starting values for like move up, we see a decrease in the slope for female.

Standardized residuals = These are the residuals scaled by the standard deviation.

Now we can estimate the random slope model.   The random slope model adds one unique component which is u1j.  This is the random level two error for the slope allowing the slope to differ for each school for the covariate female.  It is indexed with j, because it can vary for each school.  It is multiplied by the covariate value of sc, because it is a slope therefore we need to multiply it by the covariate for which it is measuring the slope.
$${y_{ij} = \gamma_{00} + \gamma_{01}(sex_{ij}) +u_{0j} + u_{1j}(sex_{ij}) + e_{ij}}$$
For this model, in nlme, instead of using one, we now use the term that we want to have a random slope which is sex.
```{r}
model2 = lme(fixed = like ~ sex, random = ~ sex | school, data = dat)
sumModel2 = summary(model2); sumModel2

b00 = g01 + g01 
```
The only new parameter provided is the stdDev for the sex variable, which is the standard deviation in the slope coefficients for sex for each school. 

Finally, adding adding a level two variable (i.e. a variable that changes at the school level and is constant at the individual level) can be done in the following equation:




Finch, W. H., Bolin, J. E., & Kelley, K. (2014). Multilevel modeling using R. Crc Press.

