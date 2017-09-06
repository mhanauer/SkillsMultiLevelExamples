---
title: "Test"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
In this example, I am documenting how to formulate an equation for random intercepts and slopes in multilevel modeling along with the r code using the nlme package with interpretation of the output.  In this current data set there are four variables id, school, sex (i.e. gender), and like (i.e. a measure on a scale 1 to 8 how much the student likes school).  Here I am just tidying up the data by making the f and m 1 and 0 respectively as well as getting rid of nonsensical values such as 19.1 for the sex variable.  Overall there are 1,383 participants with 5 variables.  In this demonstration we will want to predict how much a student likes schools with the covariate sex while accounting for the nesting of students within school and make the model more complicated further into the example.  In later model we will use the level two covariate school funding.
```{r}
setwd("~/Desktop")
dat = read.csv("SkillsDataSet.csv", header = TRUE)

head(dat)
dat = dat[c("id", "school", "sex", "like")]
dat = as.data.frame(apply(dat, 2, function(x){ifelse(x == "f", 1, ifelse(x == "m", 0, ifelse(x == 19.1, NA, x)))}))
dat = as.data.frame(na.omit(dat)); head(dat)
dat = write.csv(dat, "dat.csv",row.names = FALSE)
dat = read.csv("dat.csv", header = TRUE); head(dat)
dat = as.data.frame(dat[-c(1:3),]); dim(dat)
dat$id = 1:length(dat$like)
dat$school = rep(1:30, each =  46); dim(dat)
# Creating the school level school funding 
dat$sf = round(rep(rnorm(46,0, 20), each  = 30),0)
```
Here I try to break down what each component of the equation means.

gamma00 is the average or mean like score across the schools.  The indexes indicate where the parameter falls in each level.  For example, the first 0 in the gamma00 means that it is the intercept for the level one.  The second zero means that it is the mean or intercept for level two.  However, level two intercepts can vary by u0j, which will be discussed next. 

u0j this is the random deviation from the intercept or initial value for each school.  This is a level two error term, which is why it only has the subscript j instead of i and j.  It has a j, because j represents all of the 200 schools (i.e. from 1 to j schools).  For example the unique deviation from the intercept for school one would be u01.  

gamma10 is the average regression coefficient for the change associated with sex variable (sexij).  It is the average change we would see given a one unit change in the independent variable (in this case identifying as a female).  The first 1 index means that it is the the first slope coefficient in in level one and the second 0 means that it is the average slope coefficient (i.e. not effect of any level two variables), because in this model the slope is not varying by schools.   

eij is the individual level one error term for each individual in each school. 
$$ Level~1:~~~{y_{ij} = \beta_{00} + \beta_{1}(sex_{ij}) + e_{ij}}~~~ (1.1)$$
$$ Level~2~Intercept:~~~{\beta_{0j} = \gamma_{00} + u_{0j}} ~~~ (1.2)$$

$$Mixed~model: ~~~{y_{ij} = \gamma_{00} + \gamma_{10}(sex_{ij}) + u_{0j} + e_{ij}} ~~~(1.3)$$
Here is the r-code for the multilevel model described in equation 1 using the nlme package.  There is the fixed part, which will have the average intercept and the parameter estimate for the scCenter variable.  To model the random intercepts, we use the 1 to signify that this is a random intercepts model where we want intercepts for each school.   
```{r}
library(nlme)
model1 = lme(fixed = like ~ sex, random = ~1 | school, data = dat)
sumModel1 = summary(model1); sumModel1
0.387 / (0.387 +1.802)
```
We are using REML, because it accounts for the included parameters correctly in the standard errors (i.e. making them larger by accounting for the reduced degrees of freedom). 

AIC, BIC, and logLik are all model comparison statistics.

Random effects = The random effects demonstrate the amount of variance (they are reported in standard deviations) in the dependent variable associated with the schools (intercept) and the individuals (residual).  These variances can be used to measure the amount of variance associated between cluster and within cluster using the following equation 2:
$${\rho_{1} = {\tau^2 / (\tau^2 +\sigma^2)} }~~~ (2)$$

When we plug in the random effect standard deviation values we need to square them to get the variances.  We see that almost 20% of the variation in the dependent variable is associated with differences between clusters.  Therefore, there is some indication that students in schools are more alike on the like dependent variable.
$${ .178= {0.387 / (0.387 +1.802))} }$$
Fixed effects: These are interpreted just as a regular single level regression.

Correlation (inter): The correlation between the fixed intercept and fixed slope for sex.  If the value is above zero, then we could say that as the intercept (i.e. initial values) increase so does the slope.  So for a positive relationship, a person who starts with a higher intercept will increase as their score increases.  In our case the opposite is true.  Therefore, as the intercept or starting values for like move up, we see a decrease in the slope for female.

Standardized residuals = These are the residuals scaled by the standard deviation.

Now we can estimate the random slope model.   The random slope model adds one unique component which is u1j.  This is the random level two error for the slope allowing the slope to differ for each school for the covariate female.  It is indexed with j, because it can vary for each school.  It is multiplied by the covariate value of sex, because it is a slope therefore we need to multiply it by the covariate for which it is measuring the slope.
$$ Level~1:~~~{y_{ij} = \beta_{00} + \beta_{1}(sex_{ij}) + e_{ij}}~~~ (3.1)$$

$$ Level~2~Intercept:~~~{\beta_{0j} = \gamma_{00} + u_{0j}} ~~~ (3.2)$$
$$ Level~2~Slope:~~~{\beta_{1j} = \gamma_{10} + u_{1j}} ~~~ (3.2)$$
$$ Mixed~model:~~~{y_{ij} = \gamma_{00} + \gamma_{10}(sex_{ij}) +u_{0j} + u_{1j}(sex_{ij}) + e_{ij}}~~~ 3.3$$
For this model, in nlme, instead of using one, we now use the term that we want to have a random slope which is sex.
```{r}
model2 = lme(fixed = like ~ sex, random = ~ sex | school, data = dat)
sumModel2 = summary(model2); sumModel2
```
The only new parameter provided is the stdDev for the sex variable, which is the standard deviation in the slope coefficients for sex for each school. 

For this model let us revert back to the random intercepts only model found in equation 1.  Now we want to estimate the effects of a level two variable school funding.  This parameter estimate is located in the level two intercept section, because in this model, we are assuming that school funding does not affect the level one slope coefficient, sex; however, we are assuming it does affect the mean or starting value on the dependent like for each school.  One new part of this equation is  gamma01, which is the average slope coefficient for the level two school funding variable.  The second new part is found in the mixed model and it is the covariate school funding (sf), which is multiplied by gamma01, which is its slope coefficient.       

$$ Level~1:~~~{y_{ij} = \beta_{00} + \beta_{1}(sex_{ij}) + e_{ij}}~~~ (4.1)$$
$$ Level~2~Intercept:~~~{\beta_{00} = \gamma_{00} + \gamma_{01} + u_{0j}} ~~~ (4.2)$$
$$Mixed~model: ~~~{y_{ij} = \gamma_{00} + \beta_{10}(sex_{ij}) +\gamma_{01}(sf_{j}) + u_{0j} + e_{ij}} ~~~(4.3)$$
Modeling a level two covariate with random intercepts is easy in nlme.  We just include the level two covariate in the model and nlme does the rest.  The new variable is the school funding (sf) variable which is centered at 0 and has a standard deviation of 20.
```{r}
model3 = lme(fixed = like ~ sex + sf, random = ~ sex | school, data = dat)
sumModel3 = summary(model3); sumModel3
```
Let us slightly change the data set so that we can create a model that makes sense for a random slopes model with the school funding variable.  Let us change the covariate sex to intervention (interven).  Now we can extend the model in equation 3 to include a random slopes component where school funding influences the slope of the coeffient for the intervention.  Below is the new equation:

$$ Level~1:~~~{y_{ij} = \beta_{00} + \beta_{1}(interven_{ij}) + e_{ij}}~~~ (5.1)$$
$$ Level~2~Intercept:~~~{\beta_{00} = \gamma_{00} + \gamma_{01} + u_{0j}} ~~~ (5.2)$$


$$ Level~2~Slope:~~~{\beta_{1j} = \gamma_{10} +\gamma_{11}(sf_{j}) + u_{1j}} ~~~ (5.3)$$
$$Mixed~model: ~~~{y_{ij} = \gamma_{00} + \ + \gamma_{10}(interven_{ij}) +\gamma_{01}(sf_{j}) + \gamma_{11}(sf_{j})(interven_{ij}) + u_{0j} +u_{1j}(interven_{ij}) + e_{ij}} ~~~(5.4)$$
The only new part is gamma11, which is the slope coefficient for the level two variable school funding and is interpreted as a standard regression coefficient. 

Now we have the full mixed model with the level two covariate included in the random intercepts and slopes component of the model.  It is still relatively easy to implement this model in nlme and just add the intervention term for school funding and intervention.
```{r}
head(dat)
colnames(dat) = c("id", "school", "interven", "like", "sf")
model4 = lme(fixed = like ~ interven + sf + interven*sf, random = ~ interven | school, data = dat)
sumModel4 = summary(model4); sumModel4
```
As expected the only new parameter of interest is the fixed interaction term between school funding and the student's intervention status.

References:
Finch, W. H., Bolin, J. E., & Kelley, K. (2014). Multilevel modeling using R. Crc Press.

