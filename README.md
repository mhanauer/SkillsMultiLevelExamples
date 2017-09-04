---
title: "SkillsMultiLevel Examples"
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
```{r}

```


```{r}
library(lme4)
model1 = lmer(cbm ~ sc + (1 | school), data = dat)
summary(model1)
```


