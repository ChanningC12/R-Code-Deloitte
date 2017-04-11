data.df = data.frame(topic=c(rep(c("Gossip","Sports","Weather"),each=4)),
                     duration = c(6:9, 2:5, 4:7))
View(data.df)
attach(data.df)
boxplot(duration~topic, data=data.df, ylab="Duration of conversation")

model.lm = lm(duration~topic, data=data.df)
rsq = summary(model.lm)$r.squared
sqrt(rsq)

print(model.lm$fitted.values)

cor(data.df$duration, model.lm$fitted.values)
# The correlation between fitted value predicted by the categorical value and the actual value #

# One-way ANOVA
# ANOVA is to test differences between two or more means.
# Chi-square test is intended to test how likely it is that an observed distribution is due to chance
library(heplots)
model.aov = aov(duration~topic, data=data.df)
summary(model.aov)
etasq(model.aov,partial=F)

# Chi-square test, test how likely it is that an observed distribution is due to chance
library(MASS)
View(survey)
help(survey)
tbl = table(survey$Smoke, survey$Exer)
tbl
chisq.test(tbl)

# ICC: intraclass correlation coefficient
library(irr)
data(anxiety)
View(anxiety)
help(anxiety)
icc(anxiety,model="twoway",type="agreement")

r1 = round(rnorm(20,10,4))
r2 = round(r1+10+rnorm(20,0,2))
r3 = round(r1+20+rnorm(20,0,2))
icc(cbind(r1,r2,r3),"twoway")


## Cramer's V, a measure of association for nominal variables. Effectively it is the Pearson chi-square statistics rescaled to have values between 0 and 1
library(lsr)
cramersV(tbl)










