# Model description:
# Fixed Effects: Condition, Group
# Random Effects: Subject, Item
# Condition and Group are fully crossed
# Subject is nested within Group
# Item is nested within Condition
# Subject and Item are partially crossed (since participants receive half of the move/non-move items)
# You don't need to worry about indicating the nesting explicitly in each model because
# it's already indicated as nested by uniquely coding the levels in each sub and item factor
# see here: https://stats.stackexchange.com/questions/228800/crossed-vs-nested-random-effects-how-do-they-differ-and-how-are-they-specified

# load packages
library(rstan);
require(dplyr);
require(lme4);
require(ggplot2);
require(tidyr);
require(brms);
require(brm);
require(languageR);
require(pbkrtest);
require(data.table)

rm(list=ls())

# Set working directory
setwd('/Users/testing1/Dropbox/Projects/NFB16/BSYNAnalysis');
# Load csv in which individual items are all present
# This data file only contains move and non-move items present in both long and short-forms of the task
# TODO: check that these values match percentages in Excel. Done
# subject and item are coded as unique strings so that they don't repeat across visionGroup or condition, respectively
# CHECK: items are unique between garden-path and garden-path control. However, move and non-move items are matched, but not within subject.
# one subject with get move item 1 and non-move item 2 and another subject will get move item 2 and non-move item 1
# I could rerun this analysis, coding the matched move and non-move items the same
d.alldata <- read.table('BSYN_AllSubjectsForLogit.csv', header=TRUE, sep=',');
# check levels
sapply(d.alldata, levels)
# get descriptive stats
summary(d.alldata)

###################################################################################
# break up data

# Remove false starts
d.alldata <- subset(d.alldata, rt > 0.15)

# TODO: Check move and non-move for number of items = 60
d.gpdata <- subset(d.alldata, manipulation=="gardenpath", select=sub:fail)
d.gpdata <- droplevels(d.gpdata) 
d.movedata <- subset(d.alldata, manipulation=="move", select=sub:fail)
d.movedata <- droplevels(d.movedata)

# clean the RT data to consider correct rts only
d.gpRTdata <- subset(d.alldata, success=="1" & manipulation=="gardenpath", select=sub:rt)
d.gpRTdata <- droplevels(d.gpRTdata)
d.moveRTdata <- subset(d.alldata, success=="1" & manipulation=="move", select=sub:rt)
d.moveRTdata <- droplevels(d.moveRTdata)

# aggregate data and output to check
sum <- aggregate(d.gpdata$success, by=list(d.gpdata$subNameActual, d.gpdata$sub, d.gpdata$condition, d.gpdata$visionGroup), FUN=sum)
count <- aggregate(d.gpdata$success, by=list(d.gpdata$subNameActual, d.gpdata$sub, d.gpdata$condition, d.gpdata$visionGroup), FUN=length)
agg.gpAcc <- sum
agg.gpAcc[5] <- sum[5]/count[5]  # this assumes that subject rows are identical in sum and count & the index of success

sum <- aggregate(d.movedata$success, by=list(d.movedata$subNameActual, d.movedata$sub, d.movedata$condition, d.movedata$visionGroup), FUN=sum)
count <- aggregate(d.movedata$success, by=list(d.movedata$subNameActual, d.movedata$sub, d.movedata$condition, d.movedata$visionGroup), FUN=length)
agg.moveAcc <- sum
agg.moveAcc[5] <- sum[5]/count[5]  # this assumes that subject rows are identical in sum and count & the index of success

mean <- aggregate(d.gpRTdata$rt, by=list(d.gpRTdata$subNameActual, d.gpRTdata$sub, d.gpRTdata$condition, d.gpRTdata$visionGroup), FUN=mean)
agg.gpRT <- mean

mean <- aggregate(d.moveRTdata$rt, by=list(d.moveRTdata$subNameActual, d.moveRTdata$sub, d.moveRTdata$condition, d.moveRTdata$visionGroup), FUN=mean)
agg.moveRT <- mean

remove(count,mean,sum)

setnames(agg.gpAcc, old = c('Group.1','Group.2', 'Group.3', 'Group.4', 'x'), new = c('subNameActual', 'sub', 'condition', 'visionGroup', 'success'))
setnames(agg.gpRT, old = c('Group.1','Group.2', 'Group.3', 'Group.4', 'x'), new = c('subNameActual', 'sub', 'condition', 'visionGroup', 'rt'))
setnames(agg.moveAcc, old = c('Group.1','Group.2', 'Group.3', 'Group.4', 'x'), new = c('subNameActual', 'sub', 'condition', 'visionGroup', 'success'))
setnames(agg.moveRT, old = c('Group.1','Group.2', 'Group.3', 'Group.4', 'x'), new = c('subNameActual', 'sub', 'condition', 'visionGroup', 'rt'))

dlist <- list(d.gpdata, d.gpRTdata, agg.gpAcc, agg.gpRT)
for (i in 1:length(dlist)) {
  dlist[[i]]$condition = factor(dlist[[i]]$condition, levels=c("f-gpf", "f-gp"))
  #contrasts(d.gpRTdata$condition) = contr.sum(2)  this is a fine way to do it, but the labelling is bad
  contrasts(dlist[[i]]$condition) = cbind("gardenPath"=c(0.5,-0.5)); # create readable output of regressor
}
d.gpdata <- dlist[[1]]
d.gpRTdata <- dlist[[2]]
agg.gpAcc <- dlist[[3]]
agg.gpRT <- dlist[[4]]

dlist <- list(d.movedata, d.moveRTdata, agg.moveAcc, agg.moveRT)
for (i in 1:length(dlist)) {
  dlist[[i]]$condition = factor(dlist[[i]]$condition, levels=c("nm", "m"))
  contrasts(dlist[[i]]$condition) = cbind("move"=c(0.5,-0.5)); # create readable output of regressor
}
d.movedata <- dlist[[1]]
d.moveRTdata <- dlist[[2]]
agg.moveAcc <- dlist[[3]]
agg.moveRT <- dlist[[4]]

dlist <- list(d.gpdata, d.gpRTdata, agg.gpAcc, agg.gpRT, d.movedata, d.moveRTdata, agg.moveAcc, agg.moveRT)
for (i in 1:length(dlist)) {
  dlist[[i]]$visionGroup <- factor(dlist[[i]]$visionGroup, levels=c("CB", "S"))
  contrasts(dlist[[i]]$visionGroup) <-cbind("CB"=c(0.5,-0.5)); # create readable output of regressor
}
d.gpdata <- dlist[[1]]
d.gpRTdata <- dlist[[2]]
agg.gpAcc <- dlist[[3]]
agg.gpRT <- dlist[[4]]
d.movedata <- dlist[[5]]
d.moveRTdata <- dlist[[6]]
agg.moveAcc <- dlist[[7]]
agg.moveRT <- dlist[[8]]
remove(dlist)
remove(i)

##############################################
# First do analysis on aggregated data to check results from SPSS

# Garden Path Model
# Random intercept for subject
# These results look the same as the correct/standard analysis SPSS, where individual items were first averaged
# Note that in SPSS, you can model "subject" as a random factor in a between subject analysis, 
# and it will be the same as if you had done a within-subJect (repeated measure) analysis
gp.agg.lmer <- lmer(success ~ condition*visionGroup + (1 | sub), data = agg.gpAcc)
summary(gp.agg.lmer)
# get p values from lmer function t-distribution using the t-values and KR-approximated degrees of freedom
# http://mindingthebrain.blogspot.com/2014/02/three-ways-to-get-parameter-specific-p.html
df.KR <- get_ddf_Lb(gp.agg.lmer, fixef(gp.agg.lmer))
gp.agg.coefs <- data.frame(coef(summary(gp.agg.lmer))) # extract coefficients
gp.agg.coefs$p.KR <- 2 * (1 - pt(abs(gp.agg.coefs$t.value), df.KR))
gp.agg.coefs

# Move Model
move.agg.lmer <- lmer(success ~ condition*visionGroup + (1 |sub), data = agg.moveAcc)
summary(move.agg.lmer)
# get p values from lmer function t-distribution using the t-values and KR-approximated degrees of freedom
# http://mindingthebrain.blogspot.com/2014/02/three-ways-to-get-parameter-specific-p.html
df.KR <- get_ddf_Lb(move.agg.lmer, fixef(move.agg.lmer))
move.agg.coefs <- data.frame(coef(summary(move.agg.lmer))) # extract coefficients
move.agg.coefs$p.KR <- 2 * (1 - pt(abs(move.agg.coefs$t.value), df.KR))
move.agg.coefs

# Other stuff that I tried
# Random intercept and slope for subject. 
# test.lmer = lmer(success ~ condition*visionGroup + (1 + condition|sub), data = agg.gpAcc)
# summary(test.lmer)

# Got this design from a split-plot tutorial, but looks the same as the last
#test.lmer = lmer(success ~ condition*visionGroup + (1 |visionGroup:sub), data = agg.gpAcc)
#summary(test.lmer)

# I think the way to do nesting is to use a : if nested in a fixed factor and to use a \ if nested
# in a random factor
# apparently nesting is done explicitly if you code nested values explicitly
# see here: https://stats.stackexchange.com/questions/228800/crossed-vs-nested-random-effects-how-do-they-differ-and-how-are-they-specified/228814

# you can also do something like: (1 + condition|sub) if you want to add an interaction of condition and subject

##############################################
# Now perform logistic regression on un-aggregated data

# Bayesian Generalized Non-Linear Multilevel Model
# fit <- brm(success ~ condition*visionGroup + (1|sub) + (1|item), family='bernoulli', d.gpdata, prior=set_prior("normal(0,5)", class = "b"));
# summary(fit)
# IMPORTANT: using "control = glmerControl(optimizer = "bobyqa"), nAGQ = 1" or doesn't converge
# Question: glmer will solve if give (1 + visionGroup|sub) or (1 + condition|item) even though subs only appear in 1 vision group and items are specific to each condition?

# This is the generalized linear model without random effects
# Keeping the example for posterity even though the model is incorrect for the data
# gp.acc <- glm(success ~ condition*visionGroup, data = d.gpdata, family='binomial')
# summary(gp.acc)
##############################################

# Movement
with(agg.moveAcc, aggregate(success, by=list(visionGroup, condition), FUN=mean))
with(agg.moveAcc, aggregate(success, by=list(visionGroup, condition), FUN=sd))

move.acc <- glmer(success ~ condition*visionGroup + (1|sub) + (1|item), data = d.movedata, family='binomial', control = glmerControl(optimizer = "bobyqa"), nAGQ = 1)
summary(move.acc)

with(agg.moveRT, aggregate(rt, by=list(visionGroup, condition), FUN=mean))
with(agg.moveRT, aggregate(rt, by=list(visionGroup, condition), FUN=sd))

move.rt <- lmer(rt ~ condition*visionGroup + (1|sub) + (1|item), data = d.moveRTdata)
summary(move.rt)
# get p values from lmer function t-distribution using the t-values and KR-approximated degrees of freedom
# http://mindingthebrain.blogspot.com/2014/02/three-ways-to-get-parameter-specific-p.html
df.KR <- get_ddf_Lb(move.rt, fixef(move.rt))
move.rt.coefs <- data.frame(coef(summary(move.rt))) # extract coefficients
move.rt.coefs$p.KR <- 2 * (1 - pt(abs(move.rt.coefs$t.value), df.KR))
move.rt.coefs

# could also do this more elegantly by meaning over d.gpData by subject success
# and then meaning over visionGroup and condition
with(agg.gpAcc, aggregate(success, by=list(visionGroup, condition), FUN=mean))
with(agg.gpAcc, aggregate(success, by=list(visionGroup, condition), FUN=sd))

# then make model
gp.acc <- glmer(success ~ condition*visionGroup + (1|sub) + (1|item), data = d.gpdata, family='binomial', control = glmerControl(optimizer = "bobyqa"), nAGQ = 1)
summary(gp.acc)

with(agg.gpRT, aggregate(rt, by=list(visionGroup, condition), FUN=mean))
with(agg.gpRT, aggregate(rt, by=list(visionGroup, condition), FUN=sd))

gp.rt <- lmer(rt ~ condition*visionGroup + (1|sub) + (1|item), data = d.gpRTdata)
summary(gp.rt)
# get p values from lmer function t-distribution using the t-values and KR-approximated degrees of freedom
# http://mindingthebrain.blogspot.com/2014/02/three-ways-to-get-parameter-specific-p.html
df.KR <- get_ddf_Lb(gp.rt, fixef(gp.rt))
gp.rt.coefs <- data.frame(coef(summary(gp.rt))) # extract coefficients
gp.rt.coefs$p.KR <- 2 * (1 - pt(abs(gp.rt.coefs$t.value), df.KR))
gp.rt.coefs


## put everything together
d.alldata$visionGroup <- factor(d.alldata$visionGroup, levels=c("CB", "S"))
contrasts(d.alldata$visionGroup) <-cbind("CB"=c(0.5,-0.5)); # create readable output of regressor

d.alldata$manipulation <- factor(d.alldata$manipulation, levels=c("gardenpath", "move"))
contrasts(d.alldata$manipulation) <-cbind("manipulation"=c(0.5,-0.5)); # create readable output of regressor

d.alldata$isComplex <- factor(d.alldata$isComplex, levels=c("simple", "complex"))
contrasts(d.alldata$isComplex) <-cbind("complexity"=c(0.5,-0.5)); # create readable output of regressor

all.acc <- glmer(success ~ manipulation*visionGroup*isComplex + (1|sub) + (1|item), data = d.alldata, family='binomial', control = glmerControl(optimizer = "bobyqa"), nAGQ = 1)
summary(all.acc)

all.agg.acc <- glmer(success ~ visionGroup*isComplex + (1|sub) + (1|item), data = d.alldata, family='binomial', control = glmerControl(optimizer = "bobyqa"), nAGQ = 1)
summary(all.agg.acc)

# get stats on missing data
count <- aggregate(d.alldata$success, by=list(d.alldata$subNameActual, d.alldata$sub, d.alldata$condition, d.alldata$visionGroup), FUN=length)

with(count, aggregate(x, by=list(Group.3), FUN=mean))

write.csv(agg.moveAcc, 'move.csv')
write.csv(agg.gpAcc, 'gp.csv')
write.csv(agg.moveRT, 'moveRT.csv')
write.csv(agg.gpRT, 'gpRT.csv')
