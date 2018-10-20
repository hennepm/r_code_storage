### figure for mulilevel mediation

## load packages

library(lme4)
library(mediation)
library(wesanderson)
library(diagram)
library(MuMIn)

## set working directory
setwd("wd")

## load data
d.1 = read.csv("yourdata.csv")

## mediation example

# Independent Variable (IV) on Mediating Variable (MV).
# random intercept is ID for this example

ma.1 = lmer(MV ~ IV + (1|ID), data = d.1, REML = FALSE)

# Dependent Variable (DV) predicted by IV with MV in model

mb.1 = lmer(DV ~ MV + IV + (1|ID), data = d.1, REML = FALSE)

# DV by MV

mc.1 = lmer(DV ~ MV + (1|ID), data = d.1, REML = FALSE)

# mediation

medi.1 = mediate(ma.1, mb.1, treat = "IV", mediator = "MV", control.value = "IV.1", treat.value = "IV.2")

## mediation plot

openplotmat()
ColScheme = wes_palette("Darjeeling1")
polygon(x = c(.08, .48, .9), c(.15,.75, .15), col = ColScheme[3], border = NA)
curvedarrow(from = c(.08,.15), to = c(.9,.15), curve = -0.1, lty = 1, lwd =(1-medi.1$n0)*9, lcol = ColScheme[2])
curvedarrow(from = c(.08,.15), to = c(.9,.15), curve = -0.75, lty = 1, lwd = medi.1$n0*9, lcol = ColScheme[1])
textplain(c(.08,.12), .1, 'IV', col = ColScheme[6], font = 2)
textplain(c(.48,.86), .1, 'MV', col = ColScheme[6], font = 2)
textplain(c(.91,.12), .1, 'DV', col = ColScheme[6], font = 2)
textplain(c(.48,.8), .1, paste('ACME =', round(medi.1$d0, 2)), col = ColScheme[6], font = 2)
textplain(c(.48,.26), .1, paste('ADE =', round(medi.1$z0, 2)), col = ColScheme[6], font = 2)
textplain(c(.48,.05), .1, paste('Total effect =', round(medi.1$tau.coef, 2)), col = ColScheme[6], font = 2)
textplain(c(.68,.49), .1, paste('Marginal R squared =', round(r.squaredGLMM(mc.1)[1],4)), col = ColScheme[5], srt = -53)
textplain(c(.29,.49), .1, paste('Marginal R squared =', round(r.squaredGLMM(ma.1)[1],4)), col = ColScheme[5], srt = 53)
textplain(c(.48,.12), .1, paste('Marginal R squared =', round(r.squaredGLMM(mb.1)[1],4)), col = ColScheme[5])