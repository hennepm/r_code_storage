### APA-style figure for between-participants study (means and 95% confidence intervals on Likert)

## load packages

library(ggplot)
library(jtools)
library(Rmisc)

## load data

d.1 = read.csv("data.csv")

## figure for between-participants study (means and 95% confidence intervals)

# summary
summ.1 = summarySE(d.1, measurevar = "DV", groupvars= "Condition")

# set position
pd = position_dodge(1)

# plot the plot

plot.1 = ggplot(summ.1, aes(x=Condition, y=DV, color=Condition)) +
	geom_point(stat="identity", position=pd, shape=19, size=3) +
	geom_errorbar(aes(ymin=DV-ci, ymax=DV+ci), position = pd) +
	scale_colour_grey(start=0.1, end=0.6) +
	theme_apa() +
	scale_x_discrete(name="Condition", labels=c("Condition 1", "Condition 2")) +
	scale_y_continuous(name="Mean DV", limits=c(1, 7), breaks=c(1,2,3,4,5,6,7)) +
	theme(legend.position=c(1, 1), legend.justification=c(1, 1))

print(plot.1)

# make the plot colorful for your talk
library(wesanderson)

plot.2 = ggplot(summ.1, aes(x=Condition, y=DV, color=Condition)) +
	geom_point(stat="identity", position=pd, shape=19, size=4) +
	geom_errorbar(aes(ymin=DV-ci, ymax=DV+ci), position = pd) +
	scale_colour_manual(values = wes_palette("Royal1")) +
	theme_classic() +
	guides(fill=FALSE, color=FALSE) +
	scale_x_discrete(name="Condition", labels=c("Condition 1", "Condition 2")) +
	scale_y_continuous(name="Mean DV", limits=c(1, 7), breaks=c(1,2,3,4,5,6,7))
	
print(plot.2)
