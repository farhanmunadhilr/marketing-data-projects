###################
# Factor Analysis #
###################

## Install Packages (if needed)
install.packages("nFactors")
install.packages("dplyr")
install.packages("GPArotation")
install.packages("gplots")
install.packages("RColorBrewer")

## Load Packages and Set Seed
library(nFactors)
library(dplyr)
library(GPArotation)
library(gplots)
library(RColorBrewer)
set.seed(1)

## Read in Factor Analysis data
retailer_survey <- read.csv(file.choose()) ## Choose retailer_survey.csv file

## Look at Descriptive Statistics
retailer.factor.mean <- aggregate(. ~ Retailer, data=retailer_survey, mean)
retailer.factor.mean

## Determine Number of Factors
retailer_factors <- select(retailer_survey, -Retailer) ## Remove retailer column
eigen(cor(retailer_factors))$values

## Run Factor Analysis with 4 factors
factanal(retailer_factors, factors = 4)

## Run Factor Analysis with 4 factors and oblique rotation
retailer.fa <- factanal(retailer_factors, factors = 4, rotation="oblimin", scores="Bartlett")

## Print a Heatmap of Factor Loadings
heatmap.2(retailer.fa$loadings, col=brewer.pal(9, "Reds"), trace="none", key=FALSE, dend="none",
	Colv=FALSE, cexCol = 1, margins=c(12,8), main="Factor Loadings from Survey")

## Aggregate Factor Scores by Retailer
retailer.scores <- data.frame(retailer.fa$scores)
retailer.scores$retailer <- retailer_survey$Retailer
retailer.fa.mean <- aggregate(. ~ retailer, data=retailer.scores, mean)
rownames(retailer.fa.mean) <- retailer.fa.mean[,1]
retailer.fa.mean <- select(retailer.fa.mean, -retailer)
names(retailer.fa.mean) <- c("Innovative", "High Quality", "Loyalty", "Good Value") 
retailer.fa.mean

## Print a Heatmap of Retailer Scores
heatmap.2(as.matrix(retailer.fa.mean), col=brewer.pal(9, "Blues"), trace="none", key=FALSE, dend="none",
	Colv=FALSE, cexCol = 1, margins=c(12,8), main="Factor Score by Retailer")



