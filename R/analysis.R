rm(list=ls(all=TRUE))


library(car)
library(gmodels)
library(lme4)
library(lubridate)
library(MASS)
library(nlme)
library(packrat)
library(plyr)
library(reshape)
library(xlsx)
library(contrast)
library(effects)
library(ggplot2)
library(xtable)

source("R/functions.R")

################
# Process data #
################
# source("R/process.combine.data.R")

# load data
load("output//data//FACE_lysimeter.RData")


#########
# Stats #
#########
source("R/Stats.R")



