# Plotting code to accompany 'Transmission dynamics of social and digital contagion'
# Author: AJ Kucharski (2019)

# - - - - - - - - - - - - - - - - - - - - - 
# Libraries and clear workspace

library(magrittr)
library(lubridate)
library(deSolve)
library(pracma)
library(extrafont)
library(igraph)
library(tidyverse)
library(anytime)
library(CINNA)
library(preseqR)

rm(list=ls())

# - - - - - - - - - - - - - - - - - - - - - 
# Set working directory and load functions

setwd("~/Documents/GitHub/contagion-review/")

source("R/plotting_functions.R")

# - - - - - - - - - - - - - - - - - - - - - 
# Global settings

col.list = c(rgb(0.9,0.6,0),rgb(0.2,0,0.8),rgb(0.1,0.6,0.2),rgb(1,0.4,1),rgb(0,0.6,0.8),rgb(0.8,0,0.2),rgb(0.5,0.5,0.5),rgb(0.6,0,0.6))
network.color= rgb(0.2,0.2,0.2)
width.main=6
height.main=3
height.wide=2
height.square=5

# - - - - - - - - - - - - - - - - - - - - - 
# Functions

# Figure 1 chain plots
fig1_chain_plots()

# Figure 2 reproduction number
fig2_statistics()

# Figure 3 - research effort
fig3_research()



