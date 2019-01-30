library(tidyverse)
library(ggmap)
library(rgeos)
library(maptools)
library(ggplot2)
library(plotly)

library(shiny)
library(shinydashboard)
library(leaflet)
library(dplyr)
library(tm)
library(SnowballC)
library(wordcloud2)
library(RColorBrewer)
library(shinythemes)
library(reshape2)
library(gridExtra)

# Read in dataframes
terrorism <- readRDS('Data/terrorism.RDS')
terrorism_US <- readRDS('data/terrorism_US.RDS')
bomb_freq <- readRDS('data/bomb_freq.RDS')
hj_freq <- readRDS("data/hj_freq.RDS")
assassination_freq <- readRDS('data/assassination_freq.RDS')
armed_assault_freq <- readRDS('data/armed_assault_freq.RDS')
hostage_freq <- readRDS('data/hostage_freq.RDS')
kidnapping_freq <- readRDS('data/kidnapping_freq.RDS')
facility_attack_freq <- readRDS('data/facility_attack_freq.RDS')
unarmed_assault_freq <- readRDS('data/unarmed_assault_freq.RDS')
unknown_freq <- readRDS('data/unknown_freq.RDS')
US_summary <- readRDS('data/US_summary.RDS')


top_10_groups <- terrorism %>%
  dplyr::filter(gname != "Unknown") %>%
  dplyr::group_by(gname) %>%
  dplyr::summarise(appearances = n()) %>%
  dplyr::top_n(10, appearances) %>%
  dplyr:: pull(gname) 









