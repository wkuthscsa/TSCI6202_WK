library(rio)
library(survival)
library(dplyr)
library(survminer)
library(ggsurvfit)
library(jskm)
library(superheat)
library(tigris)
library(sf)
library(tmap)
library(pander)
library(tidygeocoder)

options(datatable.na.strings=c('NULL',''));
demographics <- import("output/csv/patients.csv") %>% mutate(BIRTHDATE=as.Date(BIRTHDATE,format='%m/%d/%Y'), DEATHDATE=as.Date(DEATHDATE,format='%m/%d/%Y'), 
                                                             timetoevent=coalesce(DEATHDATE,max(DEATHDATE, na.rm=TRUE)) - BIRTHDATE,
                                                             timetoevent=(as.numeric(timetoevent)/365.25),
                                                             censor=!is.na(DEATHDATE), 
                                                             survival=Surv(timetoevent,event=censor))
survivalmodel <-survfit(survival~STATE, demographics)

source("GeoDATA_wjk260402.R")


