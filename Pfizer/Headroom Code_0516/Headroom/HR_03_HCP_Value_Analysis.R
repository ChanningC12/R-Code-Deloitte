rm(list=ls())
gc()
library(dplyr)
library(cluster)


#######################################################################################################
# Perform Analysis on Specialties
#######################################################################################################

for (i in c(
            "ALLG",
            "ANE",
            "CARD",
            "CHP",
            "DDS",
            "DERM",
            "DIAB",
            "ELCT",
            "ENDO",
            "IND",
            "NEPH",
            "NEUR",
            "NP/PA",
            "NSU",
            "OBG",
            "ONCO",
            "OPTH",
            "ORTH",
            "OTH",
            "PAIN",
            "PCP",
            "PEDS",
            "PODS",
            "PSY",
            "PULM",
            "RHU",
            "URO",
            "VMD",
            "OPT",
            "OPD"
)){
     k=4
     spec=i
     source("D:\\Soumyajit\\R Code\\get_cohort_summaries_0328.r")
   }
