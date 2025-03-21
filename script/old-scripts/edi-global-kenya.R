# This script is used to prepare the tricot data to run the analysis locally.
# You can use this for example to add environmental variables and/or 
# combine results from different projects. All the process is manual and you need to 
# adapt some parts of this script depending on the type of data that you are dealing with. 
# Mostly depending on how you designed your tricot project(s) and how similar they are to
# each other in terms of data structure and behaviour.
# Good luck and contact me if you need support and/or advice
# Kauê de Sousa

# Load packages
library("ClimMobTools")
library("gosset")
library("PlackettLuce")
library("partykit")
library("qvcalc")
library("psychotools")
library("jsonlite")
library("knitr")
library("rmarkdown")
library("pls")
library("gtools")
library("ggplot2")
library("igraph")
library("ggrepel")
library("ggparty")
library("patchwork")
library("leaflet")
library("mapview")
library("multcompView")
library("png")
library("plotrix")
library("gridExtra")
library("caret")
library("janitor")
library("nasapower")
library("climatrends")

# .........................................
# Load modules
modules <- list.files(paste0("modules"), full.names = TRUE)
modules <- modules[-which(grepl("check_packages.R", modules))]
for (i in seq_along(modules)) {
  source(modules[i])
}

dat1 <- read.csv('run-local/local/data/edi-global/kenya.csv')
dat1$country <- "Kenya"
dat2 <- read.csv('run-local/local/data/edi-global/uganda.csv')
dat2$country <- "Uganda"

# country, video1:3, best and worst, lonlat, gender
dat1 <- dat1[,c("country",paste0("video",1:3), 
                "variety_preferred","variety_lesspreferred",
                "X_longitude","X_latitude","intro_video","resp_gender")]

dat2 <- dat2[,c("country",paste0("video",1:3), 
                "variety_preferred","variety_lesspreferred",
                "X_longitude","X_latitude","intro_video","resp_gender")]

cmdata <- rbind(dat1, dat2)

cmdata <- cmdata[!is.na(cmdata$resp_gender), ]

# It is important to have all the data in one single data frame 
# This means that all columns should match
# You need to check the columns names in each data frame in the list 
# Sometimes they don't have the same name due to non standardization of 
# data collection moment
# So try to fix it, using grepl() gsub() or other replacement functions
names(cmdata) <- gsub("registration_", "", names(cmdata))

cmdata$intro_video <- paste0(cmdata$intro_video, " Agrodealer")

cmdata[paste0("video", 1:3)] <- lapply(cmdata[paste0("video", 1:3)], title_case)

sort(unique(unlist(dat2[paste0("video", 1:3)])))

cmdata[paste0("video", 1:3)] <- lapply(cmdata[paste0("video", 1:3)], function(x){
  x[x=="Drought Avoidance Variety"] <- "Drought Avoidance"
  x[x=="Green Maize Variety"] <- "Green Maize"
  x[x=="Home Use Variety"] <- "Home Use & Family Consumption"
  x[x=="Intercropping Variety"] <- "Intercropping"
  x[x=="Nutritious Variety"] <- "Nutritious"
  x[x=="Resilient Variety"] <- "Resilient"
  x[x=="Feed Variety"] <- "Feed"
  x[x=="Food & Fodder Variety"] <- "Feed & Fodder"
  x
})

table(unlist(cmdata[paste0("video", 1:3)]))

names(cmdata) <- gsub("[(]", "-", names(cmdata))
names(cmdata) <- gsub("[)]", "", names(cmdata))

# ................................
# Make list of parameters ####
# Now we should create a data frame with parameters for the traits
# this will be used internally by ClimMob.R to look for the string patterns in 
# the data and sort out for the analysis 
traits <- data.frame(nameString1 = "variety_preferred",
                           nameString2 = "variety_lesspreferred",
                           nQst = 2,
                           name = "Preference",
                           codeQst = "preference",
                           questionAsked1 = "Please select your most preferred variety – that is, the variety that you’d be most interested in obtaining and using if it were available at your local agrodealer shop? ",
                           questionAsked2 = "Please select your least preferred variety – that is, the variety that you would be least interesting in using if it were available in your local agrodealer shop?",
                           assessmentId = "registration",
                           assessmentName = "registration",
                           assessmentDay = "0",
                           traitOrder = "referenceTrait")

# Define the reference trait
# Select one among the vector in nameString1
traits$nameString1

# Check if there is a comparison with the local
# if not create this object 
tricotVSlocal <- data.frame()

# ............................................
# ............................................
# Select the covariates
# Here you select covariates from cmdata and/or 
# can add external covariates to cmdata data like environmental indices
# or socio-economic data from a different survey
# You need to build a data.frame with 
# codeQst = a vector with clean names to be displayed in the report, indicating what
#  the covariates mean
# namaString = a vector with names that match the covariates selected in cmdata 
covariates <- data.frame()

# Put the parameters in the list
pars <- list(traits = traits, tricotVSlocal = tricotVSlocal, covariates = covariates)

# ................................................................
# ................................................................
# ................................................................
# ................................................................
# Dataset parameters ####
tag <- "Maize EDI Global" # the project name
reference   <- "Green Maize" # the reference item for the analysis
projname <- paste(tag, reference, sep = "-")
outputpath  <- paste0(getwd(), "/run-local/output/", projname)
infosheets  <- FALSE # logical, if infosheets should be written TRUE FALSE
language    <- "en" # the language to write the report
extension   <- "docx" # report file format it can be "docx", "pdf", and "html"
ranker      <- "participant" # how the system will refer to participants/farmers
option      <- "variety" # how the system will refer to tested items
fullpath    <- getwd() # this is backward path
minN        <- 5 # minimum n of complete data required in a trait evaluation before it is excluded
minitem     <- 2 # minimum n of items tested, e.g. that all items are tested at least twice
mincovar    <- 0.90 # minimum proportion of covariates compared to total valid n
sig_level   <- 0.5 # significance level
sig_level_tree   <- 0.5 # significance tree level
minsplit     <- 30 # minimum n in each tree node
groups       <- c("country", "gender")
language <- "en"

dir.create(outputpath, showWarnings = FALSE, recursive = TRUE)


rank_dat <- organize_ranking_data(cmdata, 
                                  pars, 
                                  projname,
                                  groups, 
                                  option_label = option,
                                  ranker_label = ranker,
                                  reference_tech = reference,
                                  tech_index = paste0("video", 1:3))

overview_and_summaries <- get_overview_summaries(cmdata, rank_dat)

png(filename = paste0(outputpath, "/trial_network.png"),
    width = 20,
    height = 20,
    units = "cm",
    res = 300)
plot(overview_and_summaries$trial_connectivity)
dev.off()


write.csv(overview_and_summaries$summary_table_tech,
          file = paste0(outputpath, "/options_tested.csv"),
          row.names = FALSE)

map <- get_testing_sites_map(cmdata, output_path = outputpath)

PL_models <- get_PlackettLuce_models(cmdata, rank_dat)

log_worth1 <- PL_models$logworth_plot_groups[[1]] + PL_models$logworth_plot_groups[[2]]

ggsave(paste0(outputpath, "/log-worth_plot_by_gender_kenya.png"),
       plot = log_worth1,
       width = 33,
       height = 15,
       units = "cm",
       dpi = 400)

log_worth2 <- PL_models$logworth_plot_groups[[3]] + PL_models$logworth_plot_groups[[4]]

ggsave(paste0(outputpath, "/log-worth_plot_by_gender_uganda.png"),
       plot = log_worth2,
       width = 33,
       height = 15,
       units = "cm",
       dpi = 400)

ggsave(paste0(outputpath, "/log-worth_plot_all_together.png"),
       plot = PL_models$logworth_grouped_rank,
       width = 20,
       height = 16,
       units = "cm",
       dpi = 400)
