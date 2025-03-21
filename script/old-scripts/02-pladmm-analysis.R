# This analysis will assess the effect of maize intrinsic traits on 
# farmers' decision 
# Load packages ####
library("gosset")
library("PlackettLuce")
library("ClimMobTools")
library("janitor")
library("caret")
library("gtools")
# additional functions from ClimMob-analysis 
source("https://raw.githubusercontent.com/AgrDataSci/ClimMob-analysis/master/modules/01_functions.R")
# source("/Users/kauedesousa/Library/Mobile Documents/com~apple~CloudDocs/Work/Rcode/ClimMob-analysis/modules/01_functions.R")

dir.create("output/", showWarnings = FALSE)

sessioninfo::session_info()

capture.output(print(sessioninfo::session_info()), 
               file = "script/session-info/session-info-02-pladmm-analysis.txt")

# Read the data #####
# this is a csv file previously treated from the original 
# .json file with the farmers' responses on the preferred concepts
list.files("data")

dat1 = read.csv('data/kenya.csv')
dat1$country = "Kenya"
dat2 = read.csv('data/uganda.csv')
dat2$country = "Uganda"

# read the variety traits
traits = read.csv("data/maize-concepts-traits.csv")

# Organize farmers' response data ####
# country, video1:3, best and worst, lonlat, gender
dat1 = dat1[,c("country",paste0("video",1:3), 
               "variety_preferred","variety_lesspreferred",
               "X_longitude","X_latitude","intro_video","resp_gender")]

dat2 = dat2[,c("country",paste0("video",1:3), 
               "variety_preferred","variety_lesspreferred",
               "X_longitude","X_latitude","intro_video","resp_gender")]

# put them together
dat = rbind(dat1, dat2)

dat = dat[!is.na(dat$resp_gender), ]

# organize names and data
names(dat) = gsub("registration_", "", names(dat))

dat$intro_video = paste0(dat$intro_video, " Agrodealer")

# put concept names as title case
head(dat[paste0("video", 1:3)])

dat[paste0("video", 1:3)] = lapply(dat[paste0("video", 1:3)], ClimMobTools:::.title_case)

sort(unique(unlist(dat[paste0("video", 1:3)])))

dat[paste0("video", 1:3)] = lapply(dat[paste0("video", 1:3)], function(x){
  x[x=="Drought Avoidance Variety"] = "Drought Avoidance"
  x[x=="Green Maize Variety"] = "Green Maize"
  x[x=="Home Use Variety"] = "Home Use & Family Consumption"
  x[x=="Intercropping Variety"] = "Intercropping"
  x[x=="Nutritious Variety"] = "Nutritious"
  x[x=="Resilient Variety"] = "Resilient"
  x[x=="Feed Variety"] = "Feed"
  x[x=="Food & Fodder Variety"] = "Feed & Fodder"
  x
})

table(unlist(dat[paste0("video", 1:3)]))


# .........................................
# .........................................
# Organize varieties' trait data ####
names(traits) = make_clean_names(names(traits))

traits$variety

traits$variety = sapply(traits$variety, function(x){
  x[x=="Drought Avoidance Variety"] = "Drought Avoidance"
  x[x=="Green Maize Variety"] = "Green Maize"
  x[x=="Home Use Variety"] = "Home Use & Family Consumption"
  x[x=="Intercropping Variety"] = "Intercropping"
  x[x=="Nutritious Variety"] = "Nutritious"
  x[x=="Resilient Variety"] = "Resilient"
  x[x=="Feed Variety"] = "Feed"
  x[x=="Food / fodder Variety"] = "Feed & Fodder"
  x
})

# confirm that the concepts are in both datasets
all(traits$variety %in% dat$video1)

# the traits should be used as integer, lets use scales for each 
traits$grain_yield
# lets assume a distribution between 0.6 and 1 for yield
quantile(c(0.6, 1), c(0., 0.5, 1))
names(traits)

sel = c("grain_yield", "fertilizer_needs", "stress_tolerance")

traits[sel] = lapply(traits[sel], function(x){
  x[x=="Low"] = 3#0.6
  x[x=="Medium"] = 2#0.8
  x[x=="High"] = 1#1
  x = as.numeric(x)
  x
})

traits$color = as.integer(factor(traits$color, levels = c("White", "Yellow", "Orange")))

traits$food = as.integer(!grepl("Food", traits$use)) + 1

traits$sales = as.integer(!grepl("Sales", traits$use)) + 1

traits$feed = as.integer(!grepl("Feed", traits$use)) + 1

# now select the traits
sel = c("variety", "grain_yield", "fertilizer_needs", "stress_tolerance",
        "food", "feed", "sales")


variety_features = traits[sel]

variety_features

names(variety_features)[-1] = gsub("_", " ", names(variety_features)[-1])

names(variety_features)[-1] = ClimMobTools:::.title_case(names(variety_features)[-1])

names(variety_features)[-1] = gsub(" ", "", names(variety_features)[-1])

variety_features

# check variance
out = nearZeroVar(variety_features)

variety_features = variety_features[, -out]

variety_features

# PLADMM analysis ####
R = rank_tricot(dat,
                paste0("video", 1:3),
                c("variety_preferred", "variety_lesspreferred"),
                group = FALSE)

sort(variety_features$variety)

# simple PL model using only the rankings
mod = pladmm(R, ~ variety, data = variety_features)

summary(mod)

# compare worth between PLADMM and the standard PL model
cbind(pladmm = itempar(mod), pl = itempar(PlackettLuce(R)))


# PLADMM model using the variety features
form = paste("~", paste0(names(variety_features)[-1], collapse = " + "))
form = as.formula(form)
form

mod1 = pladmm(R, formula = form, data = variety_features)

summary(mod1)

pladmm_coeffs(mod1)

coef(mod1)

sort(itempar(mod1))

# PLADMM with PLtree
farmer_features = dat[,c("resp_gender", "country")]
names(farmer_features) = c("Gender", "Country")
farmer_features[,c(1:2)] = lapply(farmer_features[,c(1:2)], as.factor)

variety = group(R, index = 1:length(R))

tree = pltree(variety ~ Gender + Country,
              worth = form,
              data = list(farmer_features, variety_features),
              alpha = 1)

summary(tree)

capture.output(summary(mod1), file = "output/pladmm-whole-data.txt")

capture.output(summary(tree), file = "output/pladmm-tree-data.txt")


