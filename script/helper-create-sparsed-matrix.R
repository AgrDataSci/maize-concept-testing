# .......................................
# .......................................
# This script runs the PlackettLuce model 
# to assess farmers preferences 
# Load packages
library("gosset")
library("PlackettLuce")

# read the data from both countries. 
# this is a csv file previously treated from the original 
# .json file with the farmers' responses on the preferred concepts
dat1 = read.csv('data/kenya.csv')
dat1$country = "Kenya"
dat2 = read.csv('data/uganda.csv')
dat2$country = "Uganda"

# country, video1:3, best and worst, lonlat, gender
dat1 = dat1[,c("country",paste0("video",1:3), 
               "variety_preferred","variety_lesspreferred",
               "X_longitude","X_latitude","intro_video","resp_gender",
               "resp_age", "resp_education", "farm_land", "farm_income",
               "curr_maize", "hh_num", "farm_area")]

dat2 = dat2[,c("country",paste0("video",1:3), 
               "variety_preferred","variety_lesspreferred",
               "X_longitude","X_latitude","intro_video","resp_gender",
               "resp_age", "resp_education", "farm_land", "farm_income",
               "curr_maize_yesno", "hh_num", "farm_area")]

names(dat2) = gsub("_yesno", "", names(dat2))

# put them together
dat = rbind(dat1, dat2)

dat = dat[!is.na(dat$resp_gender), ]

# organize names and data
names(dat) = gsub("registration_", "", names(dat))

# standardize concept names
head(dat[paste0("video", 1:3)])

dat[paste0("video", 1:3)] = lapply(dat[paste0("video", 1:3)], ClimMobTools:::.title_case)

sort(unique(unlist(dat[paste0("video", 1:3)])))

dat[paste0("video", 1:3)] = lapply(dat[paste0("video", 1:3)], function(x){
  x = gsub(" Variety", "", x)
  x[x=="Drought Avoidance"] = "Drought avoidance"
  x[x=="Green Maize"] = "Green maize"
  x[x=="Home Use"] = "Home use"
  x[x=="Home Use & Family Consumption"] = "Home use"
  x[x=="Intercropping"] = "Intercropping (with beans)"
  x[x=="Nutritious"] = "Family nutrition"
  x[x=="Resilient"] = "Resilience"
  x[x=="Feed"] = "Livestock feed"
  x[x=="Food & Fodder"] = "Food and fodder"
  x[x=="Feed & Fodder"] = "Food and fodder"
  x
})

table(unlist(dat[paste0("video", 1:3)]))

# start data analysis by making the grouped rankings
names(dat)

# .........................................
# .........................................
# PlackettLuce model ####
R = rank_tricot(dat,
                paste0("video", 1:3),
                c("variety_preferred", "variety_lesspreferred"),
                group = FALSE)

# get sparsed matrix 
M = unclass(R)

write.csv(M, "data/sparsed-matrix-rankings.csv", row.names = F)




