# Load packages
library("gosset")
library("PlackettLuce")
# additional functions from ClimMob-analysis 
source("https://raw.githubusercontent.com/AgrDataSci/ClimMob-analysis/master/modules/01_functions.R")

dir.create("output/", showWarnings = FALSE)

sessioninfo::session_info()

capture.output(print(sessioninfo::session_info()), 
               file = "script/session-info/session-info-concept-testing.txt")

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
               "curr_maize", "hh_num")]

dat2 = dat2[,c("country",paste0("video",1:3), 
               "variety_preferred","variety_lesspreferred",
               "X_longitude","X_latitude","intro_video","resp_gender",
               "resp_age", "resp_education", "farm_land", "farm_income",
               "curr_maize_yesno", "hh_num")]

names(dat2) = gsub("_yesno", "", names(dat2))

# put them together
dat = rbind(dat1, dat2)

dat = dat[!is.na(dat$resp_gender), ]

# organize names and data
names(dat) = gsub("registration_", "", names(dat))

# put concept names as title case
head(dat[paste0("video", 1:3)])

dat[paste0("video", 1:3)] = lapply(dat[paste0("video", 1:3)], ClimMobTools:::.title_case)

sort(unique(unlist(dat[paste0("video", 1:3)])))

lapply(dat[paste0("video", 1:3)], function(x){ })

dat[paste0("video", 1:3)] = lapply(dat[paste0("video", 1:3)], function(x){
  x[x=="Drought Avoidance Variety"] = "Drought Avoidance"
  x[x=="Green Maize Variety"] = "Green Maize"
  x[x=="Home Use Variety"] = "Home Use"
  x[x=="Home Use & Family Consumption"] = "Home Use"
  x[x=="Intercropping Variety"] = "Intercropping"
  x[x=="Nutritious Variety"] = "Nutritious"
  x[x=="Resilient Variety"] = "Resilient (benchmark)"
  x[x=="Resilient"] = "Resilient (benchmark)"
  x[x=="Feed Variety"] = "Feed"
  x[x=="Food & Fodder Variety"] = "Food & Fodder"
  x[x=="Feed & Fodder"] = "Food & Fodder"
  x
})

table(unlist(dat[paste0("video", 1:3)]))

# start data analysis by making the grouped rankings
names(dat)

# into rankings
R = rank_tricot(dat,
                paste0("video", 1:3),
                c("variety_preferred", "variety_lesspreferred"))

R = unclass(R)

# select only the resilient variety 
r = R[, "Resilient (benchmark)"]

# combine with the data
D = dat[c("intro_video","resp_gender", "resp_age", "hh_num")]

names(D) = c("gender_vendor", "gender_farmer", "age", "household_size")

D$adopt = r

D = D[union("adopt", names(D))]

D = na.omit(D)

# remove the 0s (not tested)
D$adopt[D$adopt == 2] = 0
D = D[D$adopt > 0, ]
# 3 into 0
D$adopt[D$adopt == 3] = 0

table(as.factor(D$adopt))

table(as.factor(D$adopt))

xtabs(~ adopt + gender_farmer, data = D)

boxplot(age ~ gender_farmer, data = D)

hist(D$household_size)

mod = glm(adopt ~ gender_farmer + gender_vendor + age + household_size, 
          data = D, family = "binomial")

summary(mod)

pseudoR2(mod)

## now we can plot the data
fit = data.frame(prob = mod$fitted.values,
                 observed = as.factor(D$adopt))

fit = fit[order(fit$prob, decreasing=FALSE), ]

fit$rank = 1:nrow(fit)

## Lastly, we can plot the predicted probabilities for each sample having
## heart disease and color by whether or not they actually had heart disease
ggplot(data=fit, aes(x = rank, y = prob)) +
  geom_point(aes(color = observed),
             alpha=1,
             shape=4, 
             stroke=2) +
  xlab("Index") +
  ylab("Predicted probability of adopting")


# now a bit of model testing, intro to ML
# we split the model into two datasets 
# one for training and other for testing
n = nrow(dat)
prop = 0.7
set.seed(200)
s = sample(1:nrow(dat), as.integer(n*prop), replace = FALSE)

# get test and train data
train = dat[s, ]
test = dat[-s, ]

mod = glm(adopt ~ ., data = train, family = "binomial")

summary(mod)

# and the predictions
preds = predict(mod, newdata = test, type = "response")

predition = as.integer(preds > 0.5)

confusion_m = addmargins(table(test$adopt, predition))

colnames(confusion_m) = c("Fail", "Success", "Total")
rownames(confusion_m) = c("Fail", "Success", "Total")

confusion_m

# false positive rate (Type 1 error)
confusion_m[1,2] / (confusion_m[1, 3])

# true positive rate 
confusion_m[2,2] / (confusion_m[2, 3])

# accuracy
(confusion_m[1, 1] + confusion_m[2, 2]) / confusion_m[3, 3]

preds = predict(mod, newdata = test)

p = prediction(preds, test$adopt) 
performance(p, "tpr", x.measure = "fpr")

