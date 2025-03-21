# .......................................
# .......................................
# This script runs a text analysis to produce the 
# word clouds with the open responses indicating the 
# reasons for selecting or rejecting the maize concepts 
# presented in the tricot incomplete blocks
library("tidyverse")
library("magrittr")
library("tidytext")
library("tm")

# .......................................
# .......................................
# .......................................
# Organize the data ####
# read the data from both countries. 
# this is a csv file previously treated from the original 
# .json file with the farmers' responses on the preferred concepts
dat1 = read.csv('data/kenya.csv')
dat1$country = "Kenya"
dat2 = read.csv('data/uganda.csv')
dat2$country = "Uganda"

# library with clean words
words = read.csv("data/agri-words-library.csv")

words$input = paste0(words$input, "$")

# country, video1:3, best and worst, lonlat, gender
dat1 = dat1[,c("country",paste0("video",1:3), 
               "variety_preferred","variety_lesspreferred",
               "resp_gender",
               "reason_preferred","reason_lesspreferred")]
head(dat1)

dat2 = dat2[,c("country",paste0("video",1:3), 
               "variety_preferred","variety_lesspreferred",
               "resp_gender",
               "reason_preferred","reason_lesspreferred")]

head(dat2)

# put them together
dat = rbind(dat1, dat2)

dat = dat[!is.na(dat$resp_gender), ]

rm(dat1, dat2)

head(dat)

# put concept names as title case
head(dat[paste0("video", 1:3)])

dat[paste0("video", 1:3)] = lapply(dat[paste0("video", 1:3)], ClimMobTools:::.title_case)

sort(unique(unlist(dat[paste0("video", 1:3)])))

lapply(dat[paste0("video", 1:3)], function(x){ })

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

# Get the names of the best and worst options
dat$best = ifelse(dat$variety_preferred == "A", dat$video1, 
                  ifelse(dat$variety_preferred == "B", dat$video2,
                         ifelse(dat$variety_preferred == "C", dat$video3, NA)))

dat$worst = ifelse(dat$variety_lesspreferred == "A", dat$video1, 
                   ifelse(dat$variety_lesspreferred == "B", dat$video2,
                          ifelse(dat$variety_lesspreferred == "C", dat$video3, NA)))


sort(table(dat$best))
sort(table(dat$worst))

items = unique(dat$video1)

sort(items)

# .......................................
# .......................................
# .......................................
# Data for text analysis ####

dat$reason_preferred = gsub("[[:punct:]]", "", dat$reason_preferred)
dat$reason_lesspreferred = gsub("[[:punct:]]", "", dat$reason_lesspreferred)

# first, work on the reason for the preference
like = dat %>% 
  select(best, reason_preferred, country, resp_gender) %>% 
  mutate(preference = "Preferred") %>% 
  rename(concept = best,
         word = reason_preferred,
         gender = resp_gender)

dislike = dat %>% 
  select(worst, reason_lesspreferred, country, resp_gender) %>% 
  mutate(preference = "LessPreferred") %>% 
  rename(concept = worst,
         word = reason_lesspreferred,
         gender = resp_gender) 

dat = rbind(like, dislike)

dat$word = tolower(dat$word)

dat$word = gsub("[^[:alnum:]]", " ", dat$word)
dat$word = gsub("[[:digit:]]+", " ", dat$word)

head(dat)

dat$id = 1:nrow(dat)

dat = 
  dat %>% 
  group_by(id, preference, concept, gender, country) %>% 
  unnest_tokens("word", word)

for (i in seq_along(words$input)) {
  dat$word = gsub(words$input[i], words$standardised[i], dat$word)
}

dat$word = removeWords(dat$word, stopwords("en"))

# words with less than 2 characters 
keep = nchar(dat$word) > 2 & dat$word != ""

dat = dat[keep, ]

dat = split(dat, dat$id)

dat = lapply(dat, function(x){
  data.frame(concept = unique(x$concept),
             preference = unique(x$preference),
             country = unique(x$country),
             gender = unique(x$gender),
             word = paste(x$word, collapse = " "))
})

dat = do.call("rbind", dat)

head(dat)

write.csv(dat, 
          "data/text-data-open-question-full-statement.csv",
          row.names = FALSE)


