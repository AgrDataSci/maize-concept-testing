# .......................................
# .......................................
# This script runs the PlackettLuce model 
# to assess farmers preferences 
# Load packages
library("gosset")
library("PlackettLuce")
library("ClimMobTools")
library("multcompView")
library("ggplot2")
library("ggparty")
library("patchwork")
# additional functions from ClimMob-analysis 
source("https://raw.githubusercontent.com/AgrDataSci/ClimMob-analysis/master/modules/01_functions.R")
# source("/Users/kauedesousa/Library/Mobile Documents/com~apple~CloudDocs/Work/Rcode/ClimMob-analysis/modules/01_functions.R")

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

dat$intro_video = paste0(dat$intro_video, " Agrodealer")

# put concept names as title case
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
  x[x=="Resilience"] = "Resilient"
  x[x=="Resilient"] = "Resilient (benchmark)"
  x[x=="Feed"] = "Livestock feed"
  x[x=="Food & Fodder"] = "Food and fodder"
  x[x=="Feed & Fodder"] = "Food and fodder"
  x[x=="Drought avoidance"] = "Drought escape"
  x
})

table(unlist(dat[paste0("video", 1:3)]))

# start data analysis by making the grouped rankings
names(dat)

# .........................................
# .........................................
# PlackettLuce model ####
G = rank_tricot(dat,
                paste0("video", 1:3),
                c("variety_preferred", "variety_lesspreferred"),
                group = TRUE)


mod = PlackettLuce(G)

paste(names(coef(mod)), collapse = "', '")

lvls = c('Resilient (benchmark)', 'Drought escape', 'Food and fodder', 'Home use',
         'Green maize', 'Livestock feed', 'Intercropping (with beans)', 'Family nutrition')

lvls = sort(lvls)

lvls

ref = "Resilient (benchmark)"

# .........................................
# .........................................
# plot with worth values ####
logworth_all = 
  plot_logworth(mod, ref = ref, levels = lvls) + 
  labs(y = "Product concept",
       x = "Log-worth") + 
  theme(axis.title = element_text(size = 13),
        axis.text.x = element_text(angle = 0),
        strip.text = element_blank()) 

logworth_all

ggsave("output/log-worth-all.png",
       plot = logworth_all,
       width = 15,
       height = 10,
       units = "cm",
       dpi = 600)

ggsave("output/log-worth-all.pdf",
       plot = logworth_all,
       width = 15,
       height = 10,
       units = "cm",
       dpi = 600)


worth_all = 
  plot_worth(mod, levels = lvls) + 
  labs(y = "Product concept",
       x = "Probability of being selected (worth)") + 
  theme(axis.title = element_text(size = 13),
        axis.text.x = element_text(angle = 0),
        strip.text = element_blank()) 

worth_all

ggsave("output/worth-all.png",
       plot = worth_all,
       width = 15,
       height = 10,
       units = "cm",
       dpi = 600)

ggsave("output/worth-all.pdf",
       plot = worth_all,
       width = 15,
       height = 10,
       units = "cm",
       dpi = 600)

# .........................................
# .........................................
# get pairwise comps matrix using worth ####
cf = coefficients(mod, log = F)
cf = cf[lvls]
cf
pair_worth = pairwise_probs(cf)
pair_worth
write.csv(pair_worth, file = "output/pairwise-probabilities.csv", row.names = TRUE)

pair_dat = data.frame(player1 = rep(dimnames(pair_worth)[[1]], times = length(lvls)), 
                      player2 = rep(dimnames(pair_worth)[[1]], each = length(lvls)),
                      worth = as.vector(pair_worth))

pair_dat

pair_dat$player1 = factor(pair_dat$player1, levels = lvls)

pair_dat$player2 = factor(pair_dat$player2, levels = rev(lvls))

pair_dat$worth = round(pair_dat$worth, 2)

p = ggplot(pair_dat, 
       aes(x = player2, 
           y = player1,
           fill = worth,
           label = worth)) +
  geom_tile(show.legend = FALSE) + 
  geom_text() +
  scale_fill_gradient2(low = "#b2182b", 
                         high = "#2166ac", 
                         na.value = "white",
                         midpoint = 0) +
  scale_x_discrete(position = "top") +
  theme_bw() +
  theme(axis.text = element_text(color = "grey10"),
        strip.text.x = element_text(color = "grey10"),
        axis.text.x = element_text(angle = 90, hjust = 0),
        panel.grid = element_blank()) +
labs(x = "", 
     y = "",
     fill = "")

p

ggsave("output/pairwise-probabilities.pdf",
       plot = p,
       width = 20,
       height = 20,
       units = "cm",
       dpi = 600)

# .........................................
# .........................................
# PLtree ####
# now try to split by country and gender
hist(dat$farm_income)

boxplot(dat$farm_land)

dat$farm_land[dat$farm_land > 10] = 0

dat$farm_land[is.na(dat$farm_land)] = median(dat$farm_land, na.rm = T)

boxplot(dat$farm_land)

plot(density(dat$farm_land))

pld = data.frame(G, 
                 Gender = as.factor(dat$resp_gender), 
                 FarmLand = dat$farm_land,
                 Country = as.factor(dat$country))

head(pld)

str(pld)

tree = pltree(G ~ Gender + Country + FarmLand, 
              data = pld,
              gamma = TRUE, 
              alpha = 1,
              minsize = 300)

tree

deviance(tree)

ptree = plot(tree,
             ref = ref, 
             multcomp = TRUE,
             levels = lvls)

ptree

ggsave("output/pltree-log-worth-2.png",
       plot = ptree,
       width = 30,
       height = 20,
       units = "cm",
       dpi = 600)

ggsave("output/pltree-log-worth-2.pdf",
       plot = ptree,
       width = 30,
       height = 20,
       units = "cm",
       dpi = 600)

# now the tree showing the worth
ptree2 = plot(tree, log = FALSE, multcomp = TRUE)

ptree2

ggsave("output/pltree-log-worth-winprobability-2.png",
       plot = ptree2,
       width = 30,
       height = 20,
       units = "cm",
       dpi = 600)

ggsave("output/pltree-worth-winprobability-2.pdf",
       plot = ptree2,
       width = 30,
       height = 20,
       units = "cm",
       dpi = 600)


