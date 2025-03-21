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
library("climatrends")
library("chirps")
library("nasapower")
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
library("janitor")
source("modules/01_functions.R")

x <- fromJSON("run-local/local/data/edi-global/info.json")

quest <- x$specialfields

cmdata <- x

names(cmdata)

class(cmdata) <- union("CM_list", class(cmdata))
x=cmdata
pivot.wider = TRUE
tidynames = TRUE


cmdata = as.data.frame(cmdata, pivot.wider = TRUE, tidynames = TRUE)


# It is important to have all the data in one single data frame 
# This means that all columns should match
# You need to check the columns names in each data frame in the list 
# Sometimes they don't have the same name due to non standardization of 
# data collection moment
# So try to fix it, using grepl() gsub() or other replacement functions
names(cmdata) <- gsub("registration_", "", names(cmdata))

cmdata$intro_video <- paste0(cmdata$intro_video, " Agrodealer")

#names(cmdata) <- make_clean_names(names(cmdata))

names(cmdata) <- gsub("[(]", "-", names(cmdata))
names(cmdata) <- gsub("[)]", "", names(cmdata))

# ................................
# Make list of parameters ####
# This is the list of traits (characteristics) assessed in the 
# project
quest

# remove duplicates in quest
questions <- quest[!duplicated(quest$desc), ]
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
projname <- tag
outputpath  <- paste0(getwd(), "/run-local/output/", tag)
infosheets  <- FALSE # logical, if infosheets should be written TRUE FALSE
language    <- "en" # the language to write the report
extension   <- "docx" # report file format it can be "docx", "pdf", and "html"
ranker      <- "participant" # how the system will refer to participants/farmers
option      <- "options" # how the system will refer to tested items
fullpath    <- getwd() # this is backward path
reference   <- 1 # the reference item for the analysis
minN        <- 5 # minimum n of complete data required in a trait evaluation before it is excluded
minitem     <- 2 # minimum n of items tested, e.g. that all items are tested at least twice
mincovar    <- 0.90 # minimum proportion of covariates compared to total valid n
sig_level   <- 0.5 # significance level
sig_level_tree   <- 0.5 # significance tree level
minsplit     <- 30 # minimum n in each tree node
groups       <- c("gender")
Option       <- ClimMobTools:::.title_case(option)
options      <- ClimMobTools:::.pluralize(option)
rankers      <- ClimMobTools:::.pluralize(ranker)
nranker      <- nrow(cmdata)
items        <- cmdata[, grepl("package_item", names(cmdata))]
itemnames    <- names(items)
items        <- unique(sort(unlist(items)))
nitems       <- length(unique(sort(unlist(items))))
covar        <- pars$covariates
covarTRUE    <- isTRUE(length(covar) > 0)
ntrait       <- dim(pars$traits)[[1]]
nothertraits <- ntrait - 1
ncomp        <- length(itemnames)
nquest       <- pars$traits$nQst[1]

# select which function will be used to create the Plackett-Luce rankings
# it will depend on how many items each participant compares
if (ncomp == 3) {
  rankwith <- "rankTricot"
}

if (ncomp > 3) {
  rankwith <- "rank_numeric"
}

# minimum proportion of valid entries in tricot vs local
# this will be computed based on the valid entries of the reference
# trait after validations
mintricotVSlocal <- 0.90

# method for adjustments for confidence intervals and setting widths for comparison. 
ci_adjust <- "BH"

# confidence interval level for comparison plots with error bars
ci_level <- 0.84

# resolution of display items
dpi <- 400
out_width <- "100%"

# define height of plots based on items
favplot_h <- nitems * 0.4
agreem_h <- ntrait * 0.6
multcomp_h <- 4
worthmap_h <- ntrait
worthmap_w <- ntrait + 0.5

if (favplot_h < 5) favplot_h <- 5
if (favplot_h > 8) favplot_h <- 7.5
if (agreem_h < 6)  agreem_h <- 6
if (agreem_h > 8)  agreem_h <- 7.5
if (worthmap_h < 7)  worthmap_h <- 7
if (worthmap_h > 8)  worthmap_h <- 8
if (worthmap_w < 7)  worthmap_w <- worthmap_h + 0.5
if (worthmap_w > 8)  worthmap_w <- worthmap_h + 0.5

dir.create(outputpath, showWarnings = FALSE, recursive = TRUE)

# Two objects to begin with that will be used to verify the process
error <- NULL
done <- TRUE

# ................................................................
# ................................................................
# Run analysis ####
source("modules/02_organize_ranking_data.R")

trait_list

source("modules/04_trial_overview.R")

write.csv(itemtable, paste0(outputpath, "/summary_options_tested.csv"), row.names = F)


source("modules/05_spatial_overview.R")

names(cmdata)

cmdata$crop_div <- as.vector(sapply(cmdata$hh_crops, function(x){
  y <- strsplit(x, " ")[[1]]
  length(y)
}))

cmdata$crop_div <- cmdata$crop_div + 
  apply(cmdata[,c("hh_crops_other1", "hh_crops_other2", "hh_crops_other3")], 1, function(x){
  sum(!is.na(x))
})

cmdata$livestock_div <- as.vector(sapply(cmdata$hh_livestock_crops, function(x){
  y <- strsplit(x, " ")[[1]]
  length(y)
}))

cmdata$livestock_div <- cmdata$livestock_div +
  apply(cmdata[,c("hh_livestock_crops_other1",
                  "hh_livestock_crops_other2",
                  "hh_livestock_crops_other3")], 1, function(x){
    sum(!is.na(x))
  })


Fertilizer use
Maize monocropping, intercropping or both
Frequency of maize purchases
End use of harvest

covar <- cmdata[, c("resp_age", "hh_num", "hh_mem_18", "farm_area", "livestock_div", "crop_div",  
                    "resp_gender", "curr_maize_yesno", 
                    "resp_education", "grow_maize")]

unique(covar$grow_maize)
covar$grow_maize[covar$grow_maize=="Monocropping (nothing Else Was Planted Between Maize Rows"] <- "Monocrop"
covar$grow_maize[covar$grow_maize=="Intercropping With Trees Or Other Crops"] <- "Intercrop trees"
covar$grow_maize[covar$grow_maize=="Intercropping With Legumes (beans"] <- "Intercrop leg"
covar$grow_maize[covar$grow_maize=="Both Intercropping And Monocropping"] <- "Mono- Intercrop"


covar[1:6] <- lapply(covar[1:6], as.numeric)
covar[7:10] <- lapply(covar[7:10], as.factor)

covar

boxplot.stats(covar$farm_area)

covar$farm_area[covar$farm_area > 40]  <- NA

names(covar) <- c("Age", "HHSize", "HHSize18", "FarmArea", "LivestockDiv", "CropDiv",
                  "Gender", "MaizeGrower", "Education", "System")


plotquant <- data.frame(value = unlist(covar[1:6]), variable = rep(names(covar)[1:6], each = nrow(covar)))

plotquant$variable <- factor(plotquant$variable, levels = c("Age", "HHSize", "HHSize18", "FarmArea", "LivestockDiv", "CropDiv"))

q <- ggplot(plotquant, aes(x = value, fill = variable, group = variable)) + 
  geom_density(adjust=1.5, alpha=.4, show.legend = F) +
  facet_wrap(~ variable, scales = "free") +
  theme_bw() + 
  labs(x = "Variable", y = "Density") +
  theme(panel.grid = element_blank())

q

ggsave(paste0(outputpath, "/density_plot_quantitative_variables.png"),
       plot = q,
       width = 25,
       height = 20,
       units = "cm",
       dpi = 400)



keepc <- apply(covar, 1, function(x){sum(is.na(x))}) == 0

keep <- trait_list[[1]]$keep & keepc

G <- rankTricot(cmdata[keep, ],
                itemnames,
                trait_list[[1]]$strings,
                group = TRUE)


mod <- PlackettLuce(G)

pld <- cbind(G, covar[keep, ])

tree <- pltree(G ~ ., 
       data = pld, 
       alpha = 0.2, 
       minsize = 30, 
       gamma = TRUE)

# run this for the condensed rankings using predicted nodes
nodes_tree1 <- predict(tree, type = "node")
node_id_tree1 <- sort(unique(nodes_tree1))

length(nodes_tree1) == length(G)

tree1_mod <- list()
nobs_tree1 <- integer()
for (i in seq_along(node_id_tree1)) {
  
  Gi <- G[nodes_tree1 == node_id_tree1[i]]
  
  tree1_mod[[i]] <- PlackettLuce(Gi)
  
  nobs_tree1 <- c(nobs_tree1, length(Gi))
  
}

tree1_branch <- gosset:::build_tree_branches(tree)
tree1_nodes <- gosset:::build_tree_nodes(tree1_mod, 
                                         log = TRUE,
                                         node.ids = node_id_tree1,
                                         n.obs = nobs_tree1)

ptree <- tree1_branch / tree1_nodes + plot_layout(heights =  c(1, 1))

ptree

ggsave(paste0(outputpath, "/pltree.png"),
       plot = ptree,
       width = 50,
       height = 30,
       units = "cm",
       dpi = 400)

wr1 <- regret(tree1_mod, n1 = 1000)

reliable <- reliability(mod, ref = "green maize variety")

reliable <- data.frame(items = names(rev(sort(reliable))),
                       reliability = rev(sort(reliable)))

wr1 <- merge(wr1, reliable, by = "items")
wr1
wr1 <- wr1[rev(order(wr1$worth)), ]

write.csv(wr1, paste0(outputpath, "/regret.csv"), row.names = F)


# plackettluce model
wb <- worth_bar(mod) + theme_classic() + labs(x = "Worth", y = "Option")

ggsave(paste0(outputpath, "/worth_bar.png"),
       plot = wb,
       width = 5,
       height = 5,
       dpi = 400)

reliability(mod, ref = "green maize variety")

logworth <- plot_logworth(mod) + labs(y = "Option")

ggsave(paste0(outputpath, "/log-worth_plot.png"),
       plot = logworth,
       width = 10,
       height = 6,
       dpi = 400)

# now do it for the groups 
mod_g <- list() 
worth_g <- list()
logworth_g <- list()
for (i in seq_along(groups)) {
  gi <- G[cmdata$group == groups[i], ]
  mod_g[[i]] <- PlackettLuce(gi)
  xlab1 <- ""
  xlab2 <- ""
  if(i == 4) {
    xlab1 <- "Worth"
    xlab2 <- "Log-worth"
  }
  
  ylab1 <- ""
  ylab2 <- ""
  if(i == 1) {
    ylab1 <- "Option"
    ylab2 <- "Option"
  }
  
  worth_g[[i]] <- worth_bar(mod_g[[i]]) + theme_classic() + labs(x = xlab1, y = ylab1, title = groups[i]) 
  logworth_g[[i]] <- plot_logworth(mod_g[[i]]) + labs(y = ylab2, x = xlab2, title = groups[i])
}


pl <- logworth_g[[1]] + logworth_g[[2]]

   #+ logworth_g[[3]] + logworth_g[[4]]

ggsave(paste0(outputpath, "/log-worth_plot_by_gender.png"),
       plot = pl,
       width = 40,
       height = 24,
       units = "cm",
       dpi = 400)


pw <- worth_g[[1]] + worth_g[[2]]
  #+ worth_g[[3]] + worth_g[[4]]

pw

ggsave(paste0(outputpath, "/worth_plot_by_gender.png"),
       plot = pw,
       width = 40,
       height = 24,
       units = "cm",
       dpi = 400)
