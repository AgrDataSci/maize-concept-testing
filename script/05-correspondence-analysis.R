# .......................................
# .......................................
# This script runs a text analysis for correspondence analysis
library("tidyverse")
library("magrittr")
library("tidytext")
library("tm")
library("gosset")
library("FactoMineR")
library("factoextra")
library("ggplot2")
library("ggrepel")


dat = read.csv("data/text-data-open-question.csv")

dat["concept"] = lapply(dat["concept"], function(x){
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

dat = dat[dat$word != "mon",]

top = 7

words = 
  dat %>% 
  group_by(concept, preference) %>% 
  count(word) %>% 
  top_n(top, n) %>% 
  arrange(desc(n)) %>% 
  slice(1:top) %>% 
  ungroup()

words

words = words[, -4]

words = split(words, words$concept)

words = lapply(words, function(x){
  k = duplicated(x$word)
  x$word[k] = ""
  x
})

words = rowbind(words)

words = split(words, words$preference)

lapply(words, nrow)

df_mca = data.frame(Concept = words[[1]]$concept)

for(i in seq_along(words)) {
  x = words[[i]]
  x = data.frame(word = x$word)
  names(x) = gsub(" ", "", words[[i]]$preference[1])
  df_mca = cbind(df_mca, x)
}


#k = df_mca$Concept == "Preferred"

#df_mca = df_mca[k, ]


# .......................................
# .......................................
# .......................................
# MCA ####
# define categories
cats = apply(df_mca, 2, function(x) nlevels(as.factor(x)))

df_mca = MCA(df_mca, graph = FALSE, ncp = 8)

df_mca$eig

summary.MCA(df_mca)

# take how much dim1 and dim2 explains
eig = df_mca$eig
dim1 = round(eig[1,3], 1)
dim2 = round(eig[2,3] - eig[1,3], 1)

# get variables
mca_var_df = data.frame(df_mca$var$coord, 
                         Variable=rep(names(cats), cats))

# data frame with observation coordinates
mca_obs_df = data.frame(df_mca$ind$coord)

#get MCA var 
var = get_mca_var(df_mca)

fviz_mca_var(df_mca)

mca_var_df$Concept = row.names(mca_var_df)

mca_var_df$Concept = gsub("Preferred_|LessPreferred_", "", mca_var_df$Concept)
                             

# MCA plot of observations and categories
ggplot(data = mca_obs_df, 
       aes(x = Dim.1, y = Dim.2)) +
  geom_hline(yintercept = 0, colour = "gray70") +
  geom_vline(xintercept = 0, colour = "gray70") +
  #geom_point(colour = "gray50", alpha = 0.7) +
  geom_density2d(colour = "gray80") +
  ylim(-2,2) +
  xlim(-2,2) +
  geom_text(data = mca_var_df, 
                  mapping = aes(x = Dim.1, y = Dim.2, 
                                label = Concept, colour = Variable),
                  size = 4, vjust = 1, hjust = 1,
                  show.legend = FALSE) +
  geom_point(data=mca_var_df, aes(x = Dim.1, y = Dim.2,
                                  shape=factor(Variable), 
                                  fill=factor(Variable)), 
             size=2) +
  labs(x = "Dim 1", 
       y = "Dim 2") + 
  scale_shape_manual(values=c(21,22,23)) +
  scale_fill_manual(values=c('#ff7f00','#e41a1c','#377eb8')) +
  scale_colour_manual(values=c('#ff7f00','#e41a1c','#377eb8')) +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "white"),
        legend.title= element_blank(),
        legend.position = "bottom",
        text = element_text(size = 18),
        legend.text= element_text(size=14, colour="black"),
        axis.title.x = element_text(size=14),
        axis.title.y = element_text(size=14),
        axis.text.x = element_text(size = 12, angle = 0, 
                                   hjust = 1, vjust = 1,
                                   face = "plain"),
        axis.text.y = element_text(size = 12, angle = 0, 
                                   hjust = 1, vjust = 1, 
                                   face = "plain")) 

ggsave("output/correspondence-analysis.pdf",
       plot = last_plot(),
       width = 28,
       height = 28,
       dpi = 600,
       units = "cm")




