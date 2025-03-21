# .......................................
# .......................................
# This script runs a text analysis to produce the 
# co-occurrence network
library("tidyverse")
library("magrittr")
library("tidytext")
library("igraph")
library("ggraph")
library("patchwork")


dat = read.csv("data/text-data-open-question-full-statement.csv")

#k = dat$gender == "Male"
k = TRUE

dat = dat[k, c("preference", "concept", "word")]

# .......................................
# .......................................
# Co-occurrence analysis ####
dbigram = 
  dat %>% 
  unnest_tokens(bigram, word, 
                token = "ngrams", n = 2)

head(dbigram)

dbigram %<>%
  separate(bigram, c("word1", "word2"), sep = " ")

head(dbigram)

dbigram %<>% 
  filter(!word1 %in% stop_words$word) %>% 
  filter(!word2 %in% stop_words$word)

dbigram = na.omit(dbigram)

dbigram %<>% 
  group_by(preference) %>% 
  count(word1, word2, sort = TRUE) %>% 
  filter(n > 7) 

dbigram

dbigram = dbigram[,c("word1", "word2", "preference")]

likegraph = graph_from_data_frame(dbigram)

set.seed(1659)
g = ggraph(likegraph, layout = "fr") +
  geom_edge_link(aes(colour = preference)) +
  geom_node_point() +
  geom_node_text(aes(label = name),
                 vjust = 1, 
                 hjust = 1,
                 check_overlap = TRUE) +
  theme(legend.position = "top",
        legend.title = element_blank(),
        panel.background = element_blank(),
        text = element_text(size = 20))


g

ggsave("output/co-occurrence-network.pdf",
       plot = g,
       width = 30,
       height = 25,
       units = 'cm')
