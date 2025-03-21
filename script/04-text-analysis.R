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
library("wordcloud")
library("igraph")
library("ggraph")
library("patchwork")
#library("SnowballC")

dat = read.csv("data/text-data-open-question-full-statement.csv")

# unnest the words 
dat = 
  dat %>% 
  group_by(preference, concept) %>% 
  unnest_tokens("word", word) 

print(sort(unique(dat$word)))

dat = as.data.frame(dat)

dat

# .......................................
# .......................................
# WordCloud ####
items = sort(unique(dat$concept))

preference = unique(dat$preference)

# run over items to create one cloud per tech option
for (i in seq_along(items)) {

  for (j in seq_along(preference)) {
    
    k = dat$concept == items[i] & dat$preference == preference[j]
    
    ll = as.vector(dat[k, "word"])
    
    corpus = stripWhitespace(ll)
    corpus = iconv(corpus)
    corpus = Corpus(VectorSource(corpus))
    corpus = tm_map(corpus, removeNumbers)
    corpus = tm_map(corpus, removeWords, stopwords("english"))
    tdm = TermDocumentMatrix(corpus)
    tdm = as.matrix(tdm)
    
    w = sort(rowSums(tdm), decreasing = TRUE)
    set.seed(2233)
    pdf(file = paste0("output/wordcloud-", 
                      tolower(preference[j]),
                      "-",
                      gsub(" ", "-", tolower(items[i])),
                      ".pdf"),
        width = 5,
        height = 5)
    wordcloud(words = names(w),
              freq = w,
              max.words = 100,
              random.order = FALSE,
              min.freq = 5,
              colors = brewer.pal(8, 'Dark2'),
              scale = c(5, 0.3),
              rot.per = 0.7)
    dev.off()
    
  }
  
}

# now using all the answers for all the items
# run over items to create one cloud per tech option
for (j in seq_along(preference)) {
  
  k = dat$preference == preference[j]
  
  ll = dat[k, "word"]
  
  corpus = stripWhitespace(ll)
  corpus = iconv(corpus)
  corpus = Corpus(VectorSource(corpus))
  corpus = tm_map(corpus, removeNumbers)
  corpus = tm_map(corpus, removeWords, stopwords("english"))
  tdm = TermDocumentMatrix(corpus)
  tdm = as.matrix(tdm)
  
  w = sort(rowSums(tdm), decreasing = TRUE)
  set.seed(2233)
  pdf(file = paste0("output/wordcloud-", 
                        tolower(preference[j]),
                        ".pdf"),
      width = 5,
      height = 5)
  wordcloud(words = names(w),
            freq = w,
            max.words = 100,
            random.order = FALSE,
            min.freq = 5,
            colors = brewer.pal(8, 'Dark2'),
            scale = c(5, 0.3),
            rot.per = 0.7)
  dev.off()
  
}

# .......................................
# .......................................
# Term Frequency ####
dat_idf1 = dat %>% 
  filter(preference == "Preferred") %>% 
  count(word, concept) %>% 
  bind_tf_idf(word, concept, n) %>% 
  arrange(desc(tf_idf)) %>% 
  mutate(preference = "Preferred")

dat_idf2 = dat %>% 
  filter(preference == "LessPreferred") %>% 
  count(word, concept) %>% 
  bind_tf_idf(word, concept, n) %>% 
  arrange(desc(tf_idf)) %>% 
  mutate(preference = "LessPreferred")

top = 10

p1 = dat_idf1 %>%
  group_by(concept) %>% 
  top_n(top, tf_idf) %>%
  arrange(desc(tf_idf)) %>% 
  slice(1:top) %>% 
  ungroup() %>% 
  mutate(word = reorder(word, tf_idf)) %>% 
  ggplot(aes(y = word, x = tf_idf, fill = concept)) +  
  geom_col(show.legend = FALSE) + 
  facet_wrap(concept ~ ., 
             scales = "free", 
             nrow = 2) +
  theme_classic() +
  scale_fill_brewer(palette = "Dark2") +
  labs(y = "Term", 
       x = "",
       title = "Preferred") +
  theme(panel.grid.major = element_blank(),
        panel.background = element_blank(),
        strip.background = element_rect(fill="white"),
        text = element_text(color = "grey20"),
        legend.title = element_blank(),
        strip.background.x = element_blank(),
        strip.placement = "outside")

p1

p2 = dat_idf2 %>%
  group_by(concept) %>% 
  top_n(top, tf_idf) %>%
  arrange(desc(tf_idf)) %>% 
  slice(1:top) %>% 
  ungroup() %>% 
  mutate(word = reorder(word, tf_idf)) %>% 
  ggplot(aes(y = word, x = tf_idf, fill = concept)) +  
  geom_col(show.legend = FALSE) + 
  facet_wrap(concept ~ ., 
             scales = "free", 
             nrow = 2) +
  theme_classic() +
  scale_fill_brewer(palette = "Dark2") +
  labs(y = "", 
       x = "Term Frequency - Inverse Document Frequency (TF-IDF)",
       title = "Less Preferred") +
  theme(panel.grid.major = element_blank(),
        panel.background = element_blank(),
        strip.background = element_rect(fill="white"),
        text = element_text(color = "grey20"),
        legend.title = element_blank(),
        strip.background.x = element_blank(),
        strip.placement = "outside")

p2

p = p1 / p2 + plot_annotation(tag_levels = 'A')

ggsave("output/term-frequency-idf.pdf",
       width = 30,
       height = 35,
       units = "cm")

