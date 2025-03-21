### simulation
require(tidyverse)

n <- 1000
steps <- seq(from=550, to = 50, by = -50)
r <- matrix(NA, nrow=length(steps), ncol=4)

for(i in 1:length(steps)){
  
  r_j <- vector(length=n)
  
  for(j in 1:n){
    
    ss_pld <- pld %>%
      group_by(Gender, Country) %>%
      sample_n(steps[i])
    
    tree_ij <- pltree(G ~ Country + Gender, data = ss_pld, alpha = 0.1)
    r_j[j] <- width(tree_ij)
    
  }
  
  r[i,] <- c(mean(r_j), quantile(r_j, probs=c(0.95,0.5,0.05)))
  
}

r <- cbind(steps, r)
colnames(r) <- c("n", "mean", ".95 quantile", "median", "0.05 quantile")