library(tidygraph)
library(ggraph)
library(corrplot)
library(rstatix)
library(readr)
library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)
library(tibble)
library(readxl)
library(survival) 
library(ggfortify)
library(survminer)
library(rlang)
library(janitor)
library(tidyverse)
library(igraph)
library(cluster)    
library(factoextra) 
library(dendextend)

##"328 vs 303"
graph1_matrix <- as_adjacency_matrix(graph_1, attr = 'new_weight', sparse = FALSE)
graph1_matrix <- dist(graph1_matrix, method = "euclidean")
graph1_clust <- hclust(graph1_matrix, method = "complete")

graph2_matrix <- as_adjacency_matrix(graph_2, attr = "new_weight", sparse = FALSE)
graph2_matrix <- dist(graph2_matrix, method = "euclidean")
graph2_clust <- hclust(graph2_matrix, method = "complete")

graph1_dend <- as.dendrogram(graph1_clust)
graph2_dend <- as.dendrogram(graph2_clust)

png("dend.png", width = 30, height = 40, res = 600, units = "cm")
tanglegram(graph1_dend, graph2_dend)
dev.off()

cor_cophenetic(graph1_dend, graph2_dend) 
cor_coph_1 <- cor_cophenetic(graph1_dend, graph2_dend)

##"328 vs no"
graph1_matrix <- as_adjacency_matrix(graph_1, attr = 'new_weight', sparse = FALSE)
graph1_matrix <- dist(graph1_matrix, method = "euclidean")
graph1_clust <- hclust(graph1_matrix, method = "complete")

graph2_matrix <- as_adjacency_matrix(graph_3, attr = "new_weight", sparse = FALSE)
graph2_matrix <- dist(graph2_matrix, method = "euclidean")
graph2_clust <- hclust(graph2_matrix, method = "complete")

graph1_dend <- as.dendrogram(graph1_clust)
graph2_dend <- as.dendrogram(graph2_clust)

png("dend.png", width = 30, height = 40, res = 600, units = "cm")
tanglegram(graph1_dend, graph2_dend)
dev.off()

cor_cophenetic(graph1_dend, graph2_dend)
cor_coph_2 <- cor_cophenetic(graph1_dend, graph2_dend)

##"303 vs no"
graph1_matrix <- as_adjacency_matrix(graph_2, attr = 'new_weight', sparse = FALSE)
graph1_matrix <- dist(graph1_matrix, method = "euclidean")
graph1_clust <- hclust(graph1_matrix, method = "complete")

graph2_matrix <- as_adjacency_matrix(graph_3, attr = 'new_weight', sparse = FALSE)
graph2_matrix <- dist(graph2_matrix, method = "euclidean")
graph2_clust <- hclust(graph2_matrix, method = "complete")

graph1_dend <- as.dendrogram(graph1_clust)
graph2_dend <- as.dendrogram(graph2_clust)

png("dend.png", width = 30, height = 40, res = 600, units = "cm")
tanglegram(graph1_dend, graph2_dend)
dev.off()

cor_cophenetic(graph1_dend, graph2_dend)
cor_coph_3 <- cor_cophenetic(graph1_dend, graph2_dend)

## таблица 
cor_coph_table <- data.frame(graphs = c("328 vs 303"), cor_cophenetic = cor_coph_1)
new_row <- data.frame(graphs = c("328 vs no"), cor_cophenetic = cor_coph_2)
cor_coph_table <- rbind(cor_coph_table, new_row)
new_row_1 <- data.frame(graphs = c("303 vs no"), cor_cophenetic = cor_coph_3)
cor_coph_table <- rbind(cor_coph_table, new_row_1) 


##производительность 
timing_1c <- system.time({graph1_matrix <- as_adjacency_matrix(graph_1, attr = 'new_weight', sparse = FALSE)
graph1_matrix <- dist(graph1_matrix, method = "euclidean")
graph1_clust <- hclust(graph1_matrix, method = "complete")

graph2_matrix <- as_adjacency_matrix(graph_2, attr = "new_weight", sparse = FALSE)
graph2_matrix <- dist(graph2_matrix, method = "euclidean")
graph2_clust <- hclust(graph2_matrix, method = "complete")

graph1_dend <- as.dendrogram(graph1_clust)
graph2_dend <- as.dendrogram(graph2_clust)

png("dend.png", width = 30, height = 40, res = 600, units = "cm")
tanglegram(graph1_dend, graph2_dend)
dev.off()

cor_cophenetic(graph1_dend, graph2_dend) 
cor_coph_1 <- cor_cophenetic(graph1_dend, graph2_dend)
})

print(timing_1c)

timing_table <- data.frame(method = character(), graphs = character(), performance = numeric())
method_name <- "cor_coph"
graph_name <- "328 vs 303"
performance_time <- timing_1  

##
timing_2c <- system.time({graph1_matrix <- as_adjacency_matrix(graph_1, attr = 'new_weight', sparse = FALSE)
graph1_matrix <- dist(graph1_matrix, method = "euclidean")
graph1_clust <- hclust(graph1_matrix, method = "complete")

graph2_matrix <- as_adjacency_matrix(graph_3, attr = "new_weight", sparse = FALSE)
graph2_matrix <- dist(graph2_matrix, method = "euclidean")
graph2_clust <- hclust(graph2_matrix, method = "complete")

graph1_dend <- as.dendrogram(graph1_clust)
graph2_dend <- as.dendrogram(graph2_clust)

png("dend.png", width = 30, height = 40, res = 600, units = "cm")
tanglegram(graph1_dend, graph2_dend)
dev.off()

cor_cophenetic(graph1_dend, graph2_dend)
cor_coph_2 <- cor_cophenetic(graph1_dend, graph2_dend)
})

print(timing_2c)

##
timing_3c <- system.time({graph1_matrix <- as_adjacency_matrix(graph_2, attr = 'new_weight', sparse = FALSE)
graph1_matrix <- dist(graph1_matrix, method = "euclidean")
graph1_clust <- hclust(graph1_matrix, method = "complete")

graph2_matrix <- as_adjacency_matrix(graph_3, attr = 'new_weight', sparse = FALSE)
graph2_matrix <- dist(graph2_matrix, method = "euclidean")
graph2_clust <- hclust(graph2_matrix, method = "complete")

graph1_dend <- as.dendrogram(graph1_clust)
graph2_dend <- as.dendrogram(graph2_clust)

png("dend.png", width = 30, height = 40, res = 600, units = "cm")
tanglegram(graph1_dend, graph2_dend)
dev.off()

cor_cophenetic(graph1_dend, graph2_dend)
cor_coph_3 <- cor_cophenetic(graph1_dend, graph2_dend)

})

print(timing_3c)

mean_value_time_cor <- mean(c(timing_1c[3], timing_2c[3], timing_3c[3]))