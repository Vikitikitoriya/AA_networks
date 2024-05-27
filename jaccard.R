
##328 vs 303
jaccard_edgeset_similarity <- function(graph_1, graph_2) {
  inter <- length(E(graph_1 %s% graph_2)) #%s% для пересечения двух графов 
  un <- length(E(graph_1 %u% graph_2)) #%u% для объединения двух графов 
  
  if (un == 0) {
    0
  } else {
    inter/un
  }
}


# test
jaccard_coefficient_1 <- jaccard_edgeset_similarity(graph_1, graph_2)

##328 vs no
jaccard_edgeset_similarity <- function(graph_1, graph_3) {
  inter <- length(E(graph_1 %s% graph_3)) #%s% для пересечения двух графов 
  un <- length(E(graph_1 %u% graph_3)) #%u% для объединения двух графов 
  
  if (un == 0) {
    0
  } else {
    inter/un
  }
}
jaccard_coefficient_2 <- jaccard_edgeset_similarity(graph_1, graph_3)

## 303 vs no
jaccard_edgeset_similarity <- function(graph_2, graph_3) {
  inter <- length(E(graph_2 %s% graph_3)) #%s% для пересечения двух графов 
  un <- length(E(graph_2 %u% graph_3)) #%u% для объединения двух графов 
  
  if (un == 0) {
    0
  } else {
    inter/un
  }
}
jaccard_coefficient_3 <- jaccard_edgeset_similarity(graph_2, graph_3)

## таблица
jaccard_test_table <- data.frame(graphs = c("328 vs 303"), jaccard = jaccard_coefficient_1)
new_row <- data.frame(graphs = c("328 vs no"), jaccard = jaccard_coefficient_2)
jaccard_test_table <- rbind(jaccard_test_table, new_row)
new_row_1 <- data.frame(graphs = c("303 vs no"), jaccard = jaccard_coefficient_3)
jaccard_test_table <- rbind(jaccard_test_table, new_row_1)


## производительность 
##
timing_1j <- system.time({jaccard_edgeset_similarity <- function(graph_1, graph_2) {
  inter <- length(E(graph_1 %s% graph_2)) #%s% для пересечения двух графов 
  un <- length(E(graph_1 %u% graph_2)) #%u% для объединения двух графов 
  
  if (un == 0) {
    0
  } else {
    inter/un
  }
}


# test
jaccard_coefficient_1 <- jaccard_edgeset_similarity(graph_1, graph_2)

})

print(timing_1j)

##
timing_2j <- system.time({jaccard_edgeset_similarity <- function(graph_1, graph_3) {
  inter <- length(E(graph_1 %s% graph_3)) #%s% для пересечения двух графов 
  un <- length(E(graph_1 %u% graph_3)) #%u% для объединения двух графов 
  
  if (un == 0) {
    0
  } else {
    inter/un
  }
}


# test
jaccard_coefficient_2 <- jaccard_edgeset_similarity(graph_1, graph_3)

})

print(timing_2j)

timing_3j <- system.time({jaccard_edgeset_similarity <- function(graph_2, graph_3) {
  inter <- length(E(graph_2 %s% graph_3)) #%s% для пересечения двух графов 
  un <- length(E(graph_2 %u% graph_3)) #%u% для объединения двух графов 
  
  if (un == 0) {
    0
  } else {
    inter/un
  }
}


# test
jaccard_coefficient_3 <- jaccard_edgeset_similarity(graph_2, graph_3)

})

print(timing_3j)

mean_value_time_jac <- mean(c(timing_1j[3], timing_2j[3], timing_3j[3]))