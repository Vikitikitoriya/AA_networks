### загрузила таблицы ###
path <- "C:/Users/Данил/Documents/Downloads/AA_networks/NaPi2b_AA"
str_data <- list.files(path = file.path(path, "str_data"), full.names = TRUE) 

interactions_data <- list.files(path = file.path(path, "aminoacids_interactions"), full.names = TRUE)

str_data_1 <- read_csv(str_data[1])
#str_data_1$new_column <- paste(str_data_1$resno, str_data_1$resid, sep = "_")
str_data_2 <- read_csv(str_data[2])
#str_data_2$new_column <- paste(str_data_2$resno, str_data_2$resid, sep = "_")
str_data_3 <- read_csv(str_data[3])
#str_data_3$new_column <- paste(str_data_3$resno, str_data_3$resid, sep = "_")

interactions_data_1 <- read_csv(interactions_data[1]) %>%
  filter(persent_intractions > 0)
interactions_data_2 <- read_csv(interactions_data[2]) %>%
  filter(persent_intractions > 0)
interactions_data_3 <- read_csv(interactions_data[3]) %>%
  filter(persent_intractions > 0)

#### тренировалась строить графы 

graph_1 <- tbl_graph(nodes = str_data_1, edges = interactions_data_1, directed = FALSE)

graph_1_1 <- ggraph(graph_1) +
  geom_edge_link(aes(colour = persent_intractions)) +
  geom_node_point() +
  geom_node_text(aes(label = new_column), repel = TRUE) +
  labs(edge_width = "str_data_1") +
  theme_graph()

graph_2 <- tbl_graph(nodes = str_data_2, edges = interactions_data_2, directed = FALSE)

graph_2_1 <- ggraph(graph_2) +
  geom_edge_link(aes(colour = persent_intractions)) +
  geom_node_point() +
  geom_node_text(aes(label = new_column), repel = TRUE) +
  labs(edge_width = "str_data_2") +
  theme_graph()

graph_3 <- tbl_graph(nodes = str_data_3, edges = interactions_data_3, directed = FALSE)

graph_3_1 <- ggraph(graph_3) +
  geom_edge_link(aes(colour = persent_intractions)) +
  geom_node_point() +
  geom_node_text(aes(label = new_column), repel = TRUE) +
  labs(edge_width = "str_data_3") +
  theme_graph()

################################ метод 2
sg1 <- bootnet(graph_2_1, function(i, ...) range(threshold(i)), R = 1000)
sg2 <- bootnet(graph_3_1, function(i, ...) range(threshold(i)), R = 1000)

# Retrieving the distributions
sg1 <- sg1$boot$t
sg2 <- sg2$boot$t

# Define the statistic
sdiff <- sg1 - sg2


diff_ci <- quantile(sdiff, probs = c(0.025, .975))
################## метод3 
jaccard_edgeset_similarity <- function(networks_2, networks_3) {
  inter <- length(E(networks_2 %s% networks_3))
  un <- length(E(networks_2 %u% networks_3))
  
  if (un == 0) {
    0
  } else {
    inter/un
  }
}


# test
jaccard_edgeset_similarity(networks_2, networks_3)


##### СОЗДАНИЕ ГРАФОВ  по параметрам, параметры взяла рандомные ############


parameters <- c('control', 'ramachadran', 'hbonds', 'RMSF', 'conservative', 'charge', 'hydrophobicity')

  graph_list <- list()
  
  for (i in 1:(length(parameters))) {
    
    if (i == 1) {
      
      nodes_local <- str_data_1 %>%
        dplyr::select(1:7, 11)
      
      data.net <- tbl_graph(
        nodes = nodes_local, 
        edges = interactions_data_1,
        directed = F
      )
      
    } else {
      
      nodes_local <- str_data_1 %>%
        dplyr::select(1:7, 11, all_of(parameters[i]))
      
      data.net <- tbl_graph(
        nodes = nodes_local, 
        edges = interactions_data_1,
        directed = F
      )
      
    }
    
    graph_list[[i]] <- data.net
    
  }
  
  names(graph_list) <- parameters
  
  assign(x = "graph_list", value = graph_list, envir = .GlobalEnv)
  
  graph_1 <- graph_list[1]
  graph_2 <- graph_list[2]
  
  networks_1 <- ggraph(graph_1) +
    geom_edge_link(aes(colour = persent_intractions)) +
    geom_node_point() +
    geom_node_text(aes(label = new_column), repel = TRUE) +
    labs(edge_width = "str_data_1") +
    theme_graph()

  ########################################################
  # 2nd function -  Получение параметров и визуализация #
  ########################################################
  
  # graph parameters
  parameters <- names(graph_list)
  
  graph_parameters <- data.frame(graphs = parameters)
  graph_parameters$diameter <- NA
  graph_parameters$mean_distance <- NA
  
  parameters_list <- list()
  graph_graphs <- list()
  
  for (i in 1:length(parameters)) {
    
    if (i == 1) {
      
      graph_parameter_n <- graph_list[[i]] %>%
        dplyr::mutate(diameter = graph_diameter(directed = F),
                      mean_distance = graph_mean_dist(directed = F)) %>%
        as_tibble() %>%
        dplyr::select(diameter, mean_distance) %>%
        distinct() %>%
        unlist()
      
      graph_parameters[i, 2:3] <- graph_parameter_n
      
      node_parameters <- graph_list[[i]] %>%
        activate(nodes) %>%
        mutate(betweenness = centrality_betweenness(),
               closeness = centrality_closeness(),
               hub = centrality_hub(),
               degree = centrality_degree()) %>%
        as.tibble()
      
      graph_plot_c_b_h <- graph_list[[i]] %>%
        activate(nodes) %>%
        left_join(node_parameters) %>% 
        ggraph(layout = "kk") + 
        geom_edge_link(color = "grey") +
        geom_node_point(aes(size = closeness, colour = betweenness, alpha = hub)) +
        geom_node_text(aes(label = label, size = hub), repel = T) +
        scale_color_gradient(low = "yellow", high = "red") +
        labs(title = str_c(c(parameters[i]), ". Diameter=", graph_parameter_n[1], ", Mean distance=", graph_parameter_n[2], "."), collapse = "") +
  theme_graph() 

nodes_xy <- graph_list[[i]] %>%
  activate(nodes) %>%
  as.tibble() %>%
  select(x,y) 
names(nodes_xy) <- c("x1", "y1")

graph_plot_c_b_h_2 <- graph_list[[i]] %>%
  activate(nodes) %>%
  left_join(node_parameters) %>% 
  ggraph(layout = nodes_xy) + 
  geom_edge_link(color = "grey") +
  geom_node_point(aes(size = closeness, colour = betweenness, alpha = hub)) +
  geom_node_text(aes(label = label, size = hub), repel = T) +
  scale_color_gradient(low = "yellow", high = "red") +
  labs(title = str_c(c(parameters[i]), ". Diameter=", graph_parameter_n[1], ", Mean distance=", graph_parameter_n[2], "."), collapse = "") +
  theme_graph() 

graph_graphs[[i*2-1]] <- graph_plot_c_b_h
graph_graphs[[i*2]] <- graph_plot_c_b_h_2

# ggsave(filename = str_c(firstup(parameters[i]), ".png"), plot = graph_plot_c_b_h, scale = 3)

    } else {
      
      node_info <- graph_list[[i]] %>%
        activate(nodes) %>%
        as_tibble() %>%
        select(1, 9)
      
      node_pre_parameters <- graph_list[[i]] %>%
        activate(edges) %>%
        left_join(node_info, by = c("from" = "id")) %>%
        left_join(node_info, by = c("to" = "id"), suffix = c("_from", "_to")) %>%
        mutate(new_weight = !!sym(str_c(parameters[i], "_from")) + !!sym(str_c(parameters[i], "_to"))) %>%
        mutate(new_weight = 2**((new_weight - min(new_weight) / diff(range(new_weight)))) * weight)
      
      graph_parameter_n <- node_pre_parameters %>%
        dplyr::mutate(diameter = graph_diameter(weights = new_weight, directed = F),
                      mean_distance = graph_mean_dist(directed = F)) %>%
        as.tibble() %>%
        dplyr::select(diameter, mean_distance) %>%
        distinct() %>%
        unlist()
      
      graph_parameters[i, 2:3] <- graph_parameter_n
      
      node_parameters <- node_pre_parameters %>%
        activate(nodes) %>%
        mutate(betweenness = centrality_betweenness(weights = new_weight),
               closeness = centrality_closeness(weights = new_weight),
               hub = centrality_hub(weights = new_weight),
               degree = centrality_degree(weights = new_weight)) %>%
        as.tibble()
      
      graph_plot_c_b_h <- node_pre_parameters %>%
        activate(nodes) %>%
        left_join(node_parameters) %>% 
        ggraph(layout = "nicely") + 
        geom_edge_link(aes(alpha = new_weight), color = "grey") +
        geom_node_point(aes(size = closeness, colour = betweenness, alpha = hub)) +
        geom_node_text(aes(label = label, size = hub), repel = T) +
        scale_color_gradient(low = "yellow", high = "red") +
        labs(title = str_c(c(firstup(parameters[i]), ". Diameter=", graph_parameter_n[1], ", Mean distance=", graph_parameter_n[2], "."), collapse = "")) +
        theme_graph()
      
      nodes_xy <- graph_list[[i]] %>%
        activate(nodes) %>%
        as.tibble() %>%
        select(x,y) 
      names(nodes_xy) <- c("x1", "y1")
      
      graph_plot_c_b_h_2 <- graph_list[[i]] %>%
        activate(nodes) %>%
        left_join(node_parameters) %>% 
        ggraph(layout = nodes_xy) + 
        geom_edge_link(color = "grey") +
        geom_node_point(aes(size = closeness, colour = betweenness, alpha = hub)) +
        geom_node_text(aes(label = label, size = hub), repel = T) +
        scale_color_gradient(low = "yellow", high = "red") +
        labs(title = str_c(c(firstup(parameters[i]), ". Diameter=", graph_parameter_n[1], ", Mean distance=", graph_parameter_n[2], "."), collapse = "")) +
        theme_graph() 
      
      graph_graphs[[i*2-1]] <- graph_plot_c_b_h
      graph_graphs[[i*2]] <- graph_plot_c_b_h_2     
      
      # ggsave(filename = "graph_plot_try.png", plot = graph_plot_c_b_h, scale = 3)
      
    }
    
    parameters_list[[i]] <- node_parameters
    
  }
  
  assign(x = "graph_parameters", value = graph_parameters, envir = .GlobalEnv)
  
  names(parameters_list) <- parameters
  assign(x = "parameter_list", value = parameters_list, envir = .GlobalEnv)
  
  names(graph_graphs) <- str_c(rep(parameters, each=2), c("_kk", "_xy"))
  assign(x = "plot_list", value = graph_graphs, envir = .GlobalEnv)
  
  #############################################
  # 3rd function - Сравнение параметров нодов #
  #############################################
  
  node_parameters_names <- c("betweenness","closeness","hub", "degree")
  
    
    comparison_node_parameters <- list()
    
    for (i in 1:length(node_parameters_names)) {
      
      parameter_table <- data.frame(matrix(nrow = nrow(parameter_list[[i]]),ncol = length(parameters)))
      names(parameter_table) <- parameters
      
      for (j in 1:length(parameters)) {
        node_parameter_j <- parameter_list[[j]] %>%
          select(node_parameters_names[i]) %>%
          unlist()
        parameter_table[,j] <- node_parameter_j
      }
      
      parameter_table_pre_plot <- parameter_table %>%
        pivot_longer(everything(), names_to = "parameter", values_to = "node_parameter")
      
      parameter_table_pre_plot$parameter <- factor(parameter_table_pre_plot$parameter, levels = names(parameter_list))
      
      kruskal <- parameter_table_pre_plot %>%
        kruskal_test(node_parameter ~ parameter)
      
      comparison_node_parameters[[i*3-2]] <- kruskal
      
      pwc <- parameter_table_pre_plot %>%
        pairwise_wilcox_test(node_parameter ~ parameter) %>%
        add_xy_position(x = "parameter") %>%
        filter(p.adj.signif != "ns") %>%
        filter(group1 == "default") # ONLY COMPARISON WITH DEFAULT GRAPH (NO WEIGHT)
      
      comparison_node_parameters[[i*3-1]] <- pwc
      
      boxplot <-
        ggplot(
          parameter_table_pre_plot,
          aes(
            x = parameter,
            y = node_parameter
          )
        ) +
        geom_boxplot(width = .12,
                     ## remove outliers
                     # outlier.color = NA,
                     alpha = 0.5) +
        coord_flip() +
        stat_pvalue_manual(pwc, hide.ns = F, coord.flip = T, step.increase = 0.1, ) +
        labs(
          title = firstup(node_parameters_names[i]),
          subtitle = get_test_label(kruskal, detailed = TRUE),
          caption = get_pwc_label(pwc),
          x = "",
          y = ""
        ) +
        theme_pubr() +
        scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                      labels = trans_format("log10", math_format(10^.x))) +
        annotation_logticks(sides="b")
      
      comparison_node_parameters[[i*3]] <- boxplot
      
    }
    
    names(comparison_node_parameters) <- str_c(rep(node_parameters_names, each=3), c("_kruskal", "_wilcox_pwc", "_rainplot"))
    assign(x = "comparison_node_parameters", value = comparison_node_parameters, envir = .GlobalEnv)
    
  
  
  ######################################
  # 4th function - Составление формулы #
  ######################################
  
  coef_table <- data.frame(p = c("*", "**", "***", "****"), coef_part = c(0.125,0.25,0.5,1))
  

    
    wilcox_pwc_list <- comparison_node_parameters[str_detect(names(comparison_node_parameters),"wilcox")]
    
    wilcox_join_table <- wilcox_pwc_list %>%
      bind_rows(.id = "node_parameter") %>%
      select(node_parameter, group1, group2, p.adj, p.adj.signif)
    
    formula_coef <- wilcox_join_table %>%
      left_join(coef_table, by = c("p.adj.signif" = "p")) %>%
      group_by(group2) %>%
      summarise(coef = sum(coef_part)) %>%
      column_to_rownames("group2")
    
    assign(x = "formula_coef", value = formula_coef, envir = .GlobalEnv)
    
  
  
  ##############################################
  # 5th function - Построение графа с формулой #
  ##############################################
  

    graph_list <- list()
    
    graph <- tbl_graph(
      nodes = nodes, 
      edges = edges,
      directed = F
    )
    
    node_info <- graph %>%
      activate(nodes) %>%
      as_tibble() %>%
      mutate(coef = conservative * formula_coef["conservative", 1] +
               # charge * formula_coef["charge", 1] +
               hbonds * formula_coef["hbonds", 1] + 
               # hydrophobicity * formula_coef["hydrophobicity", 1] +
               ramachadran * formula_coef["ramachadran", 1] + 
               RMSF * formula_coef["RMSF", 1]) %>%
      mutate(coef = scale(coef, center = F)) %>%
      select(1, last_col())
    
    node_pre_parameters <- graph %>%
      activate(edges) %>%
      left_join(node_info, by = c("from" = "id")) %>%
      left_join(node_info, by = c("to" = "id"), suffix = c("_from", "_to")) %>%
      mutate(new_weight = scale(coef_from + coef_to, center = F) * weight * five_aa)
    
    node_parameters <- node_pre_parameters %>%
      activate(nodes) %>%
      left_join(node_info, by = "id") %>%
      mutate(betweenness = centrality_betweenness(weights = new_weight),
             hub = centrality_hub(weights = new_weight),
             degree = centrality_degree(weights = new_weight)) %>%
      as_tibble()
    
    graph_pre_plot <- node_pre_parameters %>%
      activate(nodes) %>%
      left_join(node_parameters)
    
    graph_list[[1]] <- graph_pre_plot
    
    graph_plot_c_b_h <- graph_pre_plot %>%
      ggraph(layout = "graphopt") + 
      geom_edge_link(color = "grey") +
      geom_node_point(aes(colour = betweenness, size = hub, alpha = degree)) +
      geom_node_text(aes(label = label, size = hub), repel = T) +
      scale_color_gradient(low = "yellow", high = "red") +
      scale_alpha_continuous(range = c(0.5,1)) +
      scale_size_continuous(trans = "log10") +
      theme_graph()
    
    # ggsave(filename = "graph_plot_c_b_h.png", plot = graph_plot_c_b_h, scale = 4)
    graph_list[[2]] <- graph_plot_c_b_h
    
    nodes_xy <- graph %>%
      activate(nodes) %>%
      as.tibble() %>%
      select(x,y) 
    names(nodes_xy) <- c("x1", "y1")
    
    graph_plot_c_b_h_2 <- graph_pre_plot %>%
      ggraph(layout = nodes_xy) + 
      geom_edge_link(color = "grey") +
      geom_node_point(aes(colour = betweenness, size = hub, alpha = degree)) +
      geom_node_text(aes(label = label, size = hub), repel = T) +
      scale_color_gradient(low = "yellow", high = "red") +
      scale_alpha_continuous(range = c(0.5,1)) +
      scale_size_continuous(trans = "log10") +
      theme_graph() 
    
    # ggsave(filename = "graph_plot_c_b_h_2.png", plot = graph_plot_c_b_h_2, scale = 4)
    graph_list[[3]] <- graph_plot_c_b_h_2
    
    names(graph_list) <- c("graph", "plot_layout", "plot_coord")
    
    assign(x = str_c("graph_list_w-plots", name, sep = "_"), value = graph_list, envir = .GlobalEnv)
    

  