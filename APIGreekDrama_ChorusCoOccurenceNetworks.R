# Greek Drama Chorus Network

#  0: libraries & set up -----------------------------------------------------------

library(rdracor)
library(igraph)
library(tidyverse)
library(tidygraph)
library(ggraph)

# create folder for plots
dir.create("plots", showWarnings = FALSE)

#  1: help functions -----------------------------------------------------------

# 1.1 Prepare graph
# make sure network is undirected for centrality metrics

prepare_graph <- function(g){
  
  if(is_directed(g)){
    g <- as.undirected(g, mode = "collapse")
  }
  
  return(g)
  
}

# 1.2 detect chorus nodes
# returns indices of all nodes containing "Χορός"

detect_chorus <- function(g){
  
  chorus_idx <- which(
    grepl("Χορός", V(g)$name, ignore.case = TRUE)
  )
  
  return(chorus_idx)
  
}


#  2: check play for chorus -----------------------------------------------

check_chorus_presence <- function(play_id){
  
  g <- get_net_cooccur_igraph(play_id, corpus="greek")
  
  chorus_nodes <- detect_chorus(g)
  
  tibble(
    
    play = play_id,
    
    chorus_present = length(chorus_nodes) > 0,
    
    n_chorus_nodes = length(chorus_nodes),
    
    chorus_names = paste(
      V(g)$name[chorus_nodes],
      collapse="; "
    )
    
  )
  
}


#  3: analyze play --------------------------------------------------------
# compute network metrics (size, density, centrality measures, chorus metrics)

analyze_play <- function(play_id){
  
  message("Analyzing ", play_id)
  
  g <- get_net_cooccur_igraph(play_id, corpus="greek")
  
  g <- prepare_graph(g)
  
  chorus_nodes <- detect_chorus(g)
  
  # network size
  n_chars <- vcount(g)
  n_edges <- ecount(g)
  
  # network density
  density <- edge_density(g)
  
  # centrality measures
  deg <- degree(g)
  bet <- betweenness(g)
  eig <- eigen_centrality(g)$vector
  close <- closeness(g)
  
  # chorus metrics
  if(length(chorus_nodes) > 0){
    
    chorus_degree <- mean(deg[chorus_nodes])
    chorus_betweenness <- mean(bet[chorus_nodes])
    chorus_eigen <- mean(eig[chorus_nodes])
    chorus_closeness <- mean(close[chorus_nodes])
    
  } else {
    
    chorus_degree <- NA
    chorus_betweenness <- NA
    chorus_eigen <- NA
    chorus_closeness <- NA
    
  }
  
  tibble(
    
    play = play_id,
    
    n_characters = n_chars,
    n_edges = n_edges,
    density = density,
    
    chorus_degree = chorus_degree,
    chorus_betweenness = chorus_betweenness,
    chorus_eigenvector = chorus_eigen,
    chorus_closeness = chorus_closeness
    
  )
  
}


#  4: prepare GreekDraCor ------------------------------------------------

plays <- get_dracor("greek")

# check chorus presence across corpus

chorus_check <- map_dfr(
  plays$name,
  check_chorus_presence
)
print(chorus_check)

#warning if no chorus
no_chorus <- chorus_check %>%
  filter(!chorus_present)

if(nrow(no_chorus) > 0){
  warning("Plays without chorus detected:", no_chorus$play)
}

# remove plays without chorus nodes
plays_with_chorus <- chorus_check %>%
  filter(chorus_present) %>%
  pull(play)

plays <- plays %>%
  filter(name %in% plays_with_chorus)

#  5: corpus analysis -----------------------------------------------------

results <- lapply(
  
  plays$name,
  analyze_play
  
) %>%
  
  bind_rows()


#  6: merge metadata ------------------------------------------------------

results <- results %>%
  
  left_join(
    
    plays %>%
      select(playName, firstAuthorName, yearNormalized),
    
    by = c("play"="playName")
    
  ) %>%
  
  left_join(
    chorus_check,
    by="play"
  )


#  7: compute chorus brokerage score --------------------------------------

compute_chorus_brokerage <- function(play_id){
  
  g <- get_net_cooccur_igraph(play_id, corpus="greek")
  
  g <- prepare_graph(g)
  
  chorus_nodes <- detect_chorus(g)
  
  if(length(chorus_nodes) == 0){
    return(NA)
  }
  
  bet <- betweenness(g)
  
  mean(bet[chorus_nodes]) / sum(bet)
  
}

results$chorus_brokerage <- sapply(
  results$play,
  compute_chorus_brokerage
)


# 8: save datasets -------------------------------------------------------

write_csv(results, "chorus_complete_dataset.csv")

# 9: plot author comparision ----------------------------------------------

# 9.1 chorus mediation
#betweenness across plays

colours <- c("#c50000","#ffce00", "#5859a7","#aaddec")

chorus_mediation <- ggplot(results,
       
       aes(x=reorder(play, chorus_betweenness),
           y=chorus_betweenness,
           fill = firstAuthorName,
           colour = colour)) +
  
  geom_col() +
  coord_flip() +
  
  labs(
    
    title="Chorus Mediation Across Greek Drama",
    x="Play",
    y="Chorus Betweenness"
    
  )

ggsave(
  "plots/chorus_mediation.png",
  plot = chorus_mediation,
  width = 10,
  height = 7,
  dpi = 300
)


# 9.2 Chorus connectivity by playwright
#degree by author

chorus_connectivity <- ggplot(results,
       
       aes(x=firstAuthorName,
           y=chorus_degree,
           fill = firstAuthorName,
           colour = colours)) +
  
  geom_boxplot() +
  
  labs(
    
    title="Chorus Degree Centrality by Author",
    x="Playwright",
    y="Chorus Degree"
    
  )

ggsave(
  "plots/chorus_connectivity.png",
  plot = chorus_connectivity,
  width = 10,
  height = 7,
  dpi = 300
)

# 9.3 Chorus Brokerage by Author

chorus_brokerage <- ggplot(results,
                           aes(
                             x = firstAuthorName,
                             y = chorus_brokerage,
                             fill = firstAuthorName,
                             colour = colours
                           )
) +
  geom_boxplot() +
  labs(
    title = "Chorus Brokerage by Playwright",
    x = "Playwright",
    y = "Brokerage Score"
  )

ggsave(
  "plots/chorus_brokerage.png",
  plot = chorus_brokerage,
  width = 8,
  height = 6,
  dpi = 300
)


# 10: visualize networks --------------------------------------------------

plot_dracor_network <- function(play_id){
  
  g <- get_net_cooccur_igraph(play_id, corpus="greek")
  g <- prepare_graph(g)
  
  V(g)$chorus <- grepl("Χορός", V(g)$name, ignore.case=TRUE)
  V(g)$degree <- degree(g)
  
  p <- ggraph(as_tbl_graph(g), layout="fr") +
    
    geom_edge_link(alpha=.3) +
    
    geom_node_point(
      aes(size=degree,
          color=chorus)
    ) +
    
    geom_node_text(
      aes(label=name),
      repel=TRUE,
      size=3
    ) +
    
    scale_color_manual(
      values=c("grey40","red")
    ) +
    theme_graph() +
    ggtitle(play_id)
  
  ggsave(
    paste0("plots/network_", play_id, ".png"),
    plot = p,
    width = 8,
    height = 8,
    dpi = 300
  )
  
}

# 10.1 example network plot

plot_dracor_network("sophocles-antigone")
