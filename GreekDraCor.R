# Chorus Network Analysis
# GreekDraCor corpus via rdracor API

# Structure:
# 1 Packages
# 2 Data import via rdracor
# 3 Graph creation
# 4 Network metrics: density, shortest path, diameter, components, transitivity
# 5 Centrality: degree, eigenvector, betweenness
# 6 Community detection (greedy modularity)
# 7 Visualization
# 8 Scale up: all Greek plays with chorus comparison
# 9 Summary plots

# Required packages:
# install.packages(c("igraph", "dplyr", "rdracor"))

# 1: set-up -------------------------------------------------------------

library(igraph)
library(dplyr)
library(rdracor)

#Output folder
output_dir <- "figures"
dir.create(output_dir, showWarnings = FALSE)

# 2: data import ---------------------------------------------------------

# rdracor provides a co-occurrence network directly from the DraCor API as an igraph object.
G <- get_net_cooccur_igraph(play = "sophocles-antigone", corpus = "greek")
print(G)

# how many Nodes and Edges are in the network?
no_nodes <- vcount(G)
no_edges <- ecount(G)
cat("Nodes:", no_nodes, " Edges:", no_edges, "\n")

# which vertex/nodes attributes area available?
node_attr <- vertex_attr_names(G)
cat("Node attributes:", paste(node_attr, collapse = ", "), "\n")

# which edge attributes are available?
edge_attr <- edge_attr_names(G)
cat("Edge attributes:", paste(edge_attr, collapse = ", "), "\n")

# Character names in the Play, in Greek
char_names <- V(G)$name
cat("Characters:", paste(char_names, collapse = ", "), "\n")

# 3: Identify Chorus ------------------------------------------------------

# Greek drama networks contain a chorus node (name contains "Χορός")
# all chorus will be added, e.g. also woman chorus etc. CHORAL IDENTITYS
chorus_idx <- grep("Χορός", V(G)$name, ignore.case = TRUE)

chorus_name <- if (length(chorus_idx) > 0) V(G)$name[chorus_idx[1]] else NA

cat("Chorus node:", chorus_name, "\n")

# 4: Global Network Metrics ----------------------------------------------

#compute Global Network Metrics
# add would be interesting to know if chorus is in longest of all paths?

# 4.1 Density
# Verhältnis nodes zu edges
density_val <- edge_density(G)
cat("Network density:", format(density_val, digits = 4), "\n")

# 4.2 Diameter 
# longest of all shortest path, overall size of network, distance from one end to the other
diam <- diameter(G)
cat("Diameter:", diam, "\n")

# 4.3 Components
# how many seperated components in network. result 1 meaning all nodes are connected
comp <- components(G)
cat("Components:", comp$no, " Largest:", max(comp$csize), "\n")

#If disconnected, compute diameter on largest component
if (comp$no > 1) {
  dg <- decompose.graph(G)
  subgraph <- dg[[1]]
  cat("Diameter of largest component:", diameter(subgraph), "\n")
}

# 4.4 Transitivity 
# likeliness of triadic closure
# ratio of all conceivable triadic relationships to real triadic relationships
trans_val <- transitivity(G, type = "global")
cat("Transitivity:", round(trans_val, 4), "\n")


# 5: Centrality Measures --------------------------------------------------
# importance of individual nodes

# 5.1 Degree
# sum of all edges of a node
# importance of a node?
# high degree called hubs
degree.G <- degree(G)

#top characters by degree
print(sort(degree.G, decreasing = TRUE))

#where does the chorus degree rank compared to other characters
chorus_degreeRank = which(names(sort(degree.G, decreasing = TRUE)) == chorus_name)
cat("\nChorus degree:", degree.G[chorus_name],
      " Rank:", chorus_degreeRank,
      "of", vcount(G), "\n")

# 5.2 Eigenvector centrality
# degree with consideration of connected nodes
# degree of one node and also degrees of connected nodes
# high: information can be distributed quickly

eigen_centrality.G <- eigen_centrality(G)

print(sort(eigen_centrality.G$vector, decreasing = TRUE))

# Chorus Rank of Eigenvectors
chorus_eigenvRank <-which(names(sort(eigen_centrality.G$vector, decreasing = TRUE)) == chorus_name)

cat("\nChorus eigenvector:", eigen_centrality.G$vector[chorus_name],
      " Rank:", chorus_eigenvRank,
      "of", vcount(G), "\n")

# 5.3 Betweenness centrality
# number of shortest paths that pass through a node (0-1)

#identifies brokers

betweenness_centrality.G <- betweenness(G, normalized = TRUE)
print(sort(betweenness_centrality.G, decreasing = TRUE))

# Chorus Rank of Betweenness centrality
chorus_betweenRank <- which(names(sort(betweenness_centrality.G, decreasing = TRUE)) == chorus_name)

cat("\nChorus betweenness:", round(betweenness_centrality.G[chorus_name], 4),
      " Rank:", chorus_betweenRank,
      "of", vcount(G), "\n")


# 6: Community Detection -------------------------------------------------
# measure of modularity
# nodes that have a high density to each other (mutually connected), but low density to other nodes

# Greedy modularity communities
c1 <- cluster_fast_greedy(G)
G <- set_vertex_attr(G, "modularity", index = V(G), membership(c1))
node_attr <- vertex_attr_names(G)

# modularity measure
cat("Modularity:", (modularity(c1)), "\n")

# how many communities?
cat("Number of communities:", length(c1), "\n")

# size of each community?
cat("Community sizes:\n", sizes(c1))

# in which community is chorus?
chorus_comm <- membership(c1)[chorus_name]
members <- names(membership(c1)[membership(c1) == chorus_comm])

cat("Chorus is in community", chorus_comm, "together with:\n", paste(members, collapse = ", "), "\n")

# safe metrics ------------------------------------------------------------

#safe metrics
antigone <- tibble(
  chorusName = chorus_name,
  nodes = no_nodes,
  edges = no_edges,
  density = density_val,
  chorusDegreeRank = chorus_degreeRank,
  chorusEigenvRank = chorus_eigenvRank,
  chorusBetweenRank = chorus_betweenRank
)

#  7: Visualization -------------------------------------------------------

# Community plot with chorus highlighted

# Label only higher-degree nodes
label_proxy <- ifelse(degree.G >= 2, V(G)$name, NA)

# Node size proportional to degree
node_size <- degree.G / 2 + 2

node_size[chorus_idx] <- node_size[chorus_idx] + 4


# Save plot
png(file.path(output_dir, "antigone_network.png"),
    width = 1000, height = 800, res = 120)

plot(c1, G,
     vertex.size = node_size,
     vertex.label = label_proxy,
     vertex.label.font = 1,
     vertex.label.cex = 0.8,
     vertex.label.color = "black",
     vertex.label.dist = 1,
     edge.arrow.size = 0.25,
     edge.curved = 0.1,
     main = "Antigone: Character Network with Communities")

dev.off()

#  8: Scale to all plays --------------------------------------------------

# Loop over the full GreekDraCor corpus and record chorus metrics per play.

greek_plays <- get_dracor(corpus = "greek")

results <- list()

for (i in seq_len(nrow(greek_plays))) {
  
  pid <- greek_plays$playName[i]
  
  title <- greek_plays$title[i]
  
  # Author
  author <- tryCatch(
    greek_plays$firstAuthor[i],
    error = function(e) "Unknown"
  )
  
  if (is.null(author) || is.na(author)) author <- "Unknown"
  
  cat(sprintf("[%2d/%d] %s (%s)... ", i, nrow(greek_plays), title, pid))
  
  tryCatch({
    g <- get_net_cooccur_igraph(play = pid, corpus = "greek")
    
    # Chorus
    ch_idx <- grep("Χορός", V(g)$name, ignore.case = TRUE)
    ch_found <- length(ch_idx) > 0
    ch_name <- if (ch_found) V(g)$name[ch_idx[1]] else NA
    
    
    # Global metrics
    dens <- edge_density(g)
    diam <- if (is_connected(g)) diameter(g) else NA
    ncomp <- components(g)$no
    trans <- transitivity(g, type = "global")
    
    # Centrality
    deg <- degree(g)
    eig <- eigen_centrality(g)$vector
    betw <- betweenness(g, normalized = TRUE)
    
    # Chorus centrality
    ch_deg <- if (ch_found) deg[ch_name] else NA
    
    ch_deg_norm <- if (ch_found) ch_deg / (vcount(g) - 1) else NA
    
    ch_eig <- if (ch_found) eig[ch_name] else NA
    
    ch_betw <- if (ch_found) betw[ch_name] else NA
    
    
    
    # Protagonist = highest-degree non-chorus character
    
    deg_sorted <- sort(deg, decreasing = TRUE)
    
    non_ch <- deg_sorted[!grepl("chorus", names(deg_sorted), ignore.case = TRUE)]
    
    protag <- names(non_ch)[1]
    
    protag_deg <- non_ch[1]
    
    
    
    # Community detection
    
    comm <- cluster_fast_greedy(g)
    
    ch_comm <- if (ch_found) membership(comm)[ch_name] else NA
    
    pr_comm <- membership(comm)[protag]
    
    same_comm <- if (ch_found && !is.na(ch_comm)) ch_comm == pr_comm else NA
    
    
    
    results[[i]] <- tibble(
      
      play = title,
      
      play_id = pid,
      
      author = author,
      
      n_chars = vcount(g),
      
      n_edges = ecount(g),
      
      density = round(dens, 4),
      
      diameter = diam,
      
      n_components = ncomp,
      
      transitivity = round(trans, 4),
      
      chorus_found = ch_found,
      
      chorus_degree = as.integer(ch_deg),
      
      chorus_deg_norm = round(ch_deg_norm, 4),
      
      chorus_eigen = round(ch_eig, 4),
      
      chorus_betw = round(ch_betw, 4),
      
      protagonist = protag,
      
      protag_degree = as.integer(protag_deg),
      
      same_community = same_comm,
      
      modularity = round(modularity(comm), 4)
      
    )

    
    cat("OK\n")
  }, error = function(e) {
    cat("ERROR:", e$message, "\n")
  })
  
}

results_df <- bind_rows(results)

cat("\nProcessed:", nrow(results_df), "plays\n")

# Filter to plays with a chorus
chorus_df <- results_df %>% filter(chorus_found)
cat("Plays with chorus:", nrow(chorus_df), "\n")

#  9: summary -------------------------------------------------------------

cat("\n--- Chorus centrality by author ---\n")

chorus_df %>%
  
  group_by(author) %>%
  
  summarise(
    
    n = n(),
    
    mean_deg = round(mean(chorus_deg_norm, na.rm = TRUE), 3),
    
    sd_deg = round(sd(chorus_deg_norm, na.rm = TRUE), 3),
    
    mean_betw = round(mean(chorus_betw, na.rm = TRUE), 3),
    
    mean_eigen = round(mean(chorus_eigen, na.rm = TRUE), 3),
    
    same_comm_pct = round(100 * mean(same_community, na.rm = TRUE), 1),
    
    .groups = "drop"
    
  ) %>%
  
  print()



# Save CSVs

write.csv(results_df, file.path(output_dir, "all_plays_metrics.csv"),
          
          row.names = FALSE)

write.csv(chorus_df, file.path(output_dir, "chorus_metrics.csv"),
          
          row.names = FALSE)

cat("\nCSVs saved to", output_dir, "\n")



#  10: Plots --------------------------------------------------------------

# Colour palette by author
# https://www.color-hex.com/color-palette/62377, "drama Colour Palette" by bethany1999

author_cols <- c("#c50000","#ffce00", "#5859a7","#aaddec")

# Plot 1: Chorus degree (normalised) by author 

png(file.path(output_dir, "fig_chorus_degree.png"),
    
    width = 900, height = 600, res = 120)

boxplot(chorus_deg_norm ~ author, data = chorus_df,
        
        main = "Chorus Degree Centrality by Playwright",
        
        ylab = "Normalised Degree", xlab = "",
        
        col = author_cols[levels(factor(chorus_df$author))],
        
        las = 1)

# Highlight Antigone
ant <- chorus_df %>% filter(play_id == "sophocles-antigone")

if (nrow(ant) > 0) {
  x_pos <- which(levels(factor(chorus_df$author)) == ant$author[1])
  
  points(x_pos, ant$chorus_deg_norm, pch = 8, cex = 2, col = "red", lwd = 2)
  
  text(x_pos, ant$chorus_deg_norm, "Antigone", pos = 4, cex = 0.8, col = "red")
  
}

dev.off()

cat("Saved fig_chorus_degree.png\n")


# Plot 2: Chorus vs protagonist scatter

png(file.path(output_dir, "fig_chorus_vs_protagonist.png"),
    
    width = 800, height = 800, res = 120)

auth_f <- factor(chorus_df$author)

plot(chorus_df$protag_degree, chorus_df$chorus_degree,
     
     main = "Chorus vs. Protagonist Degree",
     
     xlab = "Protagonist Degree", ylab = "Chorus Degree",
     
     pch = 19, col = author_cols[as.character(auth_f)])

abline(0, 1, lty = 2, col = "gray50")

legend("topleft", legend = levels(auth_f),
       
       col = author_cols[levels(auth_f)], pch = 19, cex = 0.8)

# Label Antigone

if (nrow(ant) > 0)
  
  text(ant$protag_degree, ant$chorus_degree, "Antigone",
       
       pos = 3, cex = 0.8, font = 2, col = "red")

dev.off()

cat("Saved fig_chorus_vs_protagonist.png\n")



# Plot 3: Community co-membership ---

comm_pct <- chorus_df %>%
  
  group_by(author) %>%
  
  summarise(pct = round(100 * mean(same_community, na.rm = TRUE), 1),
            
            .groups = "drop")



png(file.path(output_dir, "fig_community.png"),
    
    width = 800, height = 600, res = 120)

bp <- barplot(comm_pct$pct, names.arg = comm_pct$author,
              
              main = "Chorus–Protagonist Same Community (%)",
              
              ylab = "Percentage", ylim = c(0, 110),
              
              col = author_cols[comm_pct$author])

text(bp, comm_pct$pct + 3, paste0(comm_pct$pct, "%"), font = 2, cex = 0.9)

dev.off()

cat("Saved fig_community.png\n")


# Plot 4: Chorus betweenness by author ---

png(file.path(output_dir, "fig_chorus_betweenness.png"),
    
    width = 900, height = 600, res = 120)

boxplot(chorus_betw ~ author, data = chorus_df,
        
        main = "Chorus Betweenness Centrality by Playwright",
        
        ylab = "Betweenness (normalised)", xlab = "",
        
        col = author_cols[levels(factor(chorus_df$author))],
        
        las = 1)

dev.off()

cat("Saved fig_chorus_betweenness.png\n")


cat("Outputs in:", normalizePath(output_dir, mustWork = FALSE), "\n")
