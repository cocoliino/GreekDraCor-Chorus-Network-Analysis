# Greek Chorus Network Analysis

library(igraph)
library(dplyr)
library(rdracor)
library(tibble)

# output directory for plots and CSVs
output_dir <- "~/R/project report/analysis_results"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# 1: Aquire Data -------------------------------------------------------------
# rdracor package for co-occurrence network 
# A "co-occurrence" means two characters appear in the same scene/segment
# Starting with a single example play: Sophocles' Antigone

# Fetch the co-occurrence network as an igraph object directly
G <- get_net_cooccur_igraph(play = "sophocles-antigone", corpus = "greek")

# print graph summary
"Network summary for Antigone"
print(G)
cat("Number of nodes (characters):", vcount(G))
cat("Number of edges (co-occurrences):", ecount(G))

#vertex (node) names are the character IDs
print(V(G)$name)

#Attributes of vertices in GreekDracor
cat("\nVertex attributes:", vertex_attr_names(G), "\n")
cat("Edge attributes:", edge_attr_names(G), "\n")


# 2: Identify Chorus Node -------------------------------------------------

# Chorus is labeled as "Χορός"

# Look for "Χορός" in the vertex name (case-insensitive)
chorus_idx <- grep("Χορός", V(G)$name, ignore.case = TRUE)

if (length(chorus_idx) > 0) {
  chorus_name <- V(G)$name[chorus_idx[1]]
  cat("Chorus node found:", chorus_name, "\n")
} else {
  cat("FAIl: No chorus 'Χορός' node found in this play\n")
  chorus_name <- NA
}

# 3: Global Network Metrics -----------------------------------------------

#Compute all metrics for Antigone

#3.1 Density
# "How many of the possible connections are actually realized?"
# Formula: actual edges / possible edges
# Range: 0 (no edges) to 1 (every node connected to every other node)

density_val <- edge_density(G)
cat("Density:", density_val, "\n")
cat("Interpretation: In Antigone,", round(density_val * 100, 1),
    "% of all possible character pairs actually co-occur in a scene.")

#3.2 Shortest path
# "What is the shortest route between two specific nodes?"

# choose specific character to demonstrate. e.g., the first non-chorus character
other_chars <- V(G)$name[V(G)$name != chorus_name]
example_target <- other_chars[1]

sp <- shortest_paths(G, from = chorus_name, to = example_target)
cat("Shortest path from", chorus_name, "to", example_target, ":\n")
cat("  Path:", paste(V(G)$name[sp$vpath[[1]]], collapse = " -> "), "\n")
cat("  Length:", length(sp$vpath[[1]]) - 1, "steps\n")

#3.3 Diameter
# "What is the longest shortest path in the network?"
# maximum 'distance' between any two characters

diam <- diameter(G)
cat("Diameter:", diam, "\n")
cat("Interpretation: The two most distant characters in Antigone are",
    diam, "steps apart.\n")

#3.4 Components
# "Is the network connected, or does it break into separate pieces?"
# Greek plays are usually a single connected component

comp <- components(G)
cat("Number of components:", comp$no, "\n")
cat("Size of largest component:", max(comp$csize), "\n")
if (comp$no == 1) {
  cat("The network is fully connected,every character can reach every other.\n")
  cat("(This is typical for Greek drama, especially when the chorus is present.)\n")
} else {
  cat("The network has disconnected parts.\n")
}

#3.5 Transitivity (Clustering coefficient)
# "If A knows B and B knows C, do A and C also know each other?"
# High transitivity = many closed triangles = tight-knit network

trans <- transitivity(G, type = "global")
cat("Global transitivity:", round(trans, 4), "\n")
cat("Interpretation:", round(trans * 100, 1),
    "% of connected character triples form closed triangles.\n")


# 4: Local Metrics of Centrality -------------------------------------------

#Compute Degree, Eigenvector and Betweenness and specifically compare the chorus to other characters

# 4.1 Degree centrality
# "How many other characters does each character co-occur with?"

deg <- degree(G)
cat("Top 10 characters by degree:\n")
print(sort(deg, decreasing = TRUE)[1:10])

cat("\nChorus degree:", deg[chorus_name], "out of", vcount(G) - 1, "possible\n")
cat("Chorus rank:", which(names(sort(deg, decreasing = TRUE)) == chorus_name),
      "of", vcount(G), "\n")

# 4.2 Eigenvector centrality
# "Is this character connected to other well-connected characters?"
# High eigenvector centrality = connected to central/important nodes

eig <- eigen_centrality(G)$vector
cat("Top 10 characters by eigenvector centrality:\n")
print(sort(eig, decreasing = TRUE)[1:10])

cat("\nChorus eigenvector centrality:", round(eig[chorus_name], 4), "\n")
cat("Chorus rank:", which(names(sort(eig, decreasing = TRUE)) == chorus_name),
      "of", vcount(G), "\n")

# 4.3 Betweenness centrality
# "Does this character lie on the shortest paths between other characters?"
# High betweenness = the character is a bridge/mediator
# interesting for chorus since scholars like Bacon (1994)
# argue the chorus MEDIATES between characters

betw <- betweenness(G)
cat("Top 10 characters by betweenness:\n")
print(sort(betw, decreasing = TRUE)[1:10])

cat("\nChorus betweenness:", round(betw[chorus_name], 4), "\n")
cat("Chorus rank:", which(names(sort(betw, decreasing = TRUE)) == chorus_name),
      "of", vcount(G), "\n")

# 5: Community Detection ---------------------------------------------------

# "Can we identify subgroups (communities) within the network?"
# The exercise used greedy modularity optimization: cluster_fast_greedy(G)
# Communities = groups of characters who interact more with each other
# than with the rest of the network

c1 <- cluster_fast_greedy(G)

# Store community membership as a vertex attribute
G <- set_vertex_attr(G, "modularity", index = V(G), membership(c1))

# Modularity score:how well-separated are the communities?
# Range: -0.5 to 1. Higher = more distinct communities
cat("Modularity:", round(modularity(c1), 4), "\n")

# Number of communities
cat("Number of communities:", length(c1), "\n")

# Size of each community
cat("Community sizes:\n")
print(sizes(c1))

# Which community is the chorus in?
chorus_community <- membership(c1)[chorus_name]
cat("\nChorus is in community:", chorus_community, "\n")

# Who else is in the chorus's community?
same_community <- names(membership(c1)[membership(c1) == chorus_community])
cat("Other members of the chorus's community:",
    paste(same_community[same_community != chorus_name], collapse = ", "), "\n")

# 6: Analyze all plays ---------------------------------------------------
# apply everything learned above to plays with chorus in GreekDraCor

# get all Greek plays from DraCor
greek_plays <- get_dracor(corpus = "greek")
cat("Total plays in GreekDraCor:", nrow(greek_plays), "\n\n")

# a results table for one row per play
results <- data.frame(
  play_name = character(),
  author = character(),
  num_characters = integer(),
  num_edges = integer(),
  density = numeric(),
  diameter = integer(),
  num_components = integer(),
  transitivity = numeric(),
  # Chorus-specific metrics
  chorus_found = logical(),
  chorus_degree = integer(),
  chorus_degree_norm = numeric(),     # chorus degree / (n-1)
  chorus_degree_rank = integer(),
  chorus_eigenvector = numeric(),
  chorus_eigenvector_rank = integer(),
  chorus_betweenness = numeric(),
  chorus_betweenness_rank = integer(),
  chorus_community = integer(),
  num_communities = integer(),
  modularity = numeric()
)

# Loop over each play
for (i in 1:nrow(greek_plays)) {
  play_id <- greek_plays$playName[i]
  play_title <- greek_plays$title[i]

  # extract author info from rdracor
  author_name <- if ("firstAuthorName" %in% names(greek_plays)) {
    greek_plays$firstAuthor[i]
  } else if ("author" %in% names(greek_plays)) {
    greek_plays$author[i]
  } else {
    "Unknown"
  }

  cat("Processing", i, "/", nrow(greek_plays), ":", play_title,
      "(", play_id, ") by", author_name, "... ")

  # Try fetch network
  tryCatch({
    g <- get_net_cooccur_igraph(play = play_id, corpus = "greek")

    # Find chorus
    ch_idx <- grep("Χορός", V(g)$name, ignore.case = TRUE)
    ch_found <- length(ch_idx) > 0
    ch_name <- if (ch_found) V(g)$name[ch_idx[1]] else NA

    # Global metrics
    dens <- edge_density(g)
    diam_val <- if (is_connected(g)) diameter(g) else NA
    comp_val <- components(g)
    trans_val <- transitivity(g, type = "global")

    # Local metrics
    deg_all <- degree(g)
    eig_all <- eigen_centrality(g)$vector
    betw_all <- betweenness(g)

    # Chorus metrics
    ch_deg <- if (ch_found) deg_all[ch_name] else NA
    ch_deg_norm <- if (ch_found) ch_deg / (vcount(g) - 1) else NA
    ch_deg_rank <- if (ch_found) which(names(sort(deg_all, decreasing = TRUE)) == ch_name) else NA
    ch_eig <- if (ch_found) eig_all[ch_name] else NA
    ch_eig_rank <- if (ch_found) which(names(sort(eig_all, decreasing = TRUE)) == ch_name) else NA
    ch_betw <- if (ch_found) betw_all[ch_name] else NA
    ch_betw_rank <- if (ch_found) which(names(sort(betw_all, decreasing = TRUE)) == ch_name) else NA

    # Community detection
    c_det <- cluster_fast_greedy(g)
    ch_comm <- if (ch_found) membership(c_det)[ch_name] else NA

    # Store results
    results <- rbind(results, data.frame(
      play_name = play_title,
      author = author_name,
      num_characters = vcount(g),
      num_edges = ecount(g),
      density = round(dens, 4),
      diameter = ifelse(is.na(diam_val), NA, diam_val),
      num_components = comp_val$no,
      transitivity = round(trans_val, 4),
      chorus_found = ch_found,
      chorus_degree = ifelse(is.na(ch_deg), NA, ch_deg),
      chorus_degree_norm = ifelse(is.na(ch_deg_norm), NA, round(ch_deg_norm, 4)),
      chorus_degree_rank = ifelse(is.na(ch_deg_rank), NA, ch_deg_rank),
      chorus_eigenvector = ifelse(is.na(ch_eig), NA, round(ch_eig, 4)),
      chorus_eigenvector_rank = ifelse(is.na(ch_eig_rank), NA, ch_eig_rank),
      chorus_betweenness = ifelse(is.na(ch_betw), NA, round(ch_betw, 4)),
      chorus_betweenness_rank = ifelse(is.na(ch_betw_rank), NA, ch_betw_rank),
      chorus_community = ifelse(is.na(ch_comm), NA, ch_comm),
      num_communities = length(c_det),
      modularity = round(modularity(c_det), 4),
      stringsAsFactors = FALSE
    ))

    cat("OK\n")

  }, error = function(e) {
    cat("ERROR:", e$message, "\n")
  })
}

cat("Processed", nrow(results), "plays successfully\n")


# 7: Result Overview -----------------------------------------------------

# only plays that have a chorus
chorus_plays <- results %>% filter(chorus_found == TRUE)
cat("Plays with a chorus node:", nrow(chorus_plays), "out of", nrow(results), "\n\n")

# 7.1 Chorus degree across authors
chorus_plays %>%
  group_by(author) %>%
  summarise(
    n_plays = n(),
    avg_chorus_degree = round(mean(chorus_degree, na.rm = TRUE), 2),
    avg_chorus_degree_norm = round(mean(chorus_degree_norm, na.rm = TRUE), 4),
    avg_chorus_degree_rank = round(mean(chorus_degree_rank, na.rm = TRUE), 1)
  ) %>%
  print()

# 7.2 Chorus betweenness across authors
cat("High betweenness = chorus mediates between character groups\n")
chorus_plays %>%
  group_by(author) %>%
  summarise(
    n_plays = n(),
    avg_chorus_betweenness = round(mean(chorus_betweenness, na.rm = TRUE), 2),
    avg_chorus_betw_rank = round(mean(chorus_betweenness_rank, na.rm = TRUE), 1)
  ) %>%
  print()

# 7.3 Chorus eigenvector centrality across authors
cat("High eigenvector = chorus is connected to well-connected characters\n")
chorus_plays %>%
  group_by(author) %>%
  summarise(
    n_plays = n(),
    avg_chorus_eigenvector = round(mean(chorus_eigenvector, na.rm = TRUE), 4),
    avg_chorus_eig_rank = round(mean(chorus_eigenvector_rank, na.rm = TRUE), 1)
  ) %>%
  print()


# 8: Save results --------------------------------------------------------

# full results table as CSV
write.csv(results, file.path(output_dir, "chorus_network_metrics_all_plays.csv"),
          row.names = FALSE)
cat("Results saved to:", file.path(output_dir, "chorus_network_metrics_all_plays.csv"), "\n")

# summary table by author
author_summary <- chorus_plays %>%
  group_by(author) %>%
  summarise(
    n_plays = n(),
    avg_density = round(mean(density), 4),
    avg_diameter = round(mean(diameter, na.rm = TRUE), 1),
    avg_transitivity = round(mean(transitivity, na.rm = TRUE), 4),
    avg_chorus_degree_norm = round(mean(chorus_degree_norm, na.rm = TRUE), 4),
    avg_chorus_betweenness = round(mean(chorus_betweenness, na.rm = TRUE), 2),
    avg_chorus_eigenvector = round(mean(chorus_eigenvector, na.rm = TRUE), 4),
    avg_modularity = round(mean(modularity, na.rm = TRUE), 4)
  )
write.csv(author_summary, file.path(output_dir, "chorus_metrics_by_author.csv"),
          row.names = FALSE)
cat("Author summary saved to:", file.path(output_dir, "chorus_metrics_by_author.csv"), "\n")

# 9: Plots ---------------------------------------------------------------

colours <- c("#c50000","#ffce00", "#5859a7","#aaddec")

# Plot 1: Chorus degree (normalized) by author
png(file.path(output_dir, "chorus_degree_by_author.png"),
    width = 900, height = 600, res = 120)
boxplot(chorus_degree_norm ~ author, data = chorus_plays,
        main = "Chorus Degree Centrality (Normalized) by Author",
        ylab = "Normalized Degree", xlab = "Author",
        col = colours,
        las = 1)
dev.off()
cat("Plot saved: chorus_degree_by_author.png\n")

# Plot 2: Chorus betweenness by author
png(file.path(output_dir, "chorus_betweenness_by_author.png"),
    width = 900, height = 600, res = 120)
boxplot(chorus_betweenness ~ author, data = chorus_plays,
        main = "Chorus Betweenness Centrality by Author",
        ylab = "Betweenness", xlab = "Author",
        col = colours,
        las = 1)
dev.off()
cat("Plot saved: chorus_betweenness_by_author.png\n")

# Plot 3: Network density by author
png(file.path(output_dir, "density_by_author.png"),
    width = 900, height = 600, res = 120)
boxplot(density ~ author, data = chorus_plays,
        main = "Network Density by Author",
        ylab = "Density", xlab = "Author",
        col = colours,
        las = 1)
dev.off()
cat("Plot saved: density_by_author.png\n")
