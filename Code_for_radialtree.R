library(tidyverse)
library(ggraph)
library(igraph)

set.seed(123)

n <- 70
categories <- c("CSP", "Studies", "Score", "Lifestyle", "Family", "Social", "Health")
characteristics <- paste0("Feature_", seq(1, n))

df <- data.frame(
  Caracteristic = characteristics,
  Category = sample(categories, n, replace = TRUE),
  Outcome = "Depression",
  OR = round(runif(n, 0.1, 5), 2),
  pval = signif(runif(n, 0, 0.5), 2)
) %>%
  mutate(color = case_when(
    OR > 1 & pval < 0.05 ~ "red",
    OR < 1 & pval < 0.05 ~ "blue",
    TRUE ~ "grey"
  ))

# Calcule largeur basée sur -log10(pval), avec un petit epsilon pour éviter Inf
df <- df %>%
  dplyr::mutate(width = OR)

# Arêtes entre catégorie et caractéristique avec couleur & poids (width basé sur p-value)
edges <- df %>%
  transmute(from = Category, to = Caracteristic, width = width, color = color) %>%
  bind_rows(
    df %>%
      distinct(Outcome, Category) %>%
      rename(from = Outcome, to = Category) %>%
      mutate(width = 1, color = "grey")
  )


# Graphe
graph <- graph_from_data_frame(edges, directed = TRUE)

# Attributs des nœuds
V(graph)$label <- V(graph)$name
V(graph)$color <- "lightgrey"
V(graph)$pval <- NA
V(graph)$size <- 2  # valeur par défaut

feature_nodes <- V(graph)$name %in% df$Caracteristic
V(graph)$color[feature_nodes] <- df$color[match(V(graph)$name[feature_nodes], df$Caracteristic)]
V(graph)$pval[feature_nodes] <- df$pval[match(V(graph)$name[feature_nodes], df$Caracteristic)]
V(graph)$size[feature_nodes] <- -log10(V(graph)$pval[feature_nodes] + 1e-10)  # Taille inversement liée à la p-value

# Tracé
ggraph(graph, layout = 'dendrogram', circular = TRUE) +
  geom_edge_diagonal(aes(width = width, color = I(color))) +
  geom_node_point(aes(size = size, color = I(color))) +
  geom_node_text(aes(label = label), repel = TRUE, size = 2.5) +
  scale_edge_width(range = c(0.3, 2.5), guide = "none") +  # <-- ici on enlève la légende
  scale_size_continuous(name = "-log10(p-value)", range = c(1, 6)) +
  theme_void()




