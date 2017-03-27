library(tidyverse)
library(stringr)
library(pander)
library(ggraph)
library(igraph)
library(Cairo)


# Generate table of index components -------------------------------------

component.labels <- c(
  "Government efficiency",
  "Government rule of law",
  "Government infrastructure",
  "Government access to markets",
  "Business efficiency",
  "Business performance",
  "Business freedom",
  "Individual well-being",
  "Individual innovation",
  "Individual freedom"
)

components.long <- read_csv(file.path(PROJHOME, "Data",
                                      "business_indices_components.csv")) %>%
  # rowwise() %>%
  # mutate(index_long = ifelse(is.na(short_name), index,
  #                            str_interp("${index} (${short_name})", .))) %>%
  select(index = short_name, start_year, components_clean) %>%
  ungroup() %>%
  separate_rows(components_clean, sep = ", ") 

components.wide <- components.long %>%
  mutate(dot = "•",
         components_clean = factor(components_clean, levels = component.labels, 
                                   ordered = TRUE)) %>%
  spread(components_clean, dot) %>%
  arrange(start_year) %>%
  rename(Index = index, `Start year` = start_year)

output <- pandoc.table.return(components.wide, split.tables = Inf, missing = "", 
                              justify = paste0(c("l", rep("c", ncol(components.wide) - 1)),
                                               collapse = ""),
                              caption = "Ideological components of business indices")

cat(output, file = file.path(PROJHOME, "Output", 
                             "table_business_indicies_components.txt"))

Pandoc.convert(file.path(PROJHOME, "Output", 
                         "table_business_indicies_components.txt"),
               format = "html", footer = FALSE, proc.time = FALSE, 
               options = "-s", open = FALSE)

Pandoc.convert(file.path(PROJHOME, "Output", 
                         "table_business_indicies_components.txt"),
               format = "docx", open = FALSE)

write_csv(components.wide, file.path(PROJHOME, "Output", 
                                     "table_business_indicies_components.csv"), 
          na = "")


# Network terminology ----------------------------------------------------
#
# Affiliation / incidence matrix
#
# set.seed(1234)
# M <- matrix(sample(0:1, 15, replace=TRUE), nc=5)
#
#      [,1] [,2] [,3] [,4] [,5]
# [1,]    0    1    0    1    0
# [2,]    1    1    0    1    1
# [3,]    1    1    1    1    0

# Adjacency matrix
#
# M.adj <- M %*% t(M)
#
#      [,1] [,2] [,3]
# [1,]    2    2    2
# [2,]    2    4    3
# [3,]    2    3    4


# Network graph ----------------------------------------------------------
#
# Convert long data frame to matrix with row and column names
components.incidence <- components.long %>%
  filter(components_clean != "Government efficiency") %>%
  rename(thing = index) %>%
  mutate(dot = 1) %>%
  spread(components_clean, dot, fill=0) %>%
  arrange(start_year) %>%
  as.data.frame() %>%  # So tibble doesn't yell about row names
  magrittr::set_rownames(.$thing) %>%
  select(-c(thing, start_year)) %>%
  as.matrix()

# Make into igraph object
g <- graph.incidence(components.incidence, directed = FALSE)

# Add metadata attributes to igraph object
# Anything added to V(g)$whatever gets transferred to the fake data frame
# created by create_layout
V(g)$color <- case_when(
  V(g)$type == TRUE & str_detect(V(g)$name, "^Business") ~ "Business",
  V(g)$type == TRUE & str_detect(V(g)$name, "^Government") ~ "Government",
  V(g)$type == TRUE & str_detect(V(g)$name, "^Individual") ~ "Individual",
  TRUE ~ "Index"
)

V(g)$size <- degree(g)^3


# Create plot
set.seed(16)
plot.network.df <- create_layout(g, layout = "graphopt")
# See ?layout_igraph_igraph for possible igraph layouts
# plot.network.df <- create_layout(g, layout = "kk")
# plot.network.df <- create_layout(g, layout = "dh")
# plot.network.df <- create_layout(g, layout = "gem")
# plot.network.df <- create_layout(g, layout = "fr")

components.graph <- ggraph(plot.network.df) + 
  geom_edge_fan(aes(alpha = ..index..), show.legend = FALSE) +
  geom_node_point(aes(color = color, fill = color, shape = type, size = size)) +
  geom_node_label(aes(color = color, fill = color, label = name),
                  size = 3, repel = TRUE, family = "Clear Sans", fontface = "bold") +
  scale_colour_manual(values = c(Index = "grey50", Business = "white", 
                                 Government = "white", Individual = "white")) +
  scale_fill_manual(values = c(Index = "white", Business = "#d47200", 
                               Government = "#8acc2d", Individual = "#b90081")) +
  scale_shape_manual(values = c(15, 21)) +
  guides(color = FALSE, shape = FALSE, size = FALSE, fill = FALSE) +
  theme_graph()
components.graph

ggsave(components.graph, 
       filename = file.path(PROJHOME, "Output", 
                            "indices_components_network.pdf"),
       width = 9, height = 5, units = "in", device = cairo_pdf)

ggsave(components.graph,
       filename = file.path(PROJHOME, "Output",
                            "indices_components_network.png"),
       width = 9, height = 5, units = "in", type = "cairo", dpi = 300)

