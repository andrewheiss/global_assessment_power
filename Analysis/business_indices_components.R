library(tidyverse)
library(stringr)
library(pander)
library(ggraph)
library(igraph)
library(Cairo)
library(gridExtra)
library(grid)
library(gtable)


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
# Helpful functions and themes
capitalize <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}

theme_gpa_table <- ttheme_minimal(
  core = 
    list(fg_params = 
           list(hjust = 0, x = 0.05,
                fontsize = 7,
                fontfamily = "Clear Sans")),
  colhead = 
    list(fg_params = 
           list(hjust = 0, x = 0.05,
                fontsize = 7, fontface = 2,
                fontfamily = "Clear Sans")))

find_cell <- function(table, row, col, name = "core-fg") {
  l <- table$layout
  which(l$t == row & l$l == col & l$name == name)
}

theme.cell.fg <- gpar(col = "white", fontsize = 7, 
                      fontfamily = "Clear Sans", fontface = 2)

theme.cell.bg <- function(color) {
  gpar(fill = color, col = "white", lwd = 3)
}


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

# Rescale nodes by degree
V(g)$size <- degree(g)^3

# Extract theme names from nodes
V(g)$name <- str_replace(V(g)$name,
                         "^Business |^Government |^Individual ", "") %>%
  capitalize()


# Create plot
seed <- 16
set.seed(seed)
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

# Create fake legend table
abbrs <- read_csv(file.path(PROJHOME, "Data",
                   "business_indices_components.csv")) %>%
  mutate(legend = sprintf("%s = %s", short_name, index)) %>%
  arrange(legend)

component.types <- c("Government", "Business", "Individual", "", "")

abbrs.mat <- cbind(component.types,
                   matrix(abbrs$legend, ncol=2, byrow=FALSE))
colnames(abbrs.mat) <- c("Themes", "Indexes", "")

# https://cran.r-project.org/web/packages/gridExtra/vignettes/tableGrob.html
leg.table <- tableGrob(abbrs.mat, theme = theme_gpa_table)

# Modify specific cells
bg.gov <- find_cell(leg.table, 2, 1, "core-bg")
bg.bus <- find_cell(leg.table, 3, 1, "core-bg")
bg.ind <- find_cell(leg.table, 4, 1, "core-bg")

fg.gov <- find_cell(leg.table, 2, 1, "core-fg")
fg.bus <- find_cell(leg.table, 3, 1, "core-fg")
fg.ind <- find_cell(leg.table, 4, 1, "core-fg")

leg.table$grobs[fg.gov][[1]][["gp"]] <- theme.cell.fg
leg.table$grobs[fg.bus][[1]][["gp"]] <- theme.cell.fg
leg.table$grobs[fg.ind][[1]][["gp"]] <- theme.cell.fg

leg.table$grobs[bg.gov][[1]][["gp"]] <- theme.cell.bg("#8acc2d")
leg.table$grobs[bg.bus][[1]][["gp"]] <- theme.cell.bg("#d47200")
leg.table$grobs[bg.ind][[1]][["gp"]] <- theme.cell.bg("#b90081")

# grid.newpage(); grid.draw(leg.table)

# Add legend plot to actual plot
set.seed(seed)
components.graph.legend <- components.graph + 
  coord_cartesian(xlim = c(-80, 100), ylim = c(-120, 70)) +
  annotation_custom(leg.table, xmin=-80, xmax=25, ymin=-120, ymax=-80)
components.graph.legend

set.seed(seed)
ggsave(components.graph.legend, 
       filename = file.path(PROJHOME, "Output", 
                            "indices_components_network.pdf"),
       width = 9, height = 5, units = "in", device = cairo_pdf)

set.seed(seed)
ggsave(components.graph.legend,
       filename = file.path(PROJHOME, "Output",
                            "indices_components_network.png"),
       width = 9, height = 5, units = "in", type = "cairo", dpi = 300)
