#' ---
#' title: 'Figures for "Global Assessment Power in the Twenty-First Century"'
#' author: "Judith Kelley and Beth Simmons"
#' date: "`r format(Sys.time(), '%B %e, %Y')`"
#' ---

#+ message=FALSE
library(tidyverse)
library(readxl)
library(forcats)
library(Cairo)

# Useful functions
theme_gpa <- function(base_size=9, base_family="Clear Sans Light") {
  update_geom_defaults("label", list(family="Clear Sans Light"))
  update_geom_defaults("text", list(family="Clear Sans Light"))
  ret <- theme_bw(base_size, base_family) + 
    theme(panel.background = element_rect(fill="#ffffff", colour=NA),
          axis.title.y = element_text(margin = margin(r = 10)),
          axis.title.x = element_text(margin = margin(t = 10)),
          title=element_text(vjust=1.2, family="Clear Sans", face="bold"),
          plot.subtitle=element_text(family="Clear Sans Light"),
          plot.caption=element_text(family="Clear Sans Light",
                                    size=rel(0.8), colour="grey70"),
          panel.border = element_blank(), 
          axis.line=element_line(colour="grey50", size=0.2),
          #panel.grid=element_blank(), 
          axis.ticks=element_blank(),
          legend.position="bottom", 
          legend.title=element_text(size=rel(0.8)),
          axis.title=element_text(size=rel(0.8), family="Clear Sans", face="bold"),
          strip.text=element_text(size=rel(1), family="Clear Sans", face="bold"),
          strip.background=element_rect(fill="#ffffff", colour=NA),
          panel.margin.y=unit(1.5, "lines"),
          legend.key=element_blank(),
          legend.margin=unit(0.2, "lines"))
  
  ret
}

fig.save.cairo <- function(fig, filepath=file.path(PROJHOME, "Output"), 
                           filename, width, height, units="in", ...) {
  ggsave(fig, filename=file.path(filepath, paste0(filename, ".pdf")),
         width=width, height=height, units=units, device=cairo_pdf, ...)
  ggsave(fig, filename=file.path(filepath, paste0(filename, ".png")),
         width=width, height=height, units=units, type="cairo", dpi=300, ...)
}

# Load and clean data
gpa.data.raw <- read_excel(file.path(PROJHOME, "Data", "masterdata1.1.xlsx"),
                           sheet="main data sheet")

# Only use the first 30 columns. Need to use [,] indexing because dplyr::select
# chokes on all the duplicated NA column names
gpa.data.clean <- gpa.data.raw[, 1:30]

#' ## Figure 1: Cumulative number of GPAs.
year.chunks <- tribble(
  ~chunk_start, ~chunk_end, ~chunk_name,
  1900,         1969,       "Pre-1970",
  1970,         1974,       "1970–74",
  1975,         1979,       "1975–79",
  1980,         1984,       "1980–84",
  1985,         1989,       "1985–89",
  1990,         1994,       "1990–94",
  1995,         1999,       "1995–99",
  2000,         2004,       "2000–04",
  2005,         2009,       "2005–09",
  2010,         2014,       "2010–14"
) %>%
  mutate(chunk_name = ordered(fct_inorder(chunk_name)))

gpa.cum.plot <- gpa.data.clean %>%
  # Calculuate the number of GPAs in each year by active status
  group_by(start_year, Active) %>%
  summarise(num = n()) %>%
  ungroup() %>%
  # Join the year chunks to the summary table 
  mutate(temp = TRUE) %>%
  left_join(year.chunks %>% mutate(temp = TRUE), by="temp") %>%
  # Select only rows where the GPA start year is within the chunk
  filter(start_year >= chunk_start, start_year <= chunk_end) %>%
  select(-temp) %>%
  # Calculate number of GPAs in each chunk
  group_by(chunk_name, Active) %>%
  summarise(total = sum(num)) %>%
  # Calculate the cumulative sum of active GPAs
  group_by(Active) %>%
  mutate(cum_total = cumsum(total)) %>%
  ungroup() %>%
  # Plot the cumulative number of GPAs and the actual number of defunct GPAs
  mutate(plot_value = ifelse(Active == 1, cum_total, total)) %>%
  mutate(Active = factor(Active, levels=c(1, 0),
                         labels=c("Continuously in use   ", "Now defunct"),
                         ordered=TRUE))

#' *Source: Authors' database.*
#' 
#' Note: "Now defunct" denotes GPAs that appeared to be actively updated in
#' that year but were discontinued. Light grey bars represent GPAs that meet
#' our criteria and appear to be regularly updated as of 2012.
#' 
fig.cum.gpas <- ggplot(gpa.cum.plot, aes(x=chunk_name, y=plot_value, fill=Active)) +
  geom_bar(stat="identity", position="stack") +
  scale_fill_manual(values=c("grey70", "grey10"), name=NULL) +
  labs(x=NULL, y=NULL) +
  theme_gpa() + theme(legend.key.size=unit(0.65, "lines"),
                      legend.key=element_blank(), legend.margin=unit(0.25, "lines"))
fig.cum.gpas

fig.save.cairo(fig.cum.gpas, filename="figure-1-cumulative-gpas",
               width=5, height=3.5)


#' ## Figure 5: Pathways of GPA influence. 
#' 
#' *Source: Adapted from Kelley and Simmons, 2015.*
#' 
#' ![](`r file.path(PROJHOME, "Output")`/figure-5-pathways.png)
#' 
#' (located at `./Output/figure-5-pathways.pdf` and `./Output/figure-5-pathways.png`)
#' 
