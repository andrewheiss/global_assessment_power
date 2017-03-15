#' ---
#' title: 'Figures for "Global Assessment Power in the Twenty-First Century"'
#' author: "Judith Kelley and Beth Simmons"
#' date: "`r format(Sys.time(), '%B %e, %Y')`"
#' ---

#' ## Data and code
#' 
#' Replication data and code available at
#' [GitHub](https://github.com/andrewheiss/global_assessment_power).
#' 

#+ message=FALSE
library(tidyverse)
library(forcats)
library(stringr)
library(Cairo)
library(ggstance)
library(ggforce)
library(maptools)
library(rgdal)

# Useful functions
theme_gpa <- function(base_size=10, base_family="Clear Sans") {
  update_geom_defaults("bar", list(fill = "grey30"))
  update_geom_defaults("line", list(colour = "grey30"))
  update_geom_defaults("label", list(family="Clear Sans"))
  update_geom_defaults("text", list(family="Clear Sans"))
  ret <- theme_bw(base_size, base_family) + 
    theme(panel.background = element_rect(fill="#ffffff", colour=NA),
          axis.title.y = element_text(margin = margin(r = 10)),
          axis.title.x = element_text(margin = margin(t = 10)),
          axis.text = element_text(colour="black"),
          title=element_text(vjust=1.2, family="Clear Sans", face="bold"),
          plot.subtitle=element_text(family="Clear Sans"),
          plot.caption=element_text(family="Clear Sans",
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
          panel.spacing.y=unit(1.5, "lines"),
          legend.key=element_blank(),
          legend.spacing=unit(0.2, "lines"))
  
  ret
}

theme_blank_map <- function(base_size=9.5, base_family="Clear Sans") {
  ret <- theme_bw(base_size, base_family) + 
    theme(panel.background = element_rect(fill="#ffffff", colour=NA),
          panel.border=element_blank(), axis.line=element_blank(),
          panel.grid=element_blank(), axis.ticks=element_blank(),
          axis.title=element_blank(), axis.text=element_blank())
  ret
}

fig.save.cairo <- function(fig, filepath=file.path(PROJHOME, "Output"), 
                           filename, width, height, units="in", seed=NULL, ...) {
  if (!is.null(seed)) set.seed(seed)
  ggsave(fig, filename=file.path(filepath, paste0(filename, ".pdf")),
         width=width, height=height, units=units, device=cairo_pdf, ...)
  if (!is.null(seed)) set.seed(seed)
  ggsave(fig, filename=file.path(filepath, paste0(filename, ".png")),
         width=width, height=height, units=units, type="cairo", dpi=300, ...)
}

# Clean up the interval labels created by cut()
# Use with forcats::fct_relabel
clean.cut.range <- function(x) {
  # If the level starts with "(", strip all the ( and [s and add 1 to the first value
  need.to.clean <- str_detect(x, "^\\(")
  cleaned <- str_replace_all(x[need.to.clean], "\\(|\\]", "") %>% 
    map_chr(function(x) {
      x.split <- as.numeric(str_split(x, ",", simplify=TRUE))
      paste0(x.split[1] + 1, "–", x.split[2])
    })
}


# Load clean data
#+ message=FALSE
gpa.data.clean <- read_csv(file.path(PROJHOME, "Data",
                                     "kelley_simmons_gpa_2017-03-08.csv"))


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
  group_by(start_year, active) %>%
  summarise(num = n()) %>%
  ungroup() %>%
  # Join the year chunks to the summary table 
  mutate(temp = TRUE) %>%
  left_join(year.chunks %>% mutate(temp = TRUE), by="temp") %>%
  # Select only rows where the GPA start year is within the chunk
  filter(start_year >= chunk_start, start_year <= chunk_end) %>%
  select(-temp) %>%
  # Calculate number of GPAs in each chunk
  group_by(chunk_name, active) %>%
  summarise(total = sum(num)) %>%
  # Calculate the cumulative sum of active GPAs
  group_by(active) %>%
  mutate(cum_total = cumsum(total)) %>%
  ungroup() %>%
  # Plot the cumulative number of GPAs and the actual number of defunct GPAs
  mutate(plot_value = ifelse(active == 1, cum_total, total)) %>%
  mutate(active = factor(active, levels=c(1, 0),
                         labels=c("Continuously in use   ", "Now defunct"),
                         ordered=TRUE))

gpas.active <- gpa.data.clean %>% filter(active == 1) %>% nrow
gpas.defunct <- gpa.data.clean %>% filter(active == 0) %>% nrow

#' *Source: Authors' database.*
#' 
#' Note: "Now defunct" denotes GPAs that appeared to be actively updated in
#' that year but were discontinued. Light grey bars represent GPAs that meet
#' our criteria and appear to be regularly updated as of 2012.
#' 
#' N = `r gpas.active` active GPAs; `r gpas.defunct` defunct GPAs.
#' 
#+ fig.width=5, fig.height=2.5
fig.cum.gpas <- ggplot(gpa.cum.plot, aes(x=chunk_name, y=plot_value, fill=active)) +
  geom_col(position="stack") +
  scale_fill_manual(values=c("grey70", "grey30"), name=NULL) +
  labs(x=NULL, y=NULL) +
  theme_gpa(9) + theme(legend.key.size=unit(0.65, "lines"),
                       legend.key=element_blank(), legend.spacing=unit(0.25, "lines"),
                       panel.grid.major.x=element_blank())
fig.cum.gpas

fig.save.cairo(fig.cum.gpas, filename="figure-1-cumulative-gpas",
               width=5, height=2.5)


#' ## Figure 2 (new): Number of GPAs, by issue and creator type.
gpa.issues.creators <- gpa.data.clean %>%
  filter(active == 1, !is.na(subject_collapsed), !is.na(creator_collapsed)) %>%
  mutate(subject_collapsed = str_split(subject_collapsed, ", ")) %>%
  unnest(subject_collapsed) %>%
  group_by(creator_collapsed, subject_collapsed) %>%
  summarise(num = n()) %>%
  group_by(subject_collapsed) %>%
  mutate(total_in_group = sum(num)) %>%
  ungroup() %>%
  complete(subject_collapsed, creator_collapsed, fill=list(num = 0)) %>%
  arrange(total_in_group, num) %>%
  mutate(creator_collapsed = fct_relevel(creator_collapsed,
                                         c("Other", "University or Private",
                                           "State", "IGO", "NGO"))) %>%
  mutate(subject_collapsed = fct_inorder(subject_collapsed))

issue.creator.denominator <- gpa.data.clean %>% 
  filter(active == 1, !is.na(creator_collapsed)) %>% 
  nrow

#' *Source: Authors' database.* 
#' 
#' Note: Includes only "active" GPAs as of 2012; excludes defunct cases. Note
#' that the total count of GPAs is larger than in Figure 1 because we have
#' double counted cases that straddle issue areas, such as health and
#' development. Overlapping and unknown creators are ommitted. 
#' 
#' Several categories collapse the following subcategories: security =
#' conflict, military; development = development, aid, health, energy, tourism,
#' education; trade & finance = trade, financy, education; human rights = human
#' rights, press freedom, religion, gender; legal = legal, intellectual 
#' property rights, privacy.
#' 
#' N = `r issue.creator.denominator`.
#' 
#+ fig.width=5, fig.height=3.5
fig.by.issue.creator <- ggplot(gpa.issues.creators,
                               aes(x=num, y=subject_collapsed, fill=creator_collapsed)) + 
  geom_barh(stat="identity", position=position_stackv()) + 
  scale_x_continuous(sec.axis = sec_axis(~ . / issue.creator.denominator,
                                         labels=scales::percent)) +
  # scale_fill_grey(start=0.9, end=0.2) +
  scale_fill_manual(values=rev(c("black", "grey50", "grey70", "grey25", "grey90"))) +
  guides(fill=guide_legend(reverse=TRUE, title=NULL, nrow=1)) +
  labs(x=NULL, y=NULL) +
  theme_gpa() + theme(panel.grid.major.y=element_blank())
fig.by.issue.creator

fig.save.cairo(fig.by.issue.creator, filename="figure-2-gpas-by-issue-creator",
               width=5, height=3.5)


#' ## Figure 2: Number of GPAs, by issue.
gpa.issues <- gpa.data.clean %>%
  filter(active == 1, !is.na(subject_collapsed)) %>%
  mutate(subject_collapsed = str_split(subject_collapsed, ", ")) %>%
  unnest(subject_collapsed) %>%
  # Get a count of each collapsed issue
  group_by(subject_collapsed) %>%
  summarise(issue_count = n()) %>%
  ungroup() %>%
  arrange(issue_count) %>%
  mutate(subject_collapsed = fct_inorder(subject_collapsed))

issue.denominator <- gpa.data.clean %>%
  filter(active == 1, !is.na(subject_collapsed)) %>%
  nrow

#' *Source: Authors' database.* 
#' 
#' Note: Includes only "active" GPAs as of 2012; excludes defunct cases. Note
#' that the total count of GPAs is larger than in Figure 1 because we have
#' double counted cases that straddle issue areas, such as health and
#' development.
#' 
#' Several categories collapse the following subcategories: security =
#' conflict, military; development = development, aid, health, energy, tourism,
#' education; trade & finance = trade, financy, education; human rights = human
#' rights, press freedom, religion, gender; legal = legal, intellectual 
#' property rights, privacy.
#' 
#' N = `r issue.denominator` active GPAs.
#' 
#+ fig.width=5, fig.height=3.5
fig.by.issue <- ggplot(gpa.issues, aes(x=issue_count, y=subject_collapsed)) + 
  geom_barh(stat="identity") + 
  scale_x_continuous(sec.axis = sec_axis(~ . / issue.denominator,
                                         labels=scales::percent)) +
  labs(x=NULL, y=NULL) +
  theme_gpa() + theme(panel.grid.major.y=element_blank())
fig.by.issue

fig.save.cairo(fig.by.issue, filename="figure-2-gpas-by-issue",
               width=5, height=3.5)


#' ## Figure 3: GPA creators, by type.
gpa.creators <- gpa.data.clean %>%
  filter(!is.na(creator_collapsed)) %>%
  group_by(creator_collapsed) %>%
  summarise(num = n()) %>%
  arrange(num) %>%
  mutate(creator_collapsed = str_replace(creator_collapsed, 
                                         "ping or", "ping\nor")) %>%
  mutate(creator_collapsed = fct_inorder(creator_collapsed))

creator.denominator <- gpa.data.clean %>% 
  filter(!is.na(creator_collapsed)) %>% 
  nrow

#' *Source: Authors' database.*
#' 
#' N = `r creator.denominator`.
#' 
#+ fig.width=5, fig.height=2.5
fig.by.creator <- ggplot(gpa.creators, aes(x=num, y=creator_collapsed)) +
  geom_barh(stat="identity") +
  scale_x_continuous(sec.axis = sec_axis(~ . / creator.denominator,
                                         labels=scales::percent)) +
  labs(x=NULL, y=NULL) +
  theme_gpa() + theme(panel.grid.major.y=element_blank())
fig.by.creator

fig.save.cairo(fig.by.creator, filename="figure-3-gpas-by-creator",
               width=5, height=2.5)


#' ## Figure 4: Country of GPA source headquarters.
gpa.countries <- gpa.data.clean %>%
  group_by(country_collapsed) %>%
  summarise(num = n()) %>%
  filter(!is.na(country_collapsed), country_collapsed != "Unknown") %>%
  arrange(num) %>%
  mutate(country_collapsed = fct_inorder(country_collapsed))

country.denominator <- sum(gpa.countries$num)

#' *Source: Authors' database.*
#' 
#' N = `r country.denominator`.
#' 
#+ fig.width=5, fig.height=2.5
fig.by.country <- ggplot(gpa.countries, aes(x=num, y=country_collapsed)) +
  geom_barh(stat="identity") +
  scale_x_continuous(sec.axis = sec_axis(~ . / country.denominator,
                                         labels=scales::percent)) +
  labs(x=NULL, y=NULL) +
  theme_gpa() + theme(panel.grid.major.y=element_blank())
fig.by.country

fig.save.cairo(fig.by.country, filename="figure-4-gpas-by-country",
               width=5, height=2.5)


#' ## Figure 4: Country of GPA source headquarters (maps).
# Load map data
if (!file.exists(file.path(PROJHOME, "Data", "map_data", 
                           "ne_110m_admin_0_countries.VERSION.txt"))) {
  map.url <- paste0("http://www.naturalearthdata.com/", 
                    "http//www.naturalearthdata.com/download/110m/cultural/", 
                    "ne_110m_admin_0_countries.zip")
  map.tmp <- file.path(PROJHOME, "Data", basename(map.url))
  download.file(map.url, map.tmp)
  unzip(map.tmp, exdir=file.path(PROJHOME, "Data", "map_data"))
  unlink(map.tmp)
}

#+ message=FALSE, warning=FALSE
countries.map <- readOGR(file.path(PROJHOME, "Data", "map_data"), 
                         "ne_110m_admin_0_countries",
                         verbose=FALSE)
countries.robinson <- spTransform(countries.map, CRS("+proj=robin"))
countries.ggmap <- fortify(countries.robinson, region="iso_a3") %>%
  filter(!(id %in% c("ATA", -99))) %>%  # Get rid of Antarctica and NAs
  mutate(id = ifelse(id == "GRL", "DNK", id))  # Greenland is part of Denmark

possible.countries <- data_frame(ISO3 = unique(as.character(countries.ggmap$id)))

gpa.countries.map <- gpa.data.clean %>%
  group_by(ISO3) %>%
  summarise(num = n()) %>%
  filter(!is.na(ISO3)) %>%
  # Bring in full list of countries and create new variable indicating if the
  # country has any GPAs
  right_join(possible.countries, by="ISO3") %>%
  mutate(num.bin = !is.na(num),
         num.ceiling = ifelse(num >= 10, 10, num))

#' *Source: Authors' data*
#' 
#' N = `r sum(gpa.countries.map$num.bin)` countries.
#' 

#+ warning=FALSE, fig.width=5, fig.height=4
gpa.map.bin <- ggplot(gpa.countries.map, aes(fill=num.bin, map_id=ISO3)) +
  geom_map(map=countries.ggmap, size=0.15, colour="black") + 
  expand_limits(x=countries.ggmap$long, y=countries.ggmap$lat) +
  coord_equal() +
  scale_fill_manual(values = c("white", "grey30"), guide=FALSE) + 
  theme_blank_map()
gpa.map.bin

fig.save.cairo(gpa.map.bin, filename="figure-4-gpas-by-country-map-bin",
               width=5, height=4)

#+ warning=FALSE, fig.width=5, fig.height=4
gpa.map <- ggplot(gpa.countries.map, aes(fill=num.ceiling, map_id=ISO3)) +
  geom_map(map=countries.ggmap, size=0.15, colour="black") + 
  expand_limits(x=countries.ggmap$long, y=countries.ggmap$lat) + 
  coord_equal() +
  scale_fill_gradient(low="grey90", high="grey30", breaks=seq(2, 10, 2), 
                      labels=c(paste(seq(2, 8, 2), "  "), "10+"),
                      na.value="white", name="GPAs based in country",
                      guide=guide_colourbar(ticks=FALSE, barwidth=5)) + 
  theme_blank_map(9) +
  theme(legend.position="bottom", legend.key.size=unit(0.65, "lines"),
        strip.background=element_rect(colour="#FFFFFF", fill="#FFFFFF"))
gpa.map

fig.save.cairo(gpa.map, filename="figure-4-gpas-by-country-map",
               width=5, height=4)


#' ## Figure 5: Pathways of GPA influence. 
#' 
#' *Source: Adapted from Kelley and Simmons, 2015.*
#' 
#' ![](http://stats.andrewheiss.com/judith/gpas/Output/figure-5-pathways.png)
#' 
#' (located at `./Output/figure-5-pathways.pdf` and `./Output/figure-5-pathways.png`)
#' 


#' ## Figure X: Pseudo-networks of indicator creators over time
#' 
#' *Source: Authors' data*
#' 
gpa.data.clean.creator.long <- gpa.data.clean %>%
  separate_rows(subject_collapsed, sep=",") %>%
  mutate(subject_collapsed = str_trim(subject_collapsed))

subject.counts <- gpa.data.clean.creator.long %>%
  count(subject_collapsed) %>%
  arrange(desc(n))

gpa.creator.cumulative <- gpa.data.clean.creator.long %>%
  filter(!is.na(creator_collapsed)) %>%
  # Infer death year based on most recent year if not active
  mutate(end_year = ifelse(active == 0, recent_year, 2015)) %>%
  # Create list of years, like 1998:2005
  mutate(year = map2(start_year, end_year, ~ seq(.x, .y))) %>%
  # Unnest list of years
  unnest(year) %>%
  filter(year > 1970) %>%
  # Divide years into five-year chunks
  mutate(pentad = cut(year, breaks=seq(1970, 2015, 5), ordered_result=TRUE),
         pentad = fct_relabel(pentad, clean.cut.range)) %>%
  # Only select the first year in the pentad. Without this, indexes appear up
  # to five times in the pentad.
  group_by(pentad, gpa_id) %>%
  slice(1) %>%
  ungroup() %>%
  left_join(subject.counts, by="subject_collapsed") %>%
  arrange(pentad, desc(n)) %>%
  mutate(subject_collapsed = ordered(fct_inorder(subject_collapsed)),
         subject_collapsed = fct_recode(subject_collapsed, 
                                        `Human rights & Gender` = "Human Rights",
                                        `Privacy & Legal issues` = "Legal"),
         creator_collapsed = ordered(fct_relevel(creator_collapsed,
                                                 "NGO", "IGO", "State",
                                                 "University or Private",
                                                 "Other")))

#+ fig.width=7, fig.height=5
gpa.type.points <- ggplot(gpa.creator.cumulative, 
                          aes(x=pentad, y=subject_collapsed,
                              shape=creator_collapsed)) + 
  # geom_point(size=0.75, alpha=0.75, position=position_jitter(width=0.25, height=0.30)) +
  geom_point(size=0.75, alpha=0.6, position=position_jitternormal(sd_x=0.1, sd_y=0.16)) +
  labs(x=NULL, y=NULL) +
  guides(shape=guide_legend(title=NULL, override.aes=list(size=3))) +
  scale_shape_manual(values=c(16, 21, 17, 24, 3)) +
  theme_gpa() +
  theme(axis.text.x=element_text(angle=45, hjust=0.5, vjust=0.5),
        panel.grid.major=element_blank())
set.seed(12345); gpa.type.points

fig.save.cairo(gpa.type.points, filename="figure-x-indicator-creator-points",
               width=7, height=5, seed=12345)
