#' ---
#' title: 'Figures for "Global Assessment Power in the Twenty-First Century"'
#' author: "Judith Kelley and Beth Simmons"
#' date: "`r format(Sys.time(), '%B %e, %Y')`"
#' ---

#+ message=FALSE
library(tidyverse)
library(readxl)
library(forcats)
library(stringr)
library(Cairo)
library(ggstance)


# Useful functions
theme_gpa <- function(base_size=9, base_family="Clear Sans Light") {
  update_geom_defaults("bar", list(fill = "grey30"))
  update_geom_defaults("line", list(colour = "grey30"))
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
gpa.data.clean <- gpa.data.raw[, 2:30]

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
  scale_fill_manual(values=c("grey70", "grey30"), name=NULL) +
  labs(x=NULL, y=NULL) +
  theme_gpa() + theme(legend.key.size=unit(0.65, "lines"),
                      legend.key=element_blank(), legend.margin=unit(0.25, "lines"))
fig.cum.gpas

fig.save.cairo(fig.cum.gpas, filename="figure-1-cumulative-gpas",
               width=5, height=2.5)


#' ## Figure 2: Number of GPAs, by issue.
gpa.issues <- gpa.data.clean %>%
  # Only look at active GPAs
  dplyr::filter(Active == 1) %>%
  # Multiple GPA issues are split with a "/" (sometimes followed by a space);
  # split into a list column and then unnest that column
  mutate(issue = str_split(`subject area`, "/ *")) %>%
  unnest(issue) %>%
  # Clean up issue names
  mutate(issue = str_trim(tolower(issue))) %>%
  mutate(issue = case_when(
    .$issue == "economy" ~ "economic",
    .$issue == "economics" ~ "economic",
    .$issue == "environmen" ~ "environment",
    .$issue == "envirionment" ~ "environment",
    .$issue == "goveranance" ~ "governance",
    .$issue == "goverance" ~ "governance",
    .$issue == "governanc" ~ "governance",
    .$issue == "governanc" ~ "governance",
    .$issue == "governence" ~ "governance",
    .$issue == "governace" ~ "governance",
    .$issue == "government" ~ "governance",
    TRUE ~ .$issue
  )) %>%
  # Collapse some categories
  mutate(issue_collapsed = recode(issue,
                                  conflict = "security",
                                  military = "security",
                                  aid = "development",
                                  health = "development",
                                  energy = "development",
                                  tourism = "development",
                                  education = "development",
                                  trade = "trade & finance",
                                  finance = "trade & finance",
                                  technology = "trade & finance",
                                  `press freedom` = "human rights",
                                  religion = "human rights",
                                  gender = "human rights",
                                  `intellectual property rights` = "legal",
                                  privacy = "legal")) %>%
  filter(!is.na(issue_collapsed), issue_collapsed != "other") %>%
  # Get unique combinations of organizations and collapsed issues (for cases
  # where a collapsed category gets duplicated; like if the GPA did aid and
  # health, which are now both development)
  distinct(name, issue_collapsed) %>%
  # Get a count of each collapsed issue
  group_by(issue_collapsed) %>%
  summarise(issue_count = n()) %>%
  ungroup() %>%
  arrange(issue_count) %>%
  mutate(issue_collapsed = str_to_title(issue_collapsed)) %>%
  mutate(issue_collapsed = fct_inorder(issue_collapsed))

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
#' N = `r length(unique(gpa.data.clean$name))` unique GPAs.
#' 
fig.by.issue <- ggplot(gpa.issues, aes(x=issue_count, y=issue_collapsed)) + 
  geom_barh(stat="identity") + 
  labs(x=NULL, y=NULL) +
  theme_gpa()
fig.by.issue

fig.save.cairo(fig.by.issue, filename="figure-2-gpas-by-issue",
               width=5, height=3.5)


#' ## Figure 3: GPA creators, by type.
creator.types <- tribble(
  ~creator_type, ~creator_codebook,   ~creator_clean,
  1,             "University",          "University",
  2,             "National government", "National government",
  3,             "NGO",                 "NGO",
  4,             "Private",             "Private",
  5,             "IGO",                 "IGO",
  6,             "NGO/university",      "Overlapping or unknown",
  7,             "IGO/university",      "Overlapping or unknown",
  8,             "NGO/country agency",  "Overlapping or unknown",
  9,             "Missing or other",    "Overlapping or unknown"
)

gpa.creators <- gpa.data.clean %>%
  # Make NAs = 9, since that's the missing category
  mutate(creator_type = ifelse(is.na(creator_type), 9, creator_type)) %>%
  # Bring in clean creator names
  left_join(creator.types, by="creator_type") %>%
  # Get count of GPAs by creator type
  group_by(creator_clean) %>%
  summarise(num = n()) %>%
  arrange(num) %>%
  mutate(creator_clean = fct_inorder(creator_clean))

#' *Source: Authors' database.*
#' 
#' N = `r sum(gpa.creators$num)`.
#' 
fig.by.creator <- ggplot(gpa.creators, aes(x=num, y=creator_clean)) +
  geom_barh(stat="identity") +
  labs(x=NULL, y=NULL) +
  theme_gpa()
fig.by.creator

fig.save.cairo(fig.by.creator, filename="figure-3-gpas-by-creator",
               width=5, height=2.5)


#' ## Figure 4: Country of GPA source headquarters.
countries <- tribble(
  ~country,                       ~country_clean,
  "Austraila",                    "Other developed nations",
  "Australia/India",              "Other developed nations",
  "Austria",                      "Europe",
  "Belgium",                      "Europe",
  "Canada",                       "Other developed nations",
  "Ethiopia",                     "Global South",
  "Fiji",                         "Global South",
  "France",                       "Europe",
  "France/ USA",                  "Europe",
  "Germany",                      "Europe",
  "Holland",                      "Europe",
  "international collaboration",  "Other developed nations",
  "Italy",                        "Europe",
  "Lithuania",                    "Europe",
  "Netherlands",                  "Europe",
  "Phillippines",                 "Global South",
  "Singapore",                    "Global South",
  "South Korea",                  "Other developed nations",
  "Spain",                        "Europe",
  "Switzerland",                  "Europe",
  "Uk",                           "United Kingdom",
  "UK",                           "United Kingdom",
  "United Kingdom",               "United Kingdom",
  "United States",                "United States",
  "Unknown",                      "Unknown",
  "Uruguay",                      "Global South",
  "USA",                          "United States"
)

gpa.countries <- gpa.data.clean %>%
  # Bring in clean and consolidated country names
  left_join(countries, by=c("country of origin" = "country")) %>%
  # Get a count of each creating country
  group_by(country_clean) %>%
  summarise(num = n()) %>%
  filter(!is.na(country_clean), country_clean != "Unknown") %>%
  arrange(num) %>%
  mutate(country_clean = fct_inorder(country_clean))

#' *Source: Authors' database.*
#' 
#' N = `r sum(gpa.countries$num)`.
#' 
fig.by.country <- ggplot(gpa.countries, aes(x=num, y=country_clean)) +
  geom_barh(stat="identity") +
  labs(x=NULL, y=NULL) +
  theme_gpa()
fig.by.country

fig.save.cairo(fig.by.country, filename="figure-4-gpas-by-country",
               width=5, height=2.5)


#' ## Figure 5: Pathways of GPA influence. 
#' 
#' *Source: Adapted from Kelley and Simmons, 2015.*
#' 
#' ![](`r file.path(PROJHOME, "Output")`/figure-5-pathways.png)
#' 
#' (located at `./Output/figure-5-pathways.pdf` and `./Output/figure-5-pathways.png`)
#' 
