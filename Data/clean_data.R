#' ---
#' title: 'Clean data for "Global Assessment Power in the Twenty-First Century"'
#' author: "Judith Kelley and Beth Simmons"
#' date: "`r format(Sys.time(), '%B %e, %Y')`"
#' ---

#+ message=FALSE
library(tidyverse)
library(readxl)
library(stringr)

# Load and clean data
gpa.data.raw <- read_excel(file.path(PROJHOME, "Data", "masterdata1.2.xlsx"),
                           sheet="main data sheet")

# Only use the first 30 columns. Need to use [,] indexing because dplyr::select
# chokes on all the duplicated NA column names
gpa.data.clean <- gpa.data.raw[, 2:31] %>%
  filter(!is.na(name)) %>%
  filter(duplicate != 1) %>%
  arrange(name) %>%
  mutate(gpa_id = row_number())


# Clean up issues
gpa.issues <- gpa.data.clean %>%
  select(gpa_id, `subject area`) %>%
  # Multiple GPA issues are split with a "/" (sometimes followed by a space);
  # split into a list column and then unnest that column
  mutate(subject_area = str_split(`subject area`, "/ *")) %>%
  unnest(subject_area) %>%
  # Clean up issue names
  mutate(subject_area = str_trim(tolower(subject_area))) %>%
  mutate(subject_area = case_when(
    .$subject_area == "economy" ~ "economic",
    .$subject_area == "economics" ~ "economic",
    .$subject_area == "environmen" ~ "environment",
    .$subject_area == "environmental" ~ "environment",
    .$subject_area == "envirionment" ~ "environment",
    .$subject_area == "goveranance" ~ "governance",
    .$subject_area == "goverance" ~ "governance",
    .$subject_area == "governanc" ~ "governance",
    .$subject_area == "governanc" ~ "governance",
    .$subject_area == "governence" ~ "governance",
    .$subject_area == "governace" ~ "governance",
    .$subject_area == "government" ~ "governance",
    TRUE ~ .$subject_area
  ))

gpa.issues.collapsed <- gpa.issues %>%
  # Collapse some categories
  mutate(subject_collapsed = recode(subject_area,
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
  filter(!is.na(subject_collapsed), subject_collapsed != "other") %>%
  # Combine all the subjects for a given GPA into a comma-separated list
  mutate(subject_collapsed = str_to_title(subject_collapsed)) %>%
  group_by(gpa_id) %>%
  mutate(subject_collapsed = paste(subject_collapsed, collapse=", ")) %>%
  # Get unique combinations of organizations and collapsed issues (for cases
  # where a collapsed category gets duplicated; like if the GPA did aid and
  # health, which are now both development)
  ungroup() %>%
  distinct(gpa_id, subject_collapsed)

gpa.issues.clean <- gpa.issues %>%
  # Combine all the subjects for a given GPA into a comma-separated list
  mutate(subject_area = str_to_title(subject_area)) %>%
  group_by(gpa_id) %>%
  mutate(subject_area = paste(subject_area, collapse=", ")) %>%
  mutate(subject_area = ifelse(subject_area == "NA", NA, subject_area)) %>%
  # Get unique combinations of organizations and collapsed issues (for cases
  # where a collapsed category gets duplicated; like if the GPA did aid and
  # health, which are now both development)
  ungroup() %>%
  distinct(gpa_id, subject_area)


# Clean up creator types
creator.types <- tribble(
  ~creator_type, ~creator_clean,        ~creator_collapsed,
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
  select(gpa_id, creator_type) %>%
  # Make NAs = 9, since that's the missing category
  mutate(creator_type = ifelse(is.na(creator_type), 9, creator_type)) %>%
  left_join(creator.types, by="creator_type")


# Clean up countries
countries <- tribble(
  ~country,                       ~country_clean,                 ~country_collapsed,         ~ ISO3,
  "Austraila",                    "Australia",                    "Other developed nations",  "AUS",
  "Australia/India",              "Australia, India",             "Other developed nations",  "AUS",
  "Austria",                      "Austria",                      "Europe",                   "AUT",
  "Belgium",                      "Belgium",                      "Europe",                   "BEL",
  "Canada",                       "Canada",                       "Other developed nations",  "CAN",
  "Ethiopia",                     "Ethiopia",                     "Global South",             "ETH",
  "Fiji",                         "Fiji",                         "Global South",             "FJI",
  "France",                       "France",                       "Europe",                   "FRA",
  "France/ USA",                  "France, USA",                  "Europe",                   "FRA",
  "Germany",                      "Germany",                      "Europe",                   "DEU",
  "Holland",                      "Netherlands",                  "Europe",                   "NLD",
  "international collaboration",  "International collaboration",  "Other developed nations",  NA,
  "Italy",                        "Italy",                        "Europe",                   "ITA",
  "Lithuania",                    "Lithuania",                    "Europe",                   "LTU",
  "Netherlands",                  "Netherlands",                  "Europe",                   "NLD",
  "Phillippines",                 "Philippines",                  "Global South",             "PHL",
  "Singapore",                    "Singapore",                    "Global South",             "SGP",
  "South Korea",                  "South Korea",                  "Other developed nations",  "KOR",
  "Spain",                        "Spain",                        "Europe",                   "ESP",
  "Switzerland",                  "Switzerland",                  "Europe",                   "CHE",
  "Uk",                           "United Kingdom",               "United Kingdom",           "GBR",
  "UK",                           "United Kingdom",               "United Kingdom",           "GBR",
  "United Kingdom",               "United Kingdom",               "United Kingdom",           "GBR",
  "United States",                "United States",                "United States",            "USA",
  "Unknown",                      NA,                             "Unknown",                  NA,
  "Uruguay",                      "Uruguay",                      "Global South",             "URY",
  "USA",                          "USA",                          "United States",            "USA"
)

gpa.countries <- gpa.data.clean %>%
  select(gpa_id, `country of origin`) %>%
  left_join(countries, by=c("country of origin" = "country"))


# Create final clean dataset
gpa.data.final <- gpa.data.clean %>%
  left_join(gpa.issues.clean, by="gpa_id") %>%
  left_join(gpa.issues.collapsed, by="gpa_id") %>%
  left_join(gpa.creators, by=c("creator_type", "gpa_id")) %>%
  left_join(gpa.countries, by=c("country of origin", "gpa_id")) %>%
  select(gpa_id, gpa_name = name, website,
         start_year, active = Active,
         subject_area, subject_collapsed,
         creator_name, creator_type, creator_clean, creator_collapsed,
         country = country_clean, country_collapsed, ISO3) %>%
  mutate_at(vars(gpa_id, start_year, active, creator_type), as.integer)

write_csv(gpa.data.final,
          path=file.path(PROJHOME, "Data",
                         "kelley_simmons_gpa_2015-10-04.csv"))
