library(tidyverse)
library(stringr)
library(pander)

component.labels <- c("Government efficiency", "Government rule of law", "Government infrastructure", "Government access to markets", "Business efficiency", "Business performance", "Business freedom", "Individual well-being", "Individual innovation", "Individual freedom")

components <- read_csv(file.path(PROJHOME, "Data",
                                 "business_indices_components.csv")) %>%
  rowwise() %>%
  mutate(index = ifelse(is.na(short_name), index, 
                        str_interp("${index} (${short_name})"))) %>%
  select(index, start_year, components_clean) %>%
  ungroup() %>%
  separate_rows(components_clean, sep=", ") %>%
  mutate(dot = "•",
         components_clean = factor(components_clean, levels=component.labels, 
                                   ordered=TRUE)) %>%
  spread(components_clean, dot) %>%
  arrange(start_year) %>%
  rename(Index = index, `Start year` = start_year)

output <- pandoc.table.return(components, split.tables=Inf, missing="", 
                              justify=paste0(c("l", rep("c", ncol(components) - 1)),
                                             collapse=""),
                              caption="Ideological components of business indices")

cat(output, file=file.path(PROJHOME, "Output", 
                           "table_business_indicies_components.txt"))

Pandoc.convert(file.path(PROJHOME, "Output", 
                         "table_business_indicies_components.txt"),
               format="html", footer=FALSE, proc.time=FALSE, 
               options = "-s", open=FALSE)

Pandoc.convert(file.path(PROJHOME, "Output", 
                         "table_business_indicies_components.txt"),
               format="docx", open=FALSE)

write_csv(components, file.path(PROJHOME, "Output", 
                                "table_business_indicies_components.csv"), 
          na="")
