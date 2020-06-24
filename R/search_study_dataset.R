
#install_packages
#install.packages("revtools")
#install.packages("DBI")
#install.packages("agoldst/mlaibr")


#install revtools
#library(revtools)
#library(DBI)
#library(dplyr)
#library(tidyverse)
#library(agoldst/mlaibr)

#to do: 
# summarize dataset to table that includes search function.
# add row for first and second screening to indicate the flow of each individual record.


file_location <- system.file("extdata", 
  "data.raw/2020april.comple_search_flow_deduplicated_tab.txt",
  package = revtools)
x <- read_bibliography(file_location)
class(x) # = data.frame


y <- KDViz::ReadRIS(file_location)

as_tibble(x)

q3 <- x %>%
  select(author, year, title, abstract) %>%
  filter(str_detect(author, "Smeets")) %>%
           top_n(40, author)

  summarise(
    subscribe = sum(ifelse(author %in% "Smeets", 1, 0)),
    total = n())
show_query(q1)