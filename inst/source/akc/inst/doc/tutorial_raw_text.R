## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(akc)
library(dplyr)

## -----------------------------------------------------------------------------
bibli_data_table

## -----------------------------------------------------------------------------
bibli_data_table %>%
  keyword_clean() %>%
  pull(keyword) %>%
  make_dict() -> my_dict

## -----------------------------------------------------------------------------
# get stop words from `tidytext` package
tidytext::stop_words %>%
    pull(word) %>%
    unique() -> my_stopword
  
bibli_data_table %>%
    keyword_extract(id = "id",text = "abstract",
    dict = my_dict,stopword = my_stopword) -> extracted_keywords


## -----------------------------------------------------------------------------
extracted_keywords %>% 
  keyword_merge() -> merged_keywords

## -----------------------------------------------------------------------------
merged_keywords %>% 
  keyword_group() -> grouped_keywords

## -----------------------------------------------------------------------------
grouped_keywords %>% 
  as_tibble()

## -----------------------------------------------------------------------------
grouped_keywords %>% 
  keyword_table()

## -----------------------------------------------------------------------------
grouped_keywords %>% 
  keyword_cloud()

## -----------------------------------------------------------------------------
grouped_keywords %>% 
  keyword_cloud(group_no = 1)

## -----------------------------------------------------------------------------
grouped_keywords %>% 
  keyword_network()

## -----------------------------------------------------------------------------
grouped_keywords %>% 
  keyword_network(group_no = 2,max_nodes = 20)

