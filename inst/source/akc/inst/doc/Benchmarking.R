## ----include = FALSE----------------------------------------------------------
readRDS("benchmark_res.rds") -> res
knitr::opts_chunk$set(
  collapse = TRUE,
  eval = FALSE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
# library(akc)
# library(dplyr)

## -----------------------------------------------------------------------------
# bibli_data_table %>%
#   keyword_clean() %>%
#   keyword_merge() -> clean_data

## -----------------------------------------------------------------------------
# 100:300 -> topn_sample
# ls("package:akc") %>%
#   str_extract("^group.+") %>%
#   na.omit() %>%
#   setdiff(c("group_biconnected_component",
#             "group_components",
#             "group_optimal")) -> com_detect_fun_list

## ----eval=FALSE---------------------------------------------------------------
# all = tibble()
# for(i in com_detect_fun_list){
#     for(j in topn_sample){
#       system.time({
#         clean_data %>%
#           keyword_group(top = j,com_detect_fun = get(i)) %>%
#           as_tibble -> grouped_network_table
#       }) %>% na.omit-> time_info
#       grouped_network_table %>% nrow -> node_no
#       grouped_network_table %>% distinct(group) %>% nrow -> group_no
#       grouped_network_table %>%
#         count(group) %>%
#         summarise(mean(n)) %>%
#         .[[1]] -> group_avg_node_no
#       grouped_network_table %>%
#         count(group) %>%
#         summarise(sd(n)) %>%
#         .[[1]] -> group_sd_node_no
#       c(com_detect_fun = i,
#         topn = j,
#         node_no = node_no,group_no = group_no,
#         avg = group_avg_node_no,
#         sd = group_sd_node_no,time_info[1:3]) %>%
#         bind_rows(all,.) -> all
#     }
# }
# 
# res = all %>%
#   mutate_at(2:9,function(x) as.numeric(x) %>% round(2)) %>%
#   distinct(com_detect_fun,node_no,.keep_all = T) %>%
#   select(-topn,-contains("self")) %>%
#   setNames(c("com_detect_fun","No. of total nodes","No. of total groups",
#              "Average node number in each group","Standard deviation of node number",
#              "Computer running time for keyword_group function"))

## ----eval=TRUE----------------------------------------------------------------
knitr::kable(res)

## ----eval=TRUE----------------------------------------------------------------
sessionInfo()

