library(tidyverse)
library(tidygraph)
library(ggraph)
library(here)


create_edge_node_df_from_year <- function(year) {
  
  filename <- paste0("./data/processed_xml_bottom_up/processed_xml_", year, ".Rda")
  print(filename)
  load(filename)
  
  edge_df <- year_xml %>% 
    mutate(para_len = str_length(paragraph_text), 
           
           incorporation = str_detect(section_subject, "Incorporation")*1,
           CFR_cite = str_count(paragraph_text, "CFR"),
           CFR_ref_one = str_extract_all(paragraph_text, "\\d+ CFR part \\d+"),
           from = paste0("40 CFR part ", str_extract(part_number,"\\d+"))
    ) %>%
    filter(lengths(CFR_ref_one) > 0) %>%
    rename(to = CFR_ref_one) %>%
    select(from, to) %>%
    unnest() %>%
    group_by(from, to) %>%
    count(sort = TRUE) %>%
    ungroup() %>%
    mutate(from = if_else(str_detect(from, "40 CFR"), 
                          str_remove(from, "40 CFR"),
                          from),
           to = if_else(str_detect(to, "40 CFR"), 
                        str_remove(to, "40 CFR"),
                        to),
           from = trimws(from), to = trimws(to),
           CFR_year = year) 
  
  node_df <- year_xml %>%
    mutate(para_words = str_count(paragraph_text, boundary(type = "word")),
           name = paste0("40 CFR part ", str_extract(part_number,"\\d+")),
           name = if_else(str_detect(name, "40 CFR"), 
                          str_remove(name, "40 CFR"),
                          name),
           name = trimws(name)
    ) %>%
    group_by(name) %>%
    filter(!is.na(para_words)) %>%
    summarise(num_words = sum(para_words)) %>%
    ungroup() %>%
    mutate(CFR_year = year)
  
  return(list(edge_df, node_df))
}


graph_df_all <- map(as.character(seq(2003, 2018)), create_edge_node_df_from_year)

edge_df_all <- map_dfr(graph_df_all, 1)

node_df_all <- map_dfr(graph_df_all, 2)

node_df_2003 <- node_df_all %>%
  filter(CFR_year == "2003") %>%
  select(-CFR_year) %>%
  rename(num_words_2003 = num_words)

graph_one_year <- function(year) {
  
  node_df_this_year <- node_df_all %>%
    filter(CFR_year == year) %>%
    left_join(node_df_2003) %>%
    mutate(num_words_wt = num_words/num_words_2003,
           num_words_wt = replace_na(num_words_wt, 1)) %>%
    select(name, num_words_wt)
    
  
  graph_this_year <- as_tbl_graph(subset(edge_df_all, CFR_year == year)) %>%
    activate(nodes) %>%
    left_join(node_df_this_year) %>%
    identity()
  
}


graph_2003 <- graph_one_year("2003")


graph_2018 <- graph_one_year("2018")

ggraph(decompose.graph(graph_2018)[[1]], layout = "graphopt") +
  geom_edge_link() +
  geom_node_point() +
  theme_void()


ggraph(decompose.graph(graph_2003)[[1]], layout = "graphopt") +
  geom_edge_link() +
  geom_node_point() +
  theme_void()

graph_vis_one_year <- function(graph_this_year) {
  
  
  #dg_this_year <- decompose.graph(graph_this_year, max.comps = 4)
  
  
  ggraph(graph_this_year, layout = "kk") +
    geom_edge_link0(aes(edge_width = n/max_n*5, alpha = n/max_n)) +
    geom_node_point(aes(size = num_words/max_words*10, alpha = num_words/max_words)) +
    scale_size(range = c(0, 10)) +
    #geom_node_text(aes(label = if_else(degree(dg_this_year[[1]]) >2,
    #                                   name,
    #                                   NA_character_)), repel = TRUE) +
    ggtitle(year) +
    theme_void()+ theme(legend.position = "none")
  
  
  graph_filename <- paste0("graph_", year, ".png")
  
  ggsave(here("graphs", graph_filename), height = 8, width = 11, units = "in")
  
}


          