library(tidyverse)
library(igraph)
library(tidygraph)
library(ggraph)
library(here)


create_graph_df_from_year <- function(year) {
  
  filename <- paste0("./data/processed_xml_bottom_up/processed_xml_", year, ".Rda")
  print(filename)
  load(filename)
  
  step1 <- year_xml %>% 
    mutate(CFR_cite = str_count(paragraph_text, "CFR"),
           CFR_ref_one = str_extract_all(paragraph_text, "\\d+ CFR part \\d+"),
           from = paste0("40 CFR part ", str_extract(part_number,"\\d+"))
    ) %>%
    filter(lengths(CFR_ref_one) > 0) %>%
    rename(to = CFR_ref_one) %>%
    select(from, to) %>%
    unnest() %>%
    group_by(from, to) %>%
    summarise(n = n()) %>%
    arrange(desc(n)) %>%
    ungroup() %>%
    mutate(from = if_else(str_detect(from, "40 CFR"), 
                          str_remove(from, "40 CFR"),
                          from),
           to = if_else(str_detect(to, "40 CFR"), 
                        str_remove(to, "40 CFR"),
                        to),
           CFR_year = year)
  
}

create_node_df_from_year <- function(year) {
  
  filename <- paste0("./data/processed_xml_bottom_up/processed_xml_", year, ".Rda")
  print(filename)
  load(filename)
  
  step1 <- year_xml %>%
    mutate(para_words = str_count(paragraph_text, boundary(type = "word")),
           from = paste0("40 CFR part ", str_extract(part_number,"\\d+")),
           from = if_else(str_detect(from, "40 CFR"), 
                          str_remove(from, "40 CFR"),
                          from)
    ) %>%
    group_by(from) %>%
    filter(!is.na(para_words)) %>%
    summarise(num_words = sum(para_words)) %>%
    ungroup() %>%
    mutate(CFR_year = year)
  
}

graph_df_all <- map_dfr(as.character(seq(2003, 2018)), create_graph_df_from_year)

max_n <- max(graph_df_all$n)

node_df_all <- map_dfr(as.character(seq(2003, 2018)), create_node_df_from_year)

max_words <- max(node_df_all$num_words)


graph_one_year <- function(year) {
  
  graph_this_year <- as_tbl_graph(subset(graph_df_all, CFR_year == year))
  
  nodes_this_year <- subset(node_df_all, CFR_year == year)
  
  graph_this_year <- graph_this_year %>%
    activate(nodes) %>% 
    left_join(nodes_this_year, by = c("name" = "from")) %>%
    mutate(num_words = if_else(is.na(num_words), as.integer(1), num_words))
  
  
  dg_this_year <- decompose.graph(graph_this_year, max.comps = 4)
  
  
  ggraph(dg_this_year[[1]], layout = "kk") +
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

graph_one_year("2003")

walk(as.character(seq(2003, 2018)), graph_one_year)
#  
# check_nodes <- node_df_all %>%
#   select(from, CFR_year) %>%
#   mutate(present = 1) %>%
#   pivot_wider(values_from = present, names_from = CFR_year)
# 
# count_nodes <- check_nodes %>%
#   select(-from) %>%
#   colSums(., na.rm = TRUE)

## dealing w/ "mega" graph--that is, all nodes and links ever

mega_graph_df <- graph_df_all %>%
  group_by(from, to) %>%
  summarize(n = sum(n))
  
mega_graph <- as_tbl_graph(mega_graph_df)


## set min.vertices to make sure I extract only giant component
dg_mega <- decompose.graph(mega_graph, min.vertices = 4)

nodes_mega <- dg_mega[[1]] %>% 
  as_tbl_graph %>% 
  activate(nodes) %>% 
  as_tibble()

coords_mega <- layout_with_graphopt(dg_mega[[1]])  

nodes_plus_coords_mega <- cbind(nodes_mega, coords_mega)

ggraph(dg_mega[[1]]) +
  geom_edge_link() +
  geom_node_point()
  


graph_df_all <- map_dfr(as.character(seq(2003, 2018)), create_graph_df_from_year)

max_n <- max(graph_df_all$n)

node_df_all <- map_dfr(as.character(seq(2003, 2018)), create_node_df_from_year)

max_words <- max(node_df_all$num_words)

node_df_2003 <- node_df_all %>% filter(CFR_year == "2003") %>%
  select(-CFR_year) %>%
  rename(num_words_2003 = num_words)

edge_df_2003 <- graph_df_all %>%
  filter(CFR_year == "2003") %>%
  select(-CFR_year) %>%
  rename(n_words_2003 = n)
  

graph_one_year2 <- function(year) {
  
  graph_this_year <- as_tbl_graph(subset(graph_df_all, CFR_year == year))
  
  dg_this_year <- decompose.graph(graph_this_year, max.comps = 4) 
  
  dg_this_year <- dg_this_year[[1]] %>%
    as_tbl_graph()
  
  node_df_this_year <- node_df_all %>% filter(CFR_year == year)
  
  nodes_this_year <- dg_this_year %>%
    activate(nodes) %>% 
    as_tibble() %>%
    mutate(present = TRUE) 
  
  coords_this_year <- nodes_plus_coords_mega %>%
    left_join(nodes_this_year) %>%
    filter(present == TRUE) %>%
    select(-present, -name) %>%
    rename(x = `1`, y = `2`) %>%
    identity()
  
  dg_this_year <- dg_this_year %>%
    activate(nodes) %>%
    left_join(node_df_this_year, by = c("name" = "from")) %>%
    mutate(num_words = if_else(is.na(num_words), as.integer(1), num_words)) %>%
    left_join(node_df_2003, by = c("name" = "from")) %>%
    mutate(num_words_2003 = if_else(is.na(num_words_2003), num_words, num_words_2003),
           weight = num_words/num_words_2003) %>%
    activate(edges) %>%
    left_join(edge_df_2003)
  
  

  ggraph(dg_this_year, layout = coords_this_year) +
    geom_edge_link0(aes(edge_width = n/max_n*5, alpha = n/max_n)) +
    geom_node_point(aes(size = weight, alpha = num_words/max_words)) +
    scale_size(range = c(0, 10)) +
    ggtitle(year) +
    theme_void()+ theme(legend.position = "none")


  graph_filename <- paste0("graph_", year, ".png")

  ggsave(here("graphs", graph_filename), height = 8, width = 11, units = "in")


}


walk(as.character(seq(2003, 2018)), graph_one_year2)



graph_one_year2_rel_wt <- function(year) {
  
  graph_this_year <- as_tbl_graph(subset(graph_df_all, CFR_year == year))
  
  dg_this_year <- decompose.graph(graph_this_year, max.comps = 4) 
  
  dg_this_year <- dg_this_year[[1]] %>%
    as_tbl_graph()
  
  node_df_this_year <- node_df_all %>% 
    filter(CFR_year == year)
  
  nodes_this_year <- dg_this_year %>%
    activate(nodes) %>% 
    as_tibble() %>%
    mutate(present = TRUE) 
  
  coords_this_year <- nodes_plus_coords_mega %>%
    left_join(nodes_this_year) %>%
    filter(present == TRUE) %>%
    select(-present, -name) %>%
    rename(x = `1`, y = `2`) %>%
    identity()
  
  dg_this_year <- dg_this_year %>%
    activate(nodes) %>%
    left_join(node_df_2003, by = c("name" = "from")) %>%
    mutate(num_words = if_else(is.na(num_words), as.integer(1), num_words)) 
  
  
  ggraph(dg_this_year, layout = coords_this_year) +
    geom_edge_link0(aes(edge_width = n/max_n*5, alpha = n/max_n)) +
    geom_node_point(aes(size = num_words/max_words*10, alpha = num_words/max_words)) +
    scale_size(range = c(0, 10)) +
    #geom_node_text(aes(label = if_else(degree(dg_this_year) >2,
    #                                   name,
    #                                   NA_character_)), repel = TRUE) +
    ggtitle(year) +
    theme_void()+ theme(legend.position = "none")
  
  
  graph_filename <- paste0("graph_", year, ".png")
  
  ggsave(here("graphs", graph_filename), height = 8, width = 11, units = "in")
  
  
}
