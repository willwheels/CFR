library(xml2)
library(tidyverse)

## https://blog.rstudio.com/2015/04/21/xml2/

## https://lecy.github.io/Open-Data-for-Nonprofit-Research/Quick_Guide_to_XML_in_R.html

## https://github.com/jennybc/manipulate-xml-with-purrr-dplyr-tidyr/blob/master/README.md


process_file <- function(filename) {
  
  paragraphs <- read_xml(filename) %>%
    xml_ns_strip() %>%
    xml_find_all(., ".//P") 
  
  all_ancestors <- map(paragraphs, xml_find_all, ".//ancestor::*")
  
  return_df <- tibble(row = seq_along(all_ancestors), nodeset = all_ancestors) %>%
    mutate(node_names = nodeset %>% map(~ xml_name(.))) %>%
    rowwise() %>%
    filter(any(c("CHAPTER", "SUBCHAPTER", "PART", "SUBPART", "SECTION") %in% node_names)) %>%
    mutate(chapter_position = ifelse(length(which(node_names == "CHAPTER")) > 0,
                                     which(node_names == "CHAPTER"),
                                     0),
           chapter_title = ifelse(chapter_position > 0,
                                  xml_text(xml_find_first(nodeset[chapter_position], ".//HD")),
                                  NA_character_),
           subchapter_position = ifelse(length(which(node_names == "SUBCHAP")) > 0,
                                        which(node_names == "SUBCHAP"),
                                        0),
           subchapter_title = ifelse(chapter_position > 0,
                                     xml_text(xml_find_first(nodeset[subchapter_position], ".//HD")),
                                     NA_character_),
           part_position = ifelse(length(which(node_names == "PART")) > 0,
                                  which(node_names == "PART"),
                                  0),
           part_number = ifelse(part_position > 0,
                                xml_text(xml_find_first(nodeset[part_position], ".//EAR")),
                                NA_character_),
           part_title = ifelse(part_position > 0,
                               paste0(xml_text(xml_find_first(nodeset[part_position], ".//HD")),
                                      xml_text(xml_find_first(nodeset[part_position], ".//RESERVED"))
                               ) %>% 
                                 str_remove("NA"),
                               NA_character_),
           subpart_position = ifelse(length(which(node_names == "SUBPART")) > 0,
                                     which(node_names == "SUBPART"),
                                     0),
           subpart_title = ifelse(subpart_position > 0,
                                  paste0(xml_text(xml_find_first(nodeset[subpart_position], ".//HD")),
                                         xml_text(xml_find_first(nodeset[subpart_position], ".//RESERVED"))
                                  ) %>% 
                                    str_remove("NA"),
                                  NA_character_),
           section_position = ifelse(length(which(node_names == "SECTION")) > 0,
                                     which(node_names == "SECTION"),
                                     0),
           section_number = ifelse(section_position > 0,
                                   xml_text(xml_find_first(nodeset[section_position], ".//SECTNO")),
                                   NA_character_),
           section_subject = ifelse(section_position > 0,
                                    xml_text(xml_find_first(nodeset[section_position], ".//SUBJECT")),
                                    NA_character_),
           paragraph_position = ifelse(length(which(node_names == "P")) > 0,
                                       which(node_names == "P"),
                                       0),
           
           paragraph_text = ifelse(paragraph_position > 0 ,
                                   xml_text(nodeset[paragraph_position]),
                                   NA_character_)
    ) %>%
    ungroup() %>%
    select(-nodeset, -node_names, -ends_with("position"))
  
  
 
}



# Start the clock!
ptm <- proc.time()

url <- "https://www.govinfo.gov/bulkdata/CFR/2018/title-40/CFR-2018-title40-vol34.xml"



test <- process_file(url)

# Stop the clock
proc.time() - ptm



test2 <- process_file(url)
