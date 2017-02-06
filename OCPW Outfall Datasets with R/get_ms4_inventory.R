library(tidyr)
library(dplyr)
library(readr)
library(stringr)
library(httr)
library(jsonlite)

# Function pulls current ms4 outfall inventory from feature service rest url
get_ms4_inventory <- function(){
  
  # Request outfall facility location information and attributes
  response <- GET('http://services.arcgis.com/UXmFoWC7yDHcDN5Q/arcgis/rest/services/OutfallInventoryReadOnly/FeatureServer/0/query',
             query = list(
               where = 'OBJECTID>=1',
               outFields = '*',
               f = 'json',
               returnGeometry = 'true',
               outSR = '4326'
             ))
  
  data_raw <- 
    bind_cols(
      # Feature attributes
      content(response, 'text') %>%
        fromJSON() %>%
        .$features %>%
        .$attributes %>%
        tbl_df(),
      # Feature geometry
      content(response, 'text') %>% 
        fromJSON() %>% 
        .$features %>%
        .$geometry %>%
        tbl_df()
    )
  
  # Reshape domain info
  data_domains <- content(response, 'text') %>% 
    fromJSON() %>% 
    .$fields %>% 
    .$domain %>% 
    mutate(name = c(names(data_raw))[-c(20:21)]) %>% 
    filter(!is.na(type)) %>%
    mutate(name = paste(name, 'DOMAIN', sep = '_')) %>% 
    select(-type) %>% 
    left_join(
      .,
      apply(., 1, function(df) {
        data.frame(df$codedValues) %>% 
          mutate(domain_name = rep(df$name))
      }) %>% 
        bind_rows() %>% 
        rename('value' = name),
      by = c('name' = 'domain_name')
    ) %>% 
    select(-codedValues) %>% 
    spread(name, value)
  
  # Format dataset
  
  # Append domain info
  data_formatted <- left_join(
    data_raw,
    data_domains %>% select(code, FACTYPE_DOMAIN),
    by = c('FACTYPE' = 'code')
  ) %>% 
    left_join(
      .,
      data_domains %>% select(code, FLOWMONITSTATUS_DOMAIN),
      by = c('FLOWMONITSTATUS' = 'code')
    ) %>% 
    left_join(
      .,
      data_domains %>% select(code, INSPECTED_DOMAIN),
      by = c('INSPECTED' = 'code')
    ) %>% 
    left_join(
      .,
      data_domains %>% select(code, PRIORITY_DOMAIN),
      by = c('PRIORITY' = 'code')
    )
  
  # Only include verified outfalls
  data_formatted <- data_formatted %>%
    filter(INSPECTED_DOMAIN == 'Verified')
  

}
