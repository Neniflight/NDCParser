# 2024017_01_Revised_NDCs_Parse_HTML.R

# Set working directory
owd = setwd("/Users/calvin/Research/NDCParser/20240407_NDC_Marine")

# Libraries
library(tidyverse)
options(tibble.width=Inf)

# # We need the list of relevant NDCs (first and second, non-duplicates).
# # This has been updated since our last downlaad of the data.
# 
# previous_ndcs = read_csv('Data/20221024_countries.csv')
# 
# # The way to do this is to first pull all the new HTML files.
# 
# previous_html = list.files("/Users/twc/Research/20221022_Updated_NDCs/Data/ndc-master") %>% 
#   grep("html", ., value=TRUE)
# 
# updated_html = list.files("Data/20240117_ClimateWatch_AllData/NDC_text_HTML/ndc-master") %>% 
#   grep("html", ., value=TRUE)
# 
# new_html = tibble(
#   Climate.Watch.HTML.File = setdiff(updated_html, previous_html)) %>% 
#   mutate(n = 1:n()) %>% 
#   full_join(previous_ndcs) %>% 
#   arrange(Climate.Watch.HTML.File)
# 
# i_remove = grep("archived|ndc-FR|ndc-ES", new_html$Climate.Watch.HTML.File)
# 
# write_csv(new_html[-i_remove,], "~/Desktop/new_html.csv")
# 
# # Now let's read through this by hand...
# # Add these:
# 
# write_csv(previous_ndcs, file="Data/20240118_countries.csv")
# Added the stuff from new_html.csv, then reloaded it:

ndcs = read_csv(file="Data/20240118_countries.csv")

n_ndcs = nrow(ndcs) # 302

library(rvest)
library(dplyr)
library(purrr)

extract_ndc = function(ii) {
  
  doc = read_html(paste0(
    'Data/20240117_ClimateWatch_AllData/NDC_text_HTML/ndc-master/', 
    ndcs$Climate.Watch.HTML.File[ii]))
  
  elem = doc %>% html_elements("*")
  
  head = doc %>% html_elements("head")
  meta = doc %>% html_elements("meta")
  body = doc %>% html_elements("body")
  
  h1 = doc %>% html_elements("h1")
  h2 = doc %>% html_elements("h2")
  h3 = doc %>% html_elements("h3")
  h4 = doc %>% html_elements("h4")
  
  p = doc %>% html_elements("p")
  ul = doc %>% html_elements("ul")
  ol = doc %>% html_elements("ol")
  tab = doc %>% html_elements("table")
  
  struct = data.frame(
    elem = 1:length(elem), 
    h1=0, h2=0, h3=0, h4=0, 
    p=0, ol=0, ul=0, tab=0)
  
  struct$h1[which(elem %in% h1)] = 1
  struct$h2[which(elem %in% h2)] = 1
  struct$h3[which(elem %in% h3)] = 1
  struct$h4[which(elem %in% h4)] = 1
  
  struct$h1 = cumsum(struct$h1)
  struct$h2 = cumsum(struct$h2)
  struct$h3 = cumsum(struct$h3)
  struct$h4 = cumsum(struct$h4)
  
  struct$p[which(elem %in% p)] = 1
  struct$ol[which(elem %in% ol)] = 1
  struct$ul[which(elem %in% ul)] = 1
  struct$tab[which(elem %in% tab)] = 1
  
  struct$p = cumsum(struct$p) * struct$p
  struct$ol = cumsum(struct$ol) * struct$ol
  struct$ul = cumsum(struct$ul) * struct$ul
  struct$tab = cumsum(struct$tab) * struct$tab
  
  # Ok, recall that lists and paragraphs can be embedded in tables, and so on.
  
  struct2 = struct[apply(struct[, c('p', 'ol', 'ul', 'tab')], 1, sum) > 0,]
  
  # So, can we separate out every sentence?
  
  struct2$line = ""
  for(i in 1:nrow(struct2)) struct2$line[i] = html_text2(elem[struct2$elem[i]])
  
  ndc_lines = as_tibble(struct2)
  
  # We also want the text from h1 to h4.
  
  hdr1 = html_text2(h1)
  hdr2 = html_text2(h2)
  hdr3 = html_text2(h3)
  hdr4 = html_text2(h4)
  
  ndc_lines$hdr1 = ""
  ndc_lines$hdr2 = ""
  ndc_lines$hdr3 = ""
  ndc_lines$hdr4 = ""
  
  ndc_lines$hdr1[ndc_lines$h1 > 0] = hdr1[ndc_lines$h1[ndc_lines$h1 > 0]]
  ndc_lines$hdr2[ndc_lines$h2 > 0] = hdr2[ndc_lines$h2[ndc_lines$h2 > 0]]
  ndc_lines$hdr3[ndc_lines$h3 > 0] = hdr3[ndc_lines$h3[ndc_lines$h3 > 0]]
  ndc_lines$hdr4[ndc_lines$h4 > 0] = hdr4[ndc_lines$h4[ndc_lines$h4 > 0]]
  
  ndc_lines = ndc_lines %>% 
    mutate(
      iso = ndcs$ISO[ii], 
      country = ndcs$Country[ii], 
      ndc = ndcs$NDC[ii], 
      date = ndcs$Date[ii],
      html = ndcs$Climate.Watch.HTML.File[ii]) %>% 
    select(
      iso, country, ndc, date, html, elem, h1, h2, h3, h4, p, ol, ul, tab,
      hdr1, hdr2, hdr3, hdr4, line)
  
  return(ndc_lines)
  
}

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

ndc_text = list()
for(i in 1:n_ndcs) {
  ndc_text[[i]] = extract_ndc(i)
  cat(paste(i, Sys.time(), '\n')) }

ndc_lines = do.call('rbind', ndc_text); rm(ndc_text) # 89160 rows
ndc_lines = ndc_lines %>% filter(line != "") # 86634 rows

save(ndc_lines, file='Output/20240118_ndc_lines.rData')

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# So, what do I want to do with all of this???

# Can we tokenize everything? Stem and lemmatize?
# Can we throw out all words that aren't in standardized dictionaries?
# Maybe we want to keep acronyms though...

# What's the next step?