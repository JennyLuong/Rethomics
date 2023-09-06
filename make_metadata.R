######## Get necessary packages ##########
{
  if (!require(svDialogs, quietly=T)) {install.packages('svDialogs');library(svDialogs)}
  if (!require(readxl, quietly=T)) {install.packages('readxl');library(readxl)}
  if (!require(tidyr, quietly=T)) {install.packages('tidyr');library(tidyr)}
  if (!require(dplyr, quietly=T)) {install.packages('dplyr');library(dplyr)}
  if (!require(stringr, quietly=T)) {install.packages('stringr');library(stringr)}
  if (!require(readr, quietly=T)) {install.packages('readr');library(readr)}
  if (!require(purrr, quietly=T)) {install.packages('purrr');library(purrr)}
}

######## Convert your file to appropriate metadata file #########
{
metafile <- dlg_open(title="Choose the EXCEL metadata file")$res
dir <- dirname(metafile)
name <- str_split(basename(metafile),pattern="\\.")[[1]][1]
read_excel(metafile) %>% 
  nest(data = c(start_channel, end_channel)) %>%
  mutate(region_id = map(data, ~seq(unique(.x$start_channel), unique(.x$end_channel), 1))) %>%
  unnest(region_id) %>%
  select(-data) %>%
  relocate(region_id,.after = stop_datetime) %>%
  write_csv(paste0(dir,"/",name,".csv"))
}

