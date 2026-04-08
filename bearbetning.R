library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
library(magrittr)
library(knitr)
library(readxl)
library(openxlsx)
library(janitor)


rådata <- read_excel("C:\\Users\\WILIDF17\\OneDrive - Sveriges Riksidrottsförbund\\Dokument\\GitHub\\RF-SISU Uppland\\SocialaMedier\\rådata.xlsx")

data <- clean_names(rådata)
data$publish_time <- as.Date(data$publish_time, "%y/%m/%d")

data <- data %>% 
  mutate(
    source = case_when(
      str_detect(post_type, "IG") ~ "Instagram",
      TRUE                        ~ "Facebook"
    ),
    description = str_trunc(description, width = 75),
    published_by = case_when(
      account_name != "RF-SISU Uppland" ~ "Annan",
      TRUE                              ~ account_name
    )
  ) %>% 
  select(post_id, permalink, publish_time, source, account_name, published_by, description, views, likes, shares, comments, saves, reach, follows, total_clicks, other_clicks, duration_sec)

write.xlsx(data, "data.xlsx")

