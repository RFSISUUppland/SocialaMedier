library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
library(magrittr)
library(lubridate)
library(knitr)
library(readxl)
library(openxlsx)
library(janitor)



rådata <- read_xlsx("C:\\Users\\WILIDF17\\OneDrive - Sveriges Riksidrottsförbund\\Dokument\\GitHub\\RF-SISU Uppland\\SocialaMedier\\rådata.xlsx",
                    sheet = "rådata")

data <- clean_names(rådata)
data$publish_time <- as.Date(data$publish_time, "%y/%m/%d")

data <- data %>% 
  filter(!( (is.na(views)   | views   == "") &
             (is.na(likes)   | likes   == "") &
             (is.na(shares)  | shares  == "") &
             (is.na(comments)| comments== "") &
             (is.na(reach)   | reach   == "") )) %>%
  
  mutate(
    source = case_when(
      str_detect(post_type, "IG") ~ "Instagram",
      TRUE                        ~ "Facebook"
    ),
    description = str_trunc(description, width = 100),
    published_by = case_when(
      account_name != "RF-SISU Uppland" ~ "Annan",
      TRUE                              ~ account_name
    ),
    post_type = case_when(
      post_type %in% c("IG image", "Photos") ~ "Bild",
      post_type %in% c("IG reel", "Videos") ~ "Video",
      post_type %in% c("IG carousel") ~ "Karusell",
      post_type %in% c("Links") ~ "Länkar (FB)",
      post_type %in% c("Text") ~ "Text (FB)",
      TRUE ~ post_type
    )
  ) %>% 
   
  select(post_id, permalink, publish_time, source, account_name, published_by, description, post_type, category, views, likes, shares, comments, saves, reach, follows, total_clicks, other_clicks, duration_sec)



wb <- loadWorkbook("data.xlsx")

removeWorksheet(wb, "content")

addWorksheet(wb, "content")

writeData(wb, sheet = "content", data)

saveWorkbook(wb, "data.xlsx", overwrite = TRUE)


