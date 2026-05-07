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


  # Rådata

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


  ### Webbdata

  # Interaktioner med sidor

webb_sidor <- read_excel("rådata.xlsx", 
                         sheet = "webb_sidor")

webb_sidor <- clean_names(webb_sidor)

webb_sidor <- webb_sidor %>%
  mutate(
    nyhet = if_else(
      grepl("/nyheter/", webbadress_sokvag),
      1L,
      NA_integer_
    )
  ) %>% select(
    webbadress_sokvag, sidtitel, besokare, sessioner, avvisningsfrekvens, nyhet
  )

wb <- loadWorkbook("data.xlsx")

removeWorksheet(wb, "webb_sidor")

addWorksheet(wb, "webb_sidor")

writeData(wb, sheet = "webb_sidor", webb_sidor)

saveWorkbook(wb, "data.xlsx", overwrite = TRUE)


  # Slå ihop webbnyheter med inlägg från FB + Insta
webb_nyheter <- webb_sidor %>%  # Filtrera bort icke-nyheter samt inkludera endast nyheter från 2024-25
  filter(
    nyhet == 1,
    str_detect(webbadress_sokvag, "2024|2025"),
    besokare > 4  # Filtrera bort inlägg som aldrig offentliggjordes eller är felmeddelanden
  ) %>% 
  mutate( # Plocka ut datumen från namnet på nyheten som egen kolumn
    datum = as.Date(
      str_extract(webbadress_sokvag, "\\d{4}-\\d{2}-\\d{2}")
    )
  ) %>% 
  select(datum, sidtitel, besokare, sessioner)

colnames(webb_nyheter) <- c("publish_time", "description", "reach", "views") # Gör om namnen så att de passar med den andra dataramen

webb_nyheter <- webb_nyheter %>% 
  mutate(source = "Webb",
         post_type = "Inlägg (webb)"
         )

data <- bind_rows(data, webb_nyheter)


wb <- loadWorkbook("data.xlsx")

removeWorksheet(wb, "content")

addWorksheet(wb, "content")

writeData(wb, sheet = "content", data)

saveWorkbook(wb, "data.xlsx", overwrite = TRUE)



  # Interaktioner från andra webbplatser

webb_andrawebbplatser <- read_excel("rådata.xlsx", 
                                    sheet = "webb_andrawebbplatser")

webb_andrawebbplatser <- clean_names(webb_andrawebbplatser)

webb_andrawebbplatser <- webb_andrawebbplatser %>%
  mutate(
    kalla_medium = gsub(" / referral", "", kalla_medium)
  ) %>% select(
    kalla_medium, sidtitel, besokare, sessioner, avvisningsfrekvens
  )


wb <- loadWorkbook("data.xlsx")

removeWorksheet(wb, "webb_andrawebbplatser")

addWorksheet(wb, "webb_andrawebbplatser")

writeData(wb, sheet = "webb_andrawebbplatser", webb_andrawebbplatser)

saveWorkbook(wb, "data.xlsx", overwrite = TRUE)
