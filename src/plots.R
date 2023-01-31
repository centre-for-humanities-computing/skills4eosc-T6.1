library(RCurl)
library(googlesheets4)
library(tidyverse)
library(tidytext)
library(here)

## output format
output_format <- ".png"
fill_color <- "#F39200"
reference_text <- "Source: Collected data"

## Skills4eosc colors
## Orange: #F39200
## green: #95C11F
## pink: #E6007E
## blue: #3278B1

## or
## Hi, again! designer says he doesn't have the pantone but he gave me CMYK:
## grey: 0-0-0-90
## orange: 0-50-100-0
## green: 50-0-100-0
## blue: 81-46-10-0
## magenta: 0-100-0-0

google_sheet_url <- ""
source(here("configurations.R"))
print(google_sheet_url)
## print configurations
##google_sheet_url

## Will need to authenticate to allow Tidyverse to access your google
## account.

## Need to manually select the "See, edit, create and delete all your
## Google Sheets spreadsheets. Learn more" option when authenticating.
search_results <-
    read_sheet(
        google_sheet_url,
        sheet = "Networks",
        skip = 0,
        col_types = "cccccccccccccccc"
    )

API_SP_POP_TOTL_DS2_en_csv_v2_4770387_modified <- read_csv(
    here("data/API_SP/API_SP.POP.TOTL_DS2_en_csv_v2_4770387-modified.csv"),
    col_types = cols(`1960` = col_double()))


search_results |>
    left_join(
        API_SP_POP_TOTL_DS2_en_csv_v2_4770387_modified |>
        select(`Country Code`, `Country Name`, `2021`), by="Country Code") -> search_results_pop


search_results |>
    select(`OS topics`) |>
    mutate(`OS topics` = str_replace_all(`OS topics`, ",", "\n")) |>
    unnest_tokens(competency, `OS topics`, token = "lines") |>
    mutate(competency = str_trim(competency)) |>
    group_by(competency) |>
    summarise(n = n()) |>
    arrange(desc(n)) -> search_results_tokens

library(ggthemes)

non_national <- search_results_pop |>
    filter(`Country Code` == "international" | `Country Code` == "regional") |> 
    pull(`Country Code`) |> length()

search_results_pop|>
    filter(!is.na(`Country Name`)) |>
    filter(`Country Code` != "international" & `Country Code` != "regional") |> 
    group_by(`Country Name`,`2021`) |>
    summarise(pop=max(`2021`),sum=n(), adjusted=1e6*sum/pop) |>
    select(`Country Name`, sum) |>
    ggplot() +
    geom_col(aes(x=`Country Name`, y=sum), color="black", fill=fill_color) +
    coord_flip() +
    labs(
        title = "Number of found networks",
        caption = reference_text,
        subtitle = str_c("found ", non_national," regional or international not included")
    ) +
    ylab("") +
    xlab("") +
    ##scale_y_continuous(breaks=c(0,1,2,5,10,15,20,25), minor_breaks = NULL) +
    scale_y_continuous(minor_breaks = NULL) +
    theme_light()

ggsave(here("output",str_c("count-per-country", output_format)), dpi="print")

search_results_pop|>
    filter(!is.na(`Country Name`)) |>
    filter(`Country Code` != "international" & `Country Code` != "regional") |> 
    group_by(`Country Name`,`2021`) |>
    summarise(pop=max(`2021`),sum=n(), adjusted=1e6*sum/pop) |>
    select(`Country Name`, adjusted) |>
    ggplot() +
       geom_col(aes(x=`Country Name`, y=adjusted), color="black", fill=fill_color) +
       coord_flip() +
       labs(
           title = "Number of found networks per million people",
           caption = reference_text,
           subtitle = str_c("found ", non_national," regional or international not included")
       ) +
       ylab("") +
       xlab("") +
       scale_y_continuous(breaks=c(0,1,2,3,4,5,6,7), minor_breaks = NULL) +
    theme_light()

ggsave(here("output",str_c("count-per-country-per-million", output_format)), dpi="print")

no_year <- search_results |> 
    filter(is.na(`Year of establishment`)) |> 
    pull(`Year of establishment`) |> 
    length()

search_results_pop |> 
    filter(!is.na(`Year of establishment`)) |> 
    mutate(age = 2023 - as.numeric(`Year of establishment`)) |>
    ##count(Country) |> 
    ##filter(str_detect(`Year of establishment` ,"^\\d*$")) |>
    ##mutate(yl=length(`Year of establishment`)) |>
    ##select(`Year of establishment`)
    ##select(yl)
    ggplot() +
    geom_bar(aes(x=age), color="black", fill=fill_color) +
    ##coord_flip() +
    labs(
        title = "Age of found networks",
        caption = reference_text,
        subtitle = str_c("found ",no_year," without year of establisment")
    ) +
    ylab("Count") +
    xlab("Age") +
    scale_x_continuous(breaks=c(1,3,5,10,15,20,50,75,100), minor_breaks = NULL) +
    theme_light()

ggsave(here("output",str_c("age", output_format)), dpi="print")

search_results_pop |> 
    filter(!is.na(`Year of establishment`)) |> 
    mutate(age = 2023 - as.numeric(`Year of establishment`)) |>
    filter(age > 21) |> 
    count()

search_results_pop |> 
    filter(!is.na(`Year of establishment`)) |> 
    mutate(age = 2023 - as.numeric(`Year of establishment`)) |>
    filter(age < 21) |>
    ##count(Country) |> 
    ##filter(str_detect(`Year of establishment` ,"^\\d*$")) |>
    ##mutate(yl=length(`Year of establishment`)) |>
    ##select(`Year of establishment`)
    ##select(yl)
    ggplot() +
    geom_bar(aes(x=age), color="black", fill=fill_color) +
    ##coord_flip() +
    labs(
        title = "Age of found networks younger than 21",
        caption = reference_text,
        subtitle = str_c("found ",no_year," without year of establisment")
    ) +
    ylab("Count") +
    xlab("Age") +
    scale_x_continuous(breaks=c(1,3,5,10,15,20,50,75,100), minor_breaks = NULL) +
    theme_light()

ggsave(here("output",str_c("age-21", output_format)), dpi="print")
