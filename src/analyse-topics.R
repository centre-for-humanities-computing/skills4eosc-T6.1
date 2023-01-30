library(RCurl)
library(googlesheets4)
library(tidyverse)
library(tidytext)
library(here)

## output format
output_format <- ".pdf"
fill_color <- "#F39200"

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

## Load the Google Sheet
google_sheet_url <- ""
source(here("configurations.R"))
print(google_sheet_url)

## create a table of normalized topics and super topics created by PMD
super_topics <-
    read_sheet(
        google_sheet_url,
        sheet = "OS Topics",
        skip = 0,
        col_types = "ccccc"
    ) |>
    mutate(
        os_topic = str_to_lower(`OS Topic`),
        pmd_super_topic    = str_to_lower(`PMD - super terms`),
        pmd_topic    = str_to_lower(`PMD - Search term classification`)) |>
    select(os_topic, pmd_super_topic, pmd_topic)

## Need to manually select the "See, edit, create and delete all your
## Google Sheets spreadsheets. Learn more" option when authenticating.
## Also tidy the search results on OS Topics
read_sheet(
        google_sheet_url,
        sheet = "Networks",
        skip = 0,
        col_types = "cccccccccccccccc"
    ) |>
    mutate(`OS topics` = str_to_lower(str_replace_all(`OS topics`, ",", "\n"))) |>
    unnest_tokens(os_topic, `OS topics`, token = "lines") |>
    mutate(os_topic = str_trim(os_topic)) -> tokenized_results



search_results |>
    mutate(`OS topics` = str_to_lower(str_replace_all(`OS topics`, ",", "\n"))) |>
    unnest_tokens(os_topic, `OS topics`, token = "lines") |>
    mutate(os_topic = str_trim(os_topic)) -> tokenized_results

## tokenized_results |>
##     left_join(super_topics, by="os_topic") |>
##     select(`Country`, `Country Code`, pmd_super_topic) |>
##     count(pmd_super_topic) |>
##     arrange(desc(n)) |>
##     print(n=179)

## tokenized_results |>
##     left_join(super_topics, by="os_topic") |>
##     filter(is.na(pmd_super_topic)) |>
##     select(`Name of network (English)`, `Country`, os_topic, pmd_super_topic)


## join the tidy search results with the normalized topics
tokenized_results |>
    left_join(super_topics, by="os_topic") |>
    select(`Country`, `Country Code`, pmd_super_topic) -> results_super_topics

## Create graphics of the distribution of super topics
library(ggthemes)

## Load the population data. This is not used at the moment for these graphics
API_SP_POP_TOTL_DS2_en_csv_v2_4770387_modified <- read_csv(
    here("data/API_SP/API_SP.POP.TOTL_DS2_en_csv_v2_4770387-modified.csv"),
    col_types = cols(`1960` = col_double()))

results_super_topics |>
    left_join(
        API_SP_POP_TOTL_DS2_en_csv_v2_4770387_modified |>
        select(`Country Code`, `Country Name`, `2021`), by="Country Code") -> search_results_pop


## Count networks that spans multiple countries
non_national <- search_results_pop |>
    filter(`Country Code` == "international" | `Country Code` == "regional") |> 
    pull(`Country Code`) |> length()

## count the number of found networks for each country. This is
## to enable having frequencies instead of absolute numbers
## in the graphics.
search_results |> count(`Country Code`) -> network_counts


## Non-normalized plot
search_results_pop |>
    left_join(network_counts, by="Country Code") |>
    filter(!is.na(`Country Name`)) |>
    filter(`Country Code` != "international" & `Country Code` != "regional") |>
    filter(!is.na(pmd_super_topic) & !pmd_super_topic=="?") |>
    ggplot() +
    geom_bar(aes(x=`Country Name`, fill = pmd_super_topic)) +
    coord_flip() +
    labs(
        title = "Distribution of topics",
        caption = "Source: REF TO COLLECTED DATA",
        fill = "OS Topic"
    ) +
    ylab("") +
    xlab("") +
    #scale_y_continuous(breaks=c(0,1,2,5,10,15,20,25), minor_breaks = NULL) +
    theme_light()

## The primary graphics
search_results_pop |>
    left_join(network_counts, by="Country Code") |>
    filter(!is.na(`Country Name`)) |>
    filter(`Country Code` != "international" & `Country Code` != "regional") |>
    filter(!is.na(pmd_super_topic) & !pmd_super_topic=="?") |>
    mutate(name_and_count = str_c(`Country Name`, " (",n,")")) |>
    ggplot(aes(x = name_and_count, fill = factor(pmd_super_topic))) +
    geom_bar(position = "fill") +
    coord_flip() +
    labs(
        title = "Distribution of topics",
        subtitle = "Total number of found networks in parentheses",
        caption = "Source: REF TO COLLECTED DATA",
        fill = "OS Topic"
    ) +
    ylab("") +
    xlab("") +
    ##scale_y_continuous(breaks=c(0,1,2,5,10,15,20,25), minor_breaks = NULL) +
    theme_classic() +
    theme(
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.line=element_blank(),
        axis.title.y=element_blank(),
        axis.ticks.y=element_blank())

ggsave("output/dist-topics.png")


