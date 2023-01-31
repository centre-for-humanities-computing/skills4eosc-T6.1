library(RCurl)
library(googlesheets4)
library(tidyverse)
library(tidytext)
library(here)

google_sheet_url <- ""

source(here("configurations.R"))

# print configurations
google_sheet_url

# Will need to authenticate to allow Tidyverse to access your google account.
# Need to manually select the "See, edit, create and delete all your Google Sheets spreadsheets. Learn more" option when authenticating.
search_results <-
    read_sheet(
        google_sheet_url,
        sheet = "Networks",
        skip = 0,
        col_types = "cccccccccccccccc"
    )

search_results |>
    select(`OS topics`) |>
    mutate(`OS topics` = str_replace_all(`OS topics`, ",", "\n")) |>
    unnest_tokens(topics, `OS topics`, token = "words") |>
    mutate(topics = str_trim(topics)) |>
    group_by(topics) |>
    summarise(n = n()) |>
    arrange(desc(n)) -> search_results_words



library(wordcloud)
library(RColorBrewer)

wordcloud(
    words = search_results_words$topics,
    freq = search_results_words$n,
    min.freq = 1,
    max.words = 200,
    random.order = FALSE,
    ##rot.per = 0.35,
    colors = brewer.pal(8, "Dark2")
)
ggsave("output/wc-words.png")

ggsave(here("output","wc-words.png"), w_cloud, dpi="print")
