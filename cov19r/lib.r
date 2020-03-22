library(rvest)
library(tidyverse)
library(stringr)
library(zoo)

url <- "https://en.wikipedia.org/wiki/2020_coronavirus_pandemic_in_Australia"
page <- read_html(url)

get_number <- function(input_str){
    str_replace_all(input_str, "\\[\\d+\\]", "") %>% as.numeric()
}

get_stats <- function(page){
    tables <- html_nodes(page,'.wikitable')
    
    c1 <- c("Confirmed", "Recovered", "Deaths", "Existing")
    t1 = tables[1] %>% html_table() %>% as.data.frame() %>% mutate_at(c1, get_number) %>% na.locf()
    colnames(t1) <- c("Date", c1)
    
    c2 = c("NSW.b..c.", "Qld", "Vic","SA","WA","Tas","ACT","NT","Total.a.","New.Cases")
    t2 = tables[2] %>% html_table() %>% as.data.frame() %>% mutate_at(c2, get_number) %>% na.locf()
    colnames(t2) <- c("Date", "NSW", "Qld", "Vic","SA","WA","Tas","ACT","NT","Total","New.Cases")
    
    list("stats"=t1, "new_cases" = t2)
}

