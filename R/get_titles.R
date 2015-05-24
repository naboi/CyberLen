library(rvest)
library(stringr)

# Все названия и годы статей по теме
get_titles <- function(number='', title2 = character() , year2 = character()) {
  url <- number_themes(number)
  n <- get_number_of_pages(number)
  for (n in 1:n) {
    title <- html_session(paste(url, "/", n, sep="")) %>%
      html_nodes("#articles-filtered-list .heading-text a") %>% 
      html_text()
    title2 = append(title2, title)
  }
  for (n in 1:n) {
    year <- html_session(paste(url, "/", n, sep="")) %>%
      html_nodes(".num") %>% 
      html_text()
    year2 = append(year2, year)
    year2 <- gsub("Год", "", year2)
  }
  data.frame(title2, year2, stringsAsFactors = FALSE)
}

#get_titles("52")