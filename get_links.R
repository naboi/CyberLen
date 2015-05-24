library(rvest)
library(stringr)

# Все ссылки статей по теме
get_links <- function(number='', link2 = character()) {
  n <- get_number_of_pages(number)
  url <- number_themes(number)
  for (n in 1:n) {
    link <- html_session(paste(url, "/", n, sep="")) %>%
      html_nodes("#articles-filtered-list .heading-text a") %>% 
      html_attr("href")
    link2 = append(link2, link)
  }
  #invisible(data.frame(link2, stringsAsFactors = FALSE))
  unlist(lapply(link2, function(s) paste("http://cyberleninka.ru", s, sep="")))
}

#get_links("52")