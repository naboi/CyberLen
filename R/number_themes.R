library(rvest)
library(stringr)

# Темы
number_themes <- function(number='') {
  url="http://cyberleninka.ru"
  
  themes <- html(url) %>% 
    html_nodes("#content .holder a") %>%
    html_text()  
  themes
  
  links <- html(url) %>% 
    html_nodes("#content .holder a") %>%
    html_attr("href")  
  links
  
  paste(url, data.frame(links, stringsAsFactors = FALSE)[number,], sep="")
  
}