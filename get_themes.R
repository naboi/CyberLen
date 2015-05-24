library(rvest)
library(stringr)

# Темы
get_themes <- function() {
  url="http://cyberleninka.ru"
  themes <- html(url) %>% 
    html_nodes("#content .holder a") %>%
    html_text()  
  themes
}

#number_themes("4")