#' @export
#' @import

# Темы
get_themes <- function() {
  url="http://cyberleninka.ru"
  themes <- html(url) %>% 
    html_nodes("#content .holder a") %>%
    html_text()  
  themes
}

#get_themes() %>% View()