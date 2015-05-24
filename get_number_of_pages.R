library(rvest)
library(stringr)

# Количество страниц
get_number_of_pages <- function(number='') {
  url <- html(number_themes(number))
  last <- html_node(url, ".last")
  if (is.null(last)) {
    1
  } else {
    as.numeric(html_attrs(last)["page"])
  }
}

#get_number_of_pages("4")
#get_number_of_pages("52")