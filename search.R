library(rvest)
library(stringr)

# Поиск
search <- function(request='', links2 = character(), article2 = character()){
  n <- 1
  request <- gsub(" ", "+", request)
  request <-  paste("&q=", request, sep="")
  url <- paste("http://cyberleninka.ru/search?page=", n, request, sep="")
  url <- iconv(url, "CP1251", "UTF-8")
  url <- html(URLencode(url))
  
  last <- html_node(url, ".last")
  if (is.null(last)) {
    last <- 1
  } else {
    last <- as.numeric(html_attrs(last)["page"])
  }
  
  for (n in 1:last) {
    url <- paste("http://cyberleninka.ru/search?page=", n, request, sep="")
    url <- iconv(url, "CP1251", "UTF-8")
    url <- html(URLencode(url))
    
    links <- html(url, encoding = "UTF-8") %>% 
      html_nodes(".search-result-article h2 a") %>%
      html_attr("href") 
    links2 = append(links2, links)
  }
  
  links2 <- paste("http://cyberleninka.ru", links2, sep="")
  
  art <- as.data.frame(t(sapply(links2, function(x) {article <- get_article(x) 
                                                     article2 <- rbind(article2, article)
  })), row.names = NULL)
  rownames(art) <- NULL
  colnames(art)<- c("title", "authors", "journal", "issue", "UDK", "annotation", "text","literature", "link")
  art
}

#search("санкт-петербург город анализ") %>% View()