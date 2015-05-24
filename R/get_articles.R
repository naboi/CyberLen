library(rvest)
library(stringr)

# Проход по статьям по теме
get_articles <- function(number='') {
  article2 <- character()
  
  url <- number_themes(number)
  links <- get_links(number)
  
  art <- as.data.frame(t(sapply(links, function(s) {article <- get_article(s) 
                                                    article2 <- rbind(article2, article)
  })), row.names = NULL)
  rownames(art) <- NULL
  colnames(art)<- c("title", "authors", "journal", "issue", "UDK", "annotation", "text","literature", "link")
  art
}

#get_articles("52") %>% View()
#get_articles("35") %>% View()