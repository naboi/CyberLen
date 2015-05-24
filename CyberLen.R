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
get_themes() %>% View()


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
number_themes("4")


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
get_number_of_pages("4")
get_number_of_pages("52")


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
get_titles("52")


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
get_links("52")

# Получение статьи по ссылке
get_article <- function(url='') {
  title <- html(url) %>% 
    html_nodes("h1 > span:nth-child(1)") %>%
    html_text()  
  title
  title <- paste(title, collapse=', ')
  
  authors <- html(url) %>% 
    html_nodes(".author-list") %>%
    html_text()  
  authors
  authors <- paste(authors, collapse=', ')
  
  journal <- html(url) %>% 
    html_nodes(".description-magazine a") %>%
    html_text()  
  journal
  
  issue <- html(url) %>% 
    html_nodes(".num") %>%
    html_text()  
  issue
  issue <- paste(issue, collapse=', ')
  
  UDK <- html(url) %>% 
    html_nodes(".codes-list .search-hl") %>%
    html_text()  
  UDK
  UDK <- paste(UDK, collapse=', ')
  
  annotation <- html(url) %>% 
    html_nodes(".general-block-indent+ .general-block p") %>%
    html_text()  
  annotation
  annotation <- paste(annotation, collapse=', ')
  
  text <- html(url) %>% 
    html_nodes(".similar-articles+ .general-block p") %>%
    html_text()  
  text
  text <- paste(text, collapse=', ')
  
  link <- paste(url, ".pdf", sep="")
  #link <- gsub("n/", "", link)
  
  title <- gsub("[\t\n\r\v\f]", " ", title)
  authors <- gsub("[\t\n\r\v\f,]", "", authors)
  authors <- gsub("                ", " , ", authors)
  authors <- gsub("           ", " , ", authors)
  authors <- str_trim(authors, side = "both")
  
  #authors <- unlist(strsplit(authors,"                    "))
  #authors <- str_split_fixed(authors, "      ", 2)
  #authors <- c(authors)
  
  journal <- gsub("[\t\n\r\v\f]", " ", journal)
  
  issue <- gsub("[\t\n\r\v\f]", " ", issue)
  issue <- gsub("         ", " ", issue)
  
  UDK <- gsub("[\t\n\r\v\f]", " ", UDK)
  UDK <- gsub(c("УДК: |ББК: |УДК "), "", UDK)
  
  annotation <- gsub("[\t\n\r\v\f]", " ", annotation)
  annotation <- gsub("           ", " ", annotation)
  
  text <- gsub("[\t\n\r\v\f]", " ", text)
  text <- gsub("            ", " ", text)
  #text <- gsub("  <U+FEFF>", "", text)
  
  if (length(title) == 0L) {
    title <- NA
  }
  if (length(authors) == 0L) {
    authors <- NA
  }
  if (length(journal) == 0L) {
    journal <- NA
  }
  if (length(issue) == 0L) {
    issue <- NA
  }
  if (length(UDK) == 0L) {
    UDK <- NA
  }
  if (length(annotation) == 0L) {
    annotation <- NA
  }
  if (length(text) == 0L) {
    text <- NA
  }
  
  literature <- NA
  
  # ОТДЕЛЕНИЕ СПИСКА ЛИТЕРАТУРЫ
  
  literstop <- c("Список литературы", "СПИСОК ЛИТЕРАТУРЫ", "ЛИТЕРАТУРА", "Литература.", 
                 "Лiтература", "Література")
  flag <- FALSE
  n <- 1
  
  for (n in 1:length(literstop)) { 
    if ((grepl(literstop[n], text) == TRUE) && (flag == FALSE)) {
      text.splitted <- unlist(str_split(text, literstop[n]))
      literature <- text.splitted[length(text.splitted)]
      text  <- paste(text.splitted[1:(length(text.splitted)-1)])
      text <- paste(text, collapse=' ')
      n <- n + 1
      flag <- TRUE
    }
  }
  
  c(title, authors, journal, issue, UDK, annotation, text, literature, link)
  
}
get_article("http://cyberleninka.ru/article/n/poisk-innovatsiy-v-sfere-tehnologiy") %>% View()


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

get_articles("52") %>% View()
get_articles("35") %>% View()

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
search("санкт-петербург город анализ") %>% View()
