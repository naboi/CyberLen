#' @export
#' @import

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

#get_article("http://cyberleninka.ru/article/n/poisk-innovatsiy-v-sfere-tehnologiy") %>% View()
