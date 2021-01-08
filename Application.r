library(rvest)
library(dplyr)
library(ggplot2)
library(tidytext)
library(tidyverse)
library(stringr) 
library(tidyr)   
library(wordcloud)
library(hunspell)
library(SnowballC)
library(xtable)
library(knitr)
library(tm)
library(reshape2)
library(textdata)

######ABC NEWS######
#scrape data
abc <- read_html("https://www.abc.es/politica/pedro-sanchez/")
abc_articles <- abc %>% 
  html_nodes('h1') %>% 
  html_text('title')
df_abc <- data.frame(abc_articles)

for (page in 2:159){
  page <- paste(as.character(page), ".html",sep="")
  abc_page <- paste("https://www.abc.es/politica/pedro-sanchez/pag-", page, sep="")
  abc <- read_html(abc_page)
  abc_articles <- abc %>% 
    html_nodes('h1') %>% 
    html_text('title')
  df_abc <- rbind(df_abc, data.frame(abc_articles))
}

#replace wrong characters
df_abc$abc_articles <- str_replace_all(df_abc$abc_articles, '[\t\n\r]' , '')
df_abc$abc_articles <- str_replace_all(df_abc$abc_articles, 'Ã¡' , 'á')
df_abc$abc_articles <- str_replace_all(df_abc$abc_articles, 'Â' , '')
df_abc$abc_articles <- str_replace_all(df_abc$abc_articles, 'Ã³' , 'ó')
df_abc$abc_articles <- str_replace_all(df_abc$abc_articles, 'Ã±' , 'ñ')
df_abc$abc_articles <- str_replace_all(df_abc$abc_articles, 'Ã©' , 'é')
df_abc$abc_articles <- str_replace_all(df_abc$abc_articles, 'Ãº' , 'ú')
df_abc$abc_articles <- str_replace_all(df_abc$abc_articles, 'Ã­' , 'í')
df_abc$abc_articles <- str_replace_all(df_abc$abc_articles, 'Ã¼' , 'ü')

#change column data type
df_abc <- df_abc %>% mutate_if(is.factor, as.character)

#tokenization
df_abc <- tibble(abc_articles = df_abc$abc_articles)

df_abc <- df_abc %>% unnest_tokens(word, abc_articles)

#stop words
df_abc$word = removeWords(df_abc$word, stopwords("spanish"))
df_abc$word = removeNumbers(df_abc$word)
df_abc$word = removePunctuation(df_abc$word)
df_abc$word = stripWhitespace(df_abc$word)

#stemming
df_abc$word <- wordStem(df_abc$word,  language = "spanish")

#remove empty strings
df_abc <- df_abc[!apply(df_abc, 1, function(x) any(x=="")),] 

#frecuency
df_abc %>%
  count(word, sort = TRUE) %>%
  filter(n > 450) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

#wordcloud
wordcloud(df_abc$word, min.freq = 200, random.order = FALSE)

#sentiment analysis
abc_bing <- df_abc %>%
  inner_join(get_sentiments("bing"))

abc_nrc <- df_abc %>%
  inner_join(get_sentiments("nrc"))

abc_bing %>%
  count(word,sentiment,sort=TRUE)

abc_nrc %>%
  count(word,sentiment,sort=TRUE)

#frequency of negative and positive words
abc_bing %>%                                   
  count(word,sentiment,sort=TRUE) %>%               
  group_by(sentiment) %>%                          
  top_n(15) %>%                                           
  ungroup() %>%                                    
  mutate(word=reorder(word,n)) %>%                    
  ggplot(aes(word,n,fill=sentiment))+                 
  geom_col(show.legend = FALSE)+              
  geom_text(aes(label=n), hjust= 1.2) +       
  facet_wrap(~sentiment,scales = "free_y") +    
  coord_flip() +                              
  xlab(NULL)

#frequency of words grouped by sentiment
abc_nrc %>%
  filter(sentiment!="negative" & sentiment!="positive") %>%
  count(word,sentiment,sort=TRUE) %>%             
  group_by(sentiment) %>%                        
  top_n(15) %>%                                                     
  ungroup() %>%                                   
  mutate(word=reorder(word,n)) %>%                
  ggplot(aes(word,n,fill=sentiment))+           
  geom_col(show.legend = FALSE) +
  geom_text(aes(label=n), hjust= 0) +
  facet_wrap(~sentiment,scales = "free_y")+  
  coord_flip() +
  xlab(NULL)

#word cloud of negative and positive words
abc_bing %>%
  count(word,sentiment,sort=TRUE) %>%
  acast(word~sentiment,value.var = "n", fill = 0) %>%
  comparison.cloud(max.words = 100, title.size = 2)




######ELPAIS NEWS######
#scrape data
elpais <- read_html("https://elpais.com/noticias/pedro-sanchez-perez-castejon/")
elpais_articles <- elpais %>% 
  html_nodes('h2') %>% 
  html_text('a')
df_elpais <- data.frame(elpais_articles)

for (page in 1:350){
  page <- as.character(page)
  elpais_page <- paste("https://elpais.com/noticias/pedro-sanchez-perez-castejon/", page, sep="")
  elpais <- read_html(elpais_page)
  elpais_articles <- elpais %>% 
    html_nodes('h2') %>% 
    html_text('a')
  df_elpais <- rbind(df_elpais, data.frame(elpais_articles))
}

#change datatype to character
df_elpais <- df_elpais %>% mutate_if(is.factor, as.character)

#tokenization
df_elpais <- tibble(elpais_articles = df_elpais$elpais_articles)

df_elpais <- df_elpais %>% unnest_tokens(word, elpais_articles)

#stop words
df_elpais$word = removeWords(df_elpais$word, stopwords("spanish"))
df_elpais$word = removeNumbers(df_elpais$word)
df_elpais$word = removePunctuation(df_elpais$word)
df_elpais$word = stripWhitespace(df_elpais$word)

#stemming
df_elpais$word <- wordStem(df_elpais$word,  language = "spanish")

#remove empty strings
df_elpais <- df_elpais[!apply(df_elpais, 1, function(x) any(x=="")),] 

#frecuency
df_elpais %>% 
  count(word, sort = TRUE) %>% 
  filter(n > 250) %>% 
  mutate(word = reorder(word, n)) %>% 
  ggplot(aes(word, n)) + 
  geom_col() + 
  xlab(NULL) + 
  coord_flip()

#wordcloud
wordcloud(df_elpais$word, min.freq = 100, random.order = FALSE)

#sentiment analysis
elpais_bing <- df_elpais %>%
  inner_join(get_sentiments("bing"))

elpais_nrc <- df_elpais %>%
  inner_join(get_sentiments("nrc"))

elpais_bing %>%
  count(word,sentiment,sort=TRUE)

elpais_nrc %>%
  count(word,sentiment,sort=TRUE)

#frequency of negative and positive words
elpais_bing %>%                                   
  count(word,sentiment,sort=TRUE) %>%               
  group_by(sentiment) %>%                          
  top_n(15) %>%                                           
  ungroup() %>%                                    
  mutate(word=reorder(word,n)) %>%                    
  ggplot(aes(word,n,fill=sentiment))+                 
  geom_col(show.legend = FALSE)+              
  geom_text(aes(label=n), hjust= 1.2) +       
  facet_wrap(~sentiment,scales = "free_y") +    
  coord_flip() +                              
  xlab(NULL)

#frequency of words grouped by sentiment
elpais_nrc %>%
  filter(sentiment!="negative" & sentiment!="positive") %>%
  count(word,sentiment,sort=TRUE) %>%             
  group_by(sentiment) %>%                        
  top_n(15) %>%                                                     
  ungroup() %>%                                   
  mutate(word=reorder(word,n)) %>%                
  ggplot(aes(word,n,fill=sentiment))+           
  geom_col(show.legend = FALSE) +
  geom_text(aes(label=n), hjust= 0) +
  facet_wrap(~sentiment,scales = "free_y")+  
  coord_flip() +
  xlab(NULL)

#word cloud of negative and positive words
elpais_bing %>%
  count(word,sentiment,sort=TRUE) %>%
  acast(word~sentiment,value.var = "n", fill = 0) %>%
  comparison.cloud(max.words = 100, title.size = 2)