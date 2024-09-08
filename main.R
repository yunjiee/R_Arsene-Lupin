'''
#亞森羅蘋的書籍
Arsène Lupin #4014
The Golden Triangle: The Return of Arsène Lupin #34795
Arsène Lupin versus Herlock Sholmes #40203
813 #34758
The Teeth of the Tiger #13058
The Crystal Stopper #1563
'''
#sort(rowSums(tdmmatrix),decreasing = T)
#barplot(top6$freq ,xlab = "word",ylab = "freq",names.arg = rownames(top6), main="前六多的詞彙的長條圖" )
#box<-barplot(top6$freq ,xlab = "word",ylab = "freq",names.arg = rownames(top6), main="前六多的詞彙的長條圖",col = "lightblue" )
#text(box,top6$freq,labels =top6$freq,pos=1) 

#install.packages(dplyer)
library(janeaustenr)
library(dplyr)
library(gutenbergr)
library(readr)
library(magrittr)
library(tidytext)
library(stringr)
library(tidyr)
library(tibble)
library(tidyverse)
library(ggplot2)

#讀資料
#舊titles <- c("Arsène Lupin","The Golden Triangle: The Return of Arsène Lupin","Arsène Lupin versus Herlock Sholmes","The Teeth of the Tiger","The Crystal Stopper","813")
#books <- gutenberg_works(title %in% titles) %>%
#  gutenberg_download(meta_fields = "title", mirror ="https://gutenberg.nabasny.com/")
#books
#新
ebook_no <- c('4014','34795','34758','13058','1563')
books <- gutenberg_download(ebook_no,meta_fields = "title", mirror ="https://www.gutenberg.org/")
books
#utenberg_download(meta_fields = "title", mirror ="https://www.gutenberg.org/")

#三本情緒辭典
get_sentiments("afinn")
get_sentiments("bing")
get_sentiments("nrc")

#包含書籍資料的資料框
# tidy_books 表示是針對全部書所進行的
tidy_books <- books %>%
  group_by() %>%
  mutate(
    linenumber = row_number(),
    chapter = cumsum(str_detect(text, 
                                regex("^chapter [\\divxlc]", 
                                      ignore_case = TRUE)))) %>%
  ungroup() %>%
  unnest_tokens(word, text)
#linenumber = row_number()#於給每個詞彙（word）賦予一個行號，表示該詞彙在書籍中的行數
#unnest_tokens(word, text) 轉為我們需要的tidy text

#找joy的詞 (afrinn 和bing 沒有joy))
nrc_joy <- get_sentiments("nrc") %>% 
  filter(sentiment == "joy")
#找Arsène Lupi中所有joy的詞
tidy_nrc<-tidy_books %>%
  filter(title == tidy_books$title) %>%
  inner_join(nrc_joy) %>%
  count(word, sort = TRUE)
head(tidy_nrc)

#每部小說的情節軌跡中繪製這些情感分數
jane_austen_sentiment <- tidy_books %>%
  inner_join(get_sentiments("nrc")) %>%
  count(title, index = linenumber %/% 80, sentiment) %>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>% 
  mutate(sentiment = positive - negative)
#找到每個詞的情感分數inner_join()
#繪圖
ggplot(jane_austen_sentiment, aes(index, sentiment, fill = title)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~title, ncol = 2, scales = "free_x")

###afinn辭典
jane_austen_sentiment <- tidy_books %>%
  inner_join(get_sentiments("afinn")) %>%
  group_by(title, index = linenumber %/% 80) %>%
  summarise(sentiment = sum(value)) %>%
  ungroup()
#找到每個詞的情感分數inner_join()
#繪圖
ggplot(jane_austen_sentiment, aes(index, sentiment, fill = title)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~title, ncol = 2, scales = "free_x")
#每部小說的情節如何隨著故事的軌跡向更積極或更消極的情緒轉變


###比較三本情感辭典

#只單看一本書
pride_prejudice <- tidy_books %>% 
  filter(title == "Arsène Lupin")
pride_prejudice

#來計算情緒
afinn <- pride_prejudice %>% 
  inner_join(get_sentiments("afinn")) %>% 
  group_by(index = linenumber %/% 80) %>% 
  summarise(sentiment = sum(value)) %>% 
  mutate(method = "AFINN")
#AFINN 詞典用 -5 到 5 之間的數字分數衡量情緒

#對每個情感詞典的小說文本的每個塊中的淨情感（正面 - 負面）進行了估計
bing_and_nrc <- bind_rows(
  pride_prejudice %>% 
    inner_join(get_sentiments("bing")) %>%
    mutate(method = "Bing et al."),
  pride_prejudice %>% 
    inner_join(get_sentiments("nrc") %>% 
                 filter(sentiment %in% c("positive","negative"))
    ) %>%
    mutate(method = "NRC")) %>%
  count(method, index = linenumber %/% 80, sentiment) %>%
  pivot_wider(names_from = sentiment,
              values_from = n,
              values_fill = 0) %>% 
  mutate(sentiment = positive - negative)

bind_rows(afinn,bing_and_nrc) %>%
  ggplot(aes(index, sentiment, fill = method)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~method, ncol = 1, scales = "free_y")
#在小說中大致相同的地方看到了類似的情緒低谷和高峰，但絕對值卻大不相同
#每一本書進行這三個辭典的比較，NRC 情緒高，AFINN 情緒有更多差異，Bing 等人
#正數表示正向情感多於負向情感，負數表示負向情感多於正向情感
#因此，當 x 值是正數時，表示該區段的情感傾向為正向；當 x 值是負數時，表示該區段的情感傾向為負向


#nrc詞典中有多少正面和負面的詞
get_sentiments("nrc") %>% 
  filter(sentiment %in% c("positive", "negative")) %>% 
  count(sentiment)
#bing詞典中有多少正面和負面的詞
get_sentiments("bing") %>% 
  count(sentiment)
#兩個詞典的負面詞多於正面詞，但 Bing 詞典中負面詞與正面詞的比例高於 NRC 詞典。這將有助於我們在上圖中看到的效果

#最常見的正面和負面詞
#可以分析對每種情緒有貢獻的詞數
bing_word_counts <- tidy_books %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()
bing_word_counts
#使用和count()的參數來實現，我們可以找出每個詞對每種情緒的貢獻程度
#繪圖
bing_word_counts %>%
  group_by(sentiment) %>%
  slice_max(n, n = 10) %>% 
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(x = "Contribution to sentiment",
       y = NULL)
#nrc辭典
nrc_word_counts <- tidy_books %>%
  inner_join(get_sentiments("nrc")) %>%
  filter(sentiment %in% c("positive", "negative")) %>% 
  count(word, sentiment, sort = TRUE) %>%
  ungroup()
nrc_word_counts
#使用和count()的參數來實現，我們可以找出每個詞對每種情緒的貢獻程度
#繪圖
nrc_word_counts %>%
  group_by(sentiment) %>%
  slice_max(n, n = 10) %>% 
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(x = "Contribution to sentiment",
       y = NULL)
##書
"Arsène Lupin"
"The Golden Triangle: The Return of Arsène Lupin"
"Arsène Lupin versus Herlock Sholmes"
"The Teeth of the Tiger"
"The Crystal Stopper"
"813"
#######分為每一本書 "nrc"
word_counts <- tidy_books %>% 
  filter(title == "813")%>%
  inner_join(get_sentiments("nrc")) %>%
  filter(sentiment %in% c("positive", "negative")) %>% 
  count(word, sentiment, sort = TRUE) %>%
  ungroup()
word_counts
#使用和count()的參數來實現，我們可以找出每個詞對每種情緒的貢獻程度
#繪圖
word_counts %>%
  group_by(sentiment) %>%
  slice_max(n, n = 10) %>% 
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(x = "Contribution to sentiment",
       y = NULL)
##bing
word_counts <- tidy_books %>% 
  filter(title == "813")%>%
  inner_join(get_sentiments("bing")) %>%
  filter(sentiment %in% c("positive", "negative")) %>% 
  count(word, sentiment, sort = TRUE) %>%
  ungroup()
word_counts
#使用和count()的參數來實現，我們可以找出每個詞對每種情緒的貢獻程度
#繪圖
word_counts %>%
  group_by(sentiment) %>%
  slice_max(n, n = 10) %>% 
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(x = "Contribution to sentiment",
       y = NULL)


#“miss”添加到自定義停用詞列表中bind_rows()
#自訂義詞的正否意義
custom_stop_words <- bind_rows(tibble(word = c("miss"),  
                                      lexicon = c("custom")), 
                               stop_words)
#custom 表示該停用詞是自訂的，而 SMART 表示詞典來自 SMART 詞典

#最常用詞的文字雲
library(wordcloud)
tidy_books %>%
  anti_join(stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 55,size = 0.5))

#可以使用這種可視化來查看最重要的積極和消極的詞，但是這些詞的大小在不同的情緒中是不可比的
#x 是指由 tidy_books 資料框與情感詞典 "bing" 進行內連接的結果，
#而 y 則是指情感詞典 "bing" 本身。


#警告訊息中提到，第 75894 列的 x 資料框與 y 資料框中的多個列相匹配，同樣地，第 6549 列的 y 資料框與 x 資料框中的多個列相匹配。這表示在連接過程中，有些欄位值在兩個資料框中都有多個匹配

library(reshape2)
tidy_books %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("gray20", "gray80"),
                   max.words = 100)
#acast() 函數是 reshape2 套件中的一個功能，用於將數據框轉換為矩陣
#內連接 (inner_join())是一種根據共同的欄位值將兩個資料框連接起來的操作
# comparison.cloud() 函數來生成詞彙的情感比較雲圖

#我們使用了一個類似的正則表達式來查找奧斯汀小說中所有章節的位置，以獲得按每行一個單詞組織的整潔數據框。我們可以使用整潔的文本分析來提出問題，例如簡·奧斯汀的每部小說中最負面的章節是什麼？
bingnegative <- get_sentiments("bing") %>% 
  filter(sentiment == "negative")

wordcounts <- tidy_books %>%
  group_by(title, chapter) %>%
  summarize(words = n())

tidy_books %>%
  semi_join(bingnegative) %>%
  group_by(title, chapter) %>%
  summarize(negativewords = n()) %>%S
  left_join(wordcounts, by = c("title", "chapter")) %>%
  mutate(ratio = negativewords/words) %>%
  filter(chapter != 0) %>%
  slice_max(ratio, n = 1) %>% 
  ungroup()
#顯示出來的是 每本書中用詞最多的章節
afinnnegative <- get_sentiments("afinn") %>% filter(value < 0)

wordcounts <- tidy_books %>%
  group_by(title, chapter) %>%
  summarize(words = n())

tidy_books %>%
  semi_join(afinnnegative) %>%
  group_by(title, chapter) %>%
  summarize(negativewords = n()) %>%
  left_join(wordcounts, by = c("title", "chapter")) %>%
  mutate(ratio = negativewords/words) %>%
  filter(chapter != 0) %>%
  slice_max(ratio, n = 1) %>% 
  ungroup()

