#these are all the packages I will use in this script
install.packages("tm")
install.packages("wordcloud")
install.packages("SnowballC")
install.packages("tidytext")
install.packages("magrittr")
install.packages("dplyr")
install.packages("sentimentr")
install.packages("tidyr")
install.packages("purrr")
install.packages("readr")
install.packages("textdata")


#don't forget to load the packages or nothing will work
library(tm)
library(wordcloud)
library(SnowballC)
library(tidytext)
library(magrittr)
library(dplyr)
library(sentimentr)
library(tidyr)
library(purrr)
library(readr)
library(textdata)
library(ggplot2)





#we load our file in R 
 
  
text = readLines("lotr_fellowship.txt")
text_df <- data.frame(text)
#this code creates a new dataframe that sorts the text into sentences
sentences <- text_df %>% unnest_tokens(sentence, text, token = "sentences")
#this code creates a new dataframe that sorts the sentences into words
words <- sentences %>% unnest_tokens(word, sentence, token = "words")

#I had a little issue to merge the data since my two dataframes were not the same size so
#this code will apply cbindpad to merged_data in order to merge sentences and words into 1 dataframe while either merging the common rows together or turn the empty rows as NAs
merged_data <- cbindPad(sentences, words)

#this code will give me the sentiments scores for each sentence
nrc <- get_sentiments("nrc")
sentiments_sentences <- merged_data %>% inner_join(nrc, by = "word") %>% group_by(sentence, sentiment) %>% summarize(count = n())

print(sentiments_sentences$count)

#this line will change the sentences into their id to avoid overlapping on the plot
sentiments_sentences <- sentiments_sentences %>% mutate(id = row_number())                                        

#this code will create a bubble plot 
ggplot(sentiments_sentences, aes(x = sentiment, y = count, color = id, size = abs(count))) + geom_point() + xlab("Sentiment") +  ylab("") + ggtitle("Sentiment Analysis for the sentences with Pippin") + theme(plot.title = element_text(hjust = 0.5)) +  scale_size_continuous(range = c(1, 10)) + guides(size = guide_legend(title = "Sentiment Score"))

#this line gives me the most present sentiment in all sentences
most_present_sentiment <- with(sentiments_sentences, sentiment[which.max(count)])
show(most_present_sentiment)

#to get the percentage of the sentiments
sentiment_percentages <- sentiments_sentences %>%  group_by(sentiment) %>% summarise(percentage = sum(count)/sum(sentiments_sentences$count)*100)

#to plot the sentiment distribution in a historigram
ggplot(sentiment_percentages, aes(x = sentiment, y = percentage, fill = sentiment)) + geom_bar(stat = "identity") + scale_fill_manual(values = c("red", "blue", "purple", "orange","green", "pink", "brown","grey", "darkblue","darkgreen")) + labs(title = "Sentiment distribution", x = "Sentiment", y = "Percentage")

#For Pippin and Gandalf interactions
#this code loads the text 
text_new = readLines("lotr_pippin_gandalf.txt")
text_df_new <- data.frame(text_new)
sentences_new <- text_df_new %>% unnest_tokens(sentence, text_new, token = "sentences")
words_new <- sentences_new %>% unnest_tokens(word, sentence, token = "words")

merged_Newdata <- cbindPad(sentences_new, words_new)

nrc <- get_sentiments("nrc")
colnames(merged_Newdata) = c("sentence", "word")
sentiments_pippin_gandalf <- merged_Newdata %>% inner_join(nrc, by = "word") %>% group_by(sentence, sentiment) %>% summarize(count = n())

#shows the count column
print(sentiments_pippin_gandalf$count)

#get the most common sentiment
most_common_sentiment <- with(sentiments_pippin_gandalf, sentiment[which.max(count)])
show(most_common_sentiment)
sentiments_pippin_gandalf <- sentiments_pippin_gandalf %>% mutate(id = row_number())

ggplot(sentiments_pippin_gandalf, aes(x = sentiment, y = count, color = id, size = abs(count))) + geom_point() + xlab("Sentiment") +  ylab("") + ggtitle("Sentiment Analysis for Gandalf and Pippin interactions") + theme(plot.title = element_text(hjust = 0.5)) +  scale_size_continuous(range = c(1, 10)) + guides(size = guide_legend(title = "Sentiment Score"))
