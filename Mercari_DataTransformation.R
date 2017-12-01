# Required Libraries & Functions ----
rm(list = ls()); gc()
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
library("stringr")
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))

# Data Input -----
test  <- fread(input = "test.tsv", sep = "\t")
train <- fread(input = "train.tsv", sep = "\t")
test  <- test[, `:=`(H1 = str_split_fixed(string = category_name, pattern = "/", n = 3)[,1],
										 H2 = str_split_fixed(string = category_name, pattern = "/", n = 3)[,2],
										 H3 = str_split_fixed(string = category_name, pattern = "/", n = 3)[,3])]
train <- train[, `:=`(H1 = str_split_fixed(string = category_name, pattern = "/", n = 3)[,1],
											H2 = str_split_fixed(string = category_name, pattern = "/", n = 3)[,2],
											H3 = str_split_fixed(string = category_name, pattern = "/", n = 3)[,3])]
# EDA ----
train[, .N, .(shipping)]          # 663100 shipping-free, 819435 shipping-paid
train[, .N, .(item_condition_id)] # 5 different conditions, "1" is the most frequent, then the rest as numeric order.
length(unique(train$brand_name))  # 4310 different brands

# Text Mining Process ----
train_name <- Corpus(VectorSource(train$name))
train_desc <- Corpus(VectorSource(train$item_description))
# wordcloud(train_name)