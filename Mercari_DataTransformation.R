# Required Libraries & Functions ----
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))

# Data Input -----
test  <- fread(input = "test.tsv", sep = "\t")
train <- fread(input = "train.tsv", sep = "\t")
test  <- test[, `:=`(H1 = strsplit(x = category_name, split = "/")[[1]][1],
										 H2 = strsplit(x = category_name, split = "/")[[1]][2],
										 H3 = strsplit(x = category_name, split = "/")[[1]][3])]
train <- train[, `:=`(H1 = strsplit(x = category_name, split = "/")[[1]][1],
			 							  H2 = strsplit(x = category_name, split = "/")[[1]][2],
			  						  H3 = strsplit(x = category_name, split = "/")[[1]][3])]

# EDA ----
train[, .N, .(shipping)]          # 663100 shipping-free, 819435 shipping-paid
train[, .N, .(item_condition_id)] # 5 different conditions, "1" is the most frequent, then the rest as numeric order.
length(unique(train$brand_name))  # 4310 different brands

# Text Mining Process ----
train_name <- Corpus(VectorSource(train$name))
train_desc <- Corpus(VectorSource(train$item_description))
