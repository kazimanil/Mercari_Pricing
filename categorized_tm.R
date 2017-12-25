cats <- as.data.table(unique(train$category))
rmvw <- c("description", "") # words to be removed from the wordcloud.
for(i in 1:nrow(cats)){
	assign(x = paste0("cat_id_", i), value = Corpus(VectorSource(train[category == cats[i]]$item_description)))
	assign(x = paste0("cat_id_", i), value = tm_map(get(paste0("cat_id_", i)), content_transformer(tolower))) # Convert the text to lower case
	# assign(x = paste0("cat_id_", i), value = tm_map(get(paste0("cat_id_", i)), removeNumbers)) # Remove numbers
	assign(x = paste0("cat_id_", i), value = tm_map(get(paste0("cat_id_", i)), removeWords, stopwords("english"))) # Remove english common stopwords
	# assign(x = paste0("cat_id_", i), value = tm_map(get(paste0("cat_id_", i)), removeWords, c("blabla1", "blabla2"))) # Remove your own stop word: specify your stopwords as a character vector
	assign(x = paste0("cat_id_", i), value = tm_map(get(paste0("cat_id_", i)), removePunctuation)) # Remove punctuations
	assign(x = paste0("cat_id_", i), value = tm_map(get(paste0("cat_id_", i)), stripWhitespace)) # Eliminate extra white spaces
	
	assign(x = paste0("cat_name_", i), value = Corpus(VectorSource(train[category == cats[i]]$name)))
	assign(x = paste0("cat_name_", i), value = tm_map(get(paste0("cat_name_", i)), content_transformer(tolower))) # Convert the text to lower case
	assign(x = paste0("cat_name_", i), value = tm_map(get(paste0("cat_name_", i)), removeNumbers)) # Remove numbers
	assign(x = paste0("cat_name_", i), value = tm_map(get(paste0("cat_name_", i)), removeWords, stopwords("english"))) # Remove english common stopwords
	assign(x = paste0("cat_name_", i), value = tm_map(get(paste0("cat_name_", i)), removeWords, c("blabla1", "blabla2"))) # Remove your own stop word: specify your stopwords as a character vector
	assign(x = paste0("cat_name_", i), value = tm_map(get(paste0("cat_name_", i)), removePunctuation)) # Remove punctuations
	assign(x = paste0("cat_name_", i), value = tm_map(get(paste0("cat_name_", i)), stripWhitespace)) # Eliminate extra white space
}