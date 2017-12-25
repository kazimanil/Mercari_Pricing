categorizx <- function(H1, H2, H3, NH2, NH3)
if(NH3 >= 1000){
	category = paste0(H1, "-", H2, "-", H3)
} else if(NH2 >= 1000){
	category = paste0(H1, "-", H2)
} else{
	category = H1
}
categorize <- Vectorize(categorizx)
rm(categorizx)