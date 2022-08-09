urlobs <- function(){
  require(stringr)
  
  url <- readClipboard()
  url <- str_sub(url, 1,-1)
  url <- gsub(" ", "%20", url)
  url <- gsub("\\", "/", url, fixed = TRUE)
  url <- paste("file:///", url, sep = "")
  writeClipboard(url)
  return(url)
}