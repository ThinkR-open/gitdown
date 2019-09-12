replace_in_file <- function(file, pattern, replacement){
  a <- readLines(file)
  a <- gsub(pattern, replacement, a)
  write(a, file)
}
