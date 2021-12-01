 # function source only part of the file
source2 <- function(file, start, end, ...) {
  # function to source only part of a file
  # Input:
  #  file = the name of the file
  #  start = starting line
  #  end = ending line
  #  
  # 
  # 
  file.lines <- scan(file, what=character(), skip=start-1, nlines=end-start+1, sep='\n')
  file.lines.collapsed <- paste(file.lines, collapse='\n')
  source(textConnection(file.lines.collapsed), ...)
}  
