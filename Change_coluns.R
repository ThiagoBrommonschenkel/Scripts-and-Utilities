Change_coluns <- function(matrix, col1, col2) {
  # First step: Store column1 temporarily
  temp <- matrix[, col1]
  temp_name <- colnames(matrix)[col1]
  # Swap column1 with column2
  matrix[, col1] <- matrix[, col2]
  colnames(matrix)[col1] <- colnames(matrix)[col2]
  
  # Swap column2 with temporarily column 1
  matrix[, col2] <- temp
  colnames(matrix)[col2] <- temp_name
  
  # Return the matrix with the changes
  return(matrix)
}