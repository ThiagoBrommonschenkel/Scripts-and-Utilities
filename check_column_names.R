check_column_names <- function(df1, df2) {
  # Checks if the column names are the same (ignoring case)
  iguais <- all(tolower(colnames(df1)) == tolower(colnames(df2)))
  
  # If column names are not the same, identify differences
  if (!iguais) {
    # Get the column names of each dataframe
    nomes_df1 <- tolower(colnames(df1))
    nomes_df2 <- tolower(colnames(df2))
    
    # Check which column names are different
    diferentes <- which(nomes_df1 != nomes_df2)
    
    # Show the names of different columns
    cat("The following column names are different:\n")
    cat(nomes_df1[diferentes], "\n")
  }
}