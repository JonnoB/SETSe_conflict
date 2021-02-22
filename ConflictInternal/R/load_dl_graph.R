#' load dl graph type 
#' 
#' a dl loader for pajek dl files and dl files from the Linton freedman network archive http://moreno.ss.uci.edu
#' 
#' According to the igraph documentation there is a lack of dl format standard. This means that creating a dl loader is challenging.
#' unfortunately a lot of file on the pajek database are in dl format. I have made this loader so that they can be used.
#' 
#' @return The function returns a named list where each element of the list is a an igraph object representing each network in the dl file
#' 
#' @param file_path  A character string. The file path of the dl file to be loaded
#' @param directed A logical. whether the graph is directed or not
#' @param return_graph Logical. whether a graph or adjacency matrix should be returned.

load_dl_graph <- function(file_path, directed = FALSE, return_graph = TRUE){
  
  raw_mat <- read_lines(file_path) %>%
    enframe()  %>%
    mutate(header = grepl(":", value))
  
  header_rows <- raw_mat %>%
    filter(header) %>%
    mutate(row_id = which(raw_mat$header),
           rows  = grepl("ROW", value),
           columns = grepl("COLUMN", value),
           labels = grepl("LABELS", value),
           levels = grepl("LEVEL", value))
  
  section <- 1:nrow(header_rows) %>% map(~{
    #line after header
    from_row <- header_rows$row_id[.x]+1
    
    if(.x<nrow(header_rows)){
      #the line before the next header
      to_row <- header_rows$row_id[.x+1]-1
    } else {
      
      to_row <- nrow(raw_mat)
      
    }
    
    raw_mat %>%
      slice(from_row:to_row) %>%
      select(value)
    
  })
  
  #This works on the basis that there are either two abels OR 1 that means either "ROW LABELS" and "COLUMN LABELS", OR "just "LABELS
  #The else statment doesn't work if there are more than one labels
  if(sum(header_rows$columns)>0){
    
    column_labels <- section[[which(header_rows$columns)]]
    
  } else {
    
    column_labels <- section[[which(header_rows$labels)]]
    
  }
  
  column_labels <- column_labels  %>% pull %>% str_split(., ",") %>% unlist %>% str_remove_all(., '"')
  
  if(sum(header_rows$rows)>0){
    
    row_labels <- section[[which(header_rows$rows)]]
    
  } else {
    
    row_labels <- section[[which(header_rows$labels)]] 
    
  }
  
  row_labels <- row_labels  %>% pull %>% str_split(., ",") %>% unlist %>% str_remove_all(., '"')  %>% tibble(from = .)
  
  #split the data frame into the individual table levels and convert into a graph
  #if the levels section exists add it in here
  if(sum(header_rows$levels)>0){
    
    table_levels <- section[[which(header_rows$levels)]] %>% pull
    
  } else {
    table_levels <- "unique"
  }
  
  #make the columns of the dataframe
  section[[which(grepl("DATA", header_rows$value))]] <- section[[which(grepl("DATA", header_rows$value))]] %>%
    mutate(value = str_squish(value)) %>%
    separate(col = value, into = column_labels, sep = " ", convert = TRUE) %>%
    mutate(table_id = rep(1:length(table_levels), each = nrow(.)/length(table_levels)))
  
  tables_list <- 1:length(table_levels) %>%
    map(~{
      
      temp <- section[[which(grepl("DATA", header_rows$value))]] %>%
        filter(table_id ==.x) %>%
        select(-table_id) %>%
        bind_cols(row_labels,.) 
      
      if(return_graph){ 
        
        temp <- temp %>%
          pivot_longer(cols = 2:ncol(.), names_to = "to", values_to = "values") %>%
          filter(values !=0) %>% #remove 0 strength links
          rename(weights = values) %>%
          graph_from_data_frame(., directed = directed)
      }
      
      return(temp)
      
    })
  
  #name the list of graphs if there is only one graph don't return the list but the graph
  if(length(table_levels)>1){
    names(tables_list) <- table_levels
  } else{
    
    tables_list <- tables_list[[1]]
  }
  
  return(tables_list)
  
}