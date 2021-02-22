#' Rename_clusters
#' 
#' 
#' used for compareing SETSe vs other clustering methods against a ground truth. Very niche.
#'
#'@param df A dataframe. The data frame with the clusters and the node names in
#'@param vars A character vector. A vector of characters naming the columns that need to be matched with beligerents
#'@export


rename_clusters <- function(df, vars, ref_var = "node", priority = "medici"){
  
  #make a copy of df with a different name so it can be updated and changed
  df2 <- df
  
  for(n in vars){
    
    #create a temporary df that holds the name of the beligerent nodes and their assigned clusters
    # temp  <- group_ids <- df2 %>%
    #   filter(force != 0) %>%
    #   select( ref_var_use = all_of(ref_var), cluster = .data[[n]])
    # 
    
    #These two temp dfs mean that if the two beligerents are not seperated no NAs are created
    #The function defaults to making the +1 beligerent be the dominant and the second group is the other beligerent
    temp  <- group_ids <- df2 %>%
      filter(force ==1) %>%
      select( ref_var_use = all_of(ref_var), cluster = .data[[n]])
    
    temp2  <- group_ids <- df2 %>%
      filter(force ==-1) %>%
      select( ref_var_use = all_of(ref_var), cluster = .data[[n]])
    #The above creates NAs and other problems when the two beligerents are in the same cluster
    #In this case the medici's take priority and the second cluster is default named oligarch
    
    #I wanted to know why NA values were appearing. it turned out the other methods could not separate the beligerents
   # print(temp)
    
    #rename the clusters with the name of the beligerent from each faction
    # df2 <-df2 %>%
    #   mutate(!!n := ifelse(.data[[n]] == 1, temp$ref_var_use[temp$cluster==1], temp$ref_var_use[temp$cluster==2]))
    df2 <- df2 %>%
      mutate(!!n := ifelse(.data[[n]] == temp$cluster, temp$ref_var_use, temp2$ref_var_use))
    
  }
  
  return(df2)
  
}
