#' The Thurman office function
#' 
#' 
#' The sugar to make process the thurman conflicts easier. Beligerent A is taken as the baseline
#' 
#' @param belligerent_a character string. The first person in the conflict
#' @param belligerent_b character string/ The second person in the conflict
#' 
#' @return A list of three elements
#' 
#'  * The full node embedding for each combination of edge strength
#'  * The summary showing the aggregated factional control of the group
#'  * The embeddings in igraph format
#' 
#' @export





thurman_office_function <- function(belligerent_a, belligerent_b, remove_minna = TRUE){
  
  #
  #Load the files and construct the graph
  #
  
  file_id <- grepl("thuroff", files)
  g_dl <- load_dl_graph(files[file_id], directed = FALSE)

  
  edge_df <-bind_rows(g_dl[[1]] %>%
                     as_data_frame() %>%
                     mutate(type = "business"),
                   g_dl[[2]] %>% simplify %>%
                     as_data_frame() %>%
                     mutate(type = "social")
  ) %>% as_tibble %>%
    mutate(weights = 1) %>%
    graph_from_data_frame(.,  directed = F) %>% #remove any double edges
    as_data_frame()

  #
  #Set edge data
  #
  vertex_data <-as_data_frame(g_dl[[1]], what = "vertices") %>%
    as_tibble %>%
    mutate(force = case_when(name  %in% belligerent_a ~1/length(belligerent_a),
                               name %in% belligerent_b~-1/length(belligerent_b),
                             TRUE~0))
  
  relationship_weights <- tibble(business_weight = c(1:10, rep(1, 9))*100, personal_weight =c(rep(1, 10), 2:10)*100)
  
  g_out <- graph_from_data_frame(edge_df, directed = F, vertices = vertex_data )
  
  if(remove_minna){
    
    g_out <- g_out %>%
      delete.vertices(., "MINNA") 
  }
  
  output_groups <- 1:nrow(relationship_weights) %>%
    map_df(~{
      print(paste(.x, "of", nrow(relationship_weights)))
  
  edge_df2 <- edge_df %>%
    group_by(from, to) %>%
    mutate(weights = ifelse(type == "business", relationship_weights$business_weight[.x] , relationship_weights$personal_weight[1] )) %>%
    summarise(weights = sum(weights)) %>%
    mutate(edge_name = paste(from, to, sep = "_"),
           flow = 1,
           distance = 1,
           capacity = 1,
           k = weights)
  
  g_out <- graph_from_data_frame(edge_df2, directed = F, vertices = vertex_data )
  
  if(remove_minna){
    
    g_out <- g_out %>%
      delete.vertices(., "MINNA") 
  }
  
  setse_complete <- SETSe_auto(g_out,
                               force = "force",

                               distance = "distance",
                               edge_name = "edge_name",
                               k = "k",
                               mass = sum(abs(vertex_attr(g_out, "force")))/vcount(g_out), #weight is proportional to force
                               tol = sum(abs(vertex_attr(g_out, "force")))/10000,
                               hyper_max = 10000,
                               hyper_iters = 1000,
                               hyper_tol = 0.001,
                              # step_size = 0.1,
                               verbose = FALSE)
  
  
  Out <- setse_complete$node_embeddings %>%
   # left_join(medici_partisan %>% select(-contains("force")), by = c("node" ="name") ) %>%
    mutate(
      clustering_elev =ifelse(elevation>0, 1,2)
      # clustering_kmeans = kmeans(elevation, centers = 2)$cluster
    ) %>%
    left_join(., tibble(
      node = vertex_attr(g_out , "name"),
      clustering_fast_greedy = cluster_fast_greedy(
        g_out ,
        weights = edge_attr(g_out , "k"),
        merges = T
      ) %>% as.hclust %>%
        cutree(., k = 2),
      clustering_walktrap = cluster_walktrap(
        g_out ,
        weights = edge_attr(g_out , "k"),
        merges = T
      ) %>% as.hclust %>%
        cutree(., k = 2)
    ), by = "node"
    ) %>%
    rename_clusters(., names(.)[12:14], ref_var = "node") %>%
    mutate(personal_weight = relationship_weights$personal_weight[.x],
           business_weight = relationship_weights$business_weight[.x])
   
  return(Out)
})
  
  
  output_summary <-1:nrow(relationship_weights) %>%
    map_df(~{
      
      test <- output_groups %>%
        filter(personal_weight == relationship_weights$personal_weight[.x],
               business_weight == relationship_weights$business_weight[.x])
      
      
      fract_test <- test %>%
        select(contains("clustering")) %>%
        pivot_longer(cols = contains("clustering")) %>%
        group_by(name, value) %>%
        summarise(counts = n()) %>%
        group_by(name) %>%
        mutate(fract = counts/sum(counts)) %>%
        filter(value %in% belligerent_a) %>%
        select(name, fract)
      #we know separation has been a success when the +1 and -1 forces are not in the same cluster
      
      
      test %>% select(contains("clustering")) %>% names() %>%
        map_df( ~ {
          cluster_vect <- test %>% select(.x) %>% pull(1)
          
          #if the beligerents are seperated they are in two different clusters
          beligerents_separated <- test %>% select(force , .x) %>%
            filter(force != 0) %>% pull(2) %>%
            unique(.) %>% length %>% {
              . > 1
            }
          
          tibble(name = .x, separate_succcess = beligerents_separated)%>%
            left_join(fract_test,
                      by = "name")
            
          
        }) %>%
        mutate(
          personal_weight = relationship_weights$personal_weight[.x],
          business_weight = relationship_weights$business_weight[.x]
        )
      
      
    })  %>%
    arrange(-business_weight) %>%
    mutate(ratio = paste0(personal_weight / 100, "/", business_weight / 100))
  

  output_summary  <- output_summary %>%
    mutate(ratio = ratio %>% factor(., levels = unique(output_summary$ratio)))
  
  Out <-list(groups = output_groups, 
             summary = output_summary, 
             g_out = g_out )
  
  return(Out)

  
  }  
