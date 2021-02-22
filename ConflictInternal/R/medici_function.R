#' Medici function
#' 
#' This function is a horror against all that is good in programming... I care not.
#' 
#' This function produces two dataframes for the florentine families. It is big and ugly but a better solution doesn't seem to be valuable
#' 
#' @param medici a character string. The name of the Medici party who will act as the beligerent. must be all caps
#' @param oligarch a character string. The name of the Oligarch party who will act as the beligerent. must be all caps
#' @export


medici_function <-function(medici = "MEDICI", oligarch = "STROZZI"){
  
  g_dl <- load_dl_graph(files[5], directed = FALSE)
  
  #all names
  medici_vertex <- bind_rows(as_data_frame(g_dl[[1]], what = "vertices"),
                             as_data_frame(g_dl[[2]], what = "vertices")) %>%
    distinct()
  
  #add in the partisan data for the medici dataset, also add in the force of the network
  medici_data <- read_lines(files[6])
  medici_partisan <- medici_data[26:41] %>%
    str_squish() %>%
    tibble(name = medici_data[5:20], V1 = .) %>%
    separate(., col = V1, into =c("wealth", "priors", "ties"),  sep  = " ", convert = TRUE) %>%
    # full_join(medici_vertex, .) %>%
    mutate(party = c(
      "medici",
      "split",
      "medici",
      "oligarch",
      "split",
      "oligarch",
      "oligarch",
      "oligarch",
      "oligarch",
      "medici",
      "medici",
      "oligarch",
      "split",
      "medici",
      "medici",
      "neutral"
      
    ),
    party2 = ifelse(party=="medici", "medici", "oligarch"), #there is a problem caused by the albizzi being split. Trying to fix this way
    #the problem ends up with the clusters formed by walktrap and fast greedy not being identical when looking at albizzi and strozzi
    force = case_when(
      name == medici ~1,
      name == oligarch ~-1,
      TRUE ~0
    ),
    force2 = case_when(
      party =="medici" ~1/6,
      party =="oligarch" ~-1/6,
      TRUE ~0
      
    )
    
    ) 
  
  #calulate the fraction of the total range the variable is.
  #this can then be multiplied by the range value of the spring constant to find k
  range_fract <- function(var){
    
    (var-min(var))/(max(var)-min(var))
    
  }
  
  relationship_weights <- tibble(business_weight = c(1:10, rep(1, 9))*100, marriage_weight =c(rep(1, 10), 2:10)*100)
  
  medici_groups <- 1:nrow(relationship_weights) %>%
    map_df(~{
      print(paste(.x, "of", nrow(relationship_weights)))
      
      g_medici <- bind_rows(
        as_data_frame(g_dl[[1]]) %>% distinct %>% mutate(type = "marriage",
                                                         weight = relationship_weights$marriage_weight[.x]),
        as_data_frame(g_dl[[2]]) %>% distinct  %>% mutate(type = "Business",
                                                          weight = relationship_weights$business_weight[.x])) %>%
        group_by(from, to) %>%
        summarise(edge_weight = sum(weight)) %>% 
        ungroup %>%
        mutate(
          k = edge_weight,
          temp_flow = 1,
          temp_flow2 = 1,
          edge_name = paste(from, to ,sep="-")
        ) %>%
        graph_from_data_frame(., directed = FALSE, vertices = medici_partisan) 
      
      g_medici <- g_medici  %>%
        delete.vertices(., which(vertex_attr( g_medici , "name")=="PUCCI")) %>%
        set.edge.attribute(. , "distance", value = 1) %>%
        set.edge.attribute(., "Area", value = 1)
      
      medici_SETS <-SETSe_auto(g_medici, 
                               force = "force", 
                               distance = "distance", 
                               edge_name = "edge_name",
                               k = "k",
                               tstep = 0.02,
                               tol = 10e-4,
                               mass = 1,
                               verbose = FALSE)
      
      test <- medici_SETS$node_embeddings %>%
        left_join(medici_partisan %>% select(-contains("force")), by = c("node" ="name") ) %>%
        mutate(
          clustering_elev =ifelse(elevation>0, 1,2)
          # clustering_kmeans = kmeans(elevation, centers = 2)$cluster
        ) %>%
        left_join(., tibble(
          node = vertex_attr(g_medici , "name"),
          clustering_fast_greedy = cluster_fast_greedy(
            g_medici ,
            weights = edge_attr(g_medici , "k"),
            merges = T
          ) %>% as.hclust %>%
            cutree(., k = 2),
          clustering_walktrap = cluster_walktrap(
            g_medici ,
            weights = edge_attr(g_medici , "k"),
            merges = T
          ) %>% as.hclust %>%
            cutree(., k = 2)
        ), by = "node"
        ) %>%
        rename_clusters(., names(.)[17:19], ref_var = "party2") %>% #rename clusters temporarily removed
        mutate(marriage_weight = relationship_weights$marriage_weight[.x],
               business_weight = relationship_weights$business_weight[.x],
               betweenness = betweenness(g_medici, directed = FALSE, weights = get.edge.attribute(g_medici, name = "k"), normalized = F))
      
    })
  
  
  #fast greedy better with weights
  
  
  medici_summary <-1:nrow(relationship_weights) %>%
    map_df(~{
      
      test <- medici_groups %>%
        filter(marriage_weight == relationship_weights$marriage_weight[.x],
               business_weight == relationship_weights$business_weight[.x])
      
      
      
      fract_test <- test %>%
        select(contains("clustering")) %>%
        pivot_longer(cols = contains("clustering")) %>%
        group_by(name, value) %>%
        summarise(counts = n()) %>%
        group_by(name) %>%
        mutate(fract = counts/sum(counts)) %>%
        filter(value == "medici") %>%
        select(name, fract)
      
      #get the betweeness ratio of the beligerents
      
      
      
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
          
          tibble(name = .x,
                 ARI = adjustedRandIndex(test$party, cluster_vect)) %>%
            bind_cols(
              test %>%
                select(group = .x, wealth, priors, ties) %>%
                group_by(group) %>%
                summarise_all(sum) %>%
                mutate_at(., 2:4, ~ {
                  . / sum(.)
                }) %>%
                filter(group == "medici") %>%
                select(-group)
            ) %>%
            left_join(fract_test,
                      by = "name") %>%
            mutate(separate_success = beligerents_separated)
          
        }) %>%
        mutate(
          med_between = test$betweenness[test$node == medici],
          oli_between = test$betweenness[test$node == oligarch],
          med_over_oli_between = test$betweenness[test$node == medici]/test$betweenness[test$node == oligarch],
          marriage_weight = relationship_weights$marriage_weight[.x],
          business_weight = relationship_weights$business_weight[.x]
        )
      
      
    })  %>%
    arrange(-business_weight) %>%
    mutate(ratio = paste0(marriage_weight / 100, "/", business_weight / 100))
  
  medici_summary  <- medici_summary %>%
    mutate(ratio = ratio %>% factor(., levels = unique(medici_summary$ratio)))
  
  Out <-list(groups = medici_groups, summary = medici_summary )
  
  return(Out)
  
}
