if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse,
               stringr, data.table, stringdist, gridExtra)

couleurs = c("LEXG" = "#bb0000",
             "LCOM" = "#dd0000",
             "LFI" = "#cc2443",
             "LSOC" = "#FF8080",
             "LRDG" = "#ffd1dc",
             "LDVG" = "#ffc0c0",
             "LUG" = "#cc6666",
             "LVEC" = "#00c000",
             "LECO" = "#77ff77",
             "LDIV" = "#8c8c8c",
             "LREG" = "#DCBFA3",
             "LGJ" = "#ffff00",
             "LREM" = "#ffeb00",
             "LMDM" = "#ff9900",
             "LUDI" = "#00FFFF",
             "LUC" = "#F3D79A",
             "LDVC" = "#FAC577",
             "LLR" = "#0066cc",
             "LUD" = "#82A2C6",
             "LDVD" = "#adc1fd",
             "LDLF" = "#0082C4",
             "LRN" = "#0D378A",
             "LEXD" = "#404040",
             "LNC" = "#dddddd")

df_resultats = readRDS(file = "resultats.rds")



plot_fusion = function(ville = "Belley", nom_liste = "", departement = "", lab = T, col = ""){
  # On filtre les données
  liste_villes = unique(df_resultats$commune)
  ville = liste_villes[amatch(str_to_lower(ville), str_to_lower(liste_villes), maxDist = Inf)]  # Fuzzy match pour les fautes sur les villes
 
   if (departement != ""){
    df = subset(df_resultats, commune == ville & dep == departement)
  } else {
    df = subset(df_resultats, commune == ville)
  }
  
  if(nrow(df) < 2){stop("Cette ville n'a pas de second tour ou n'existe pas")}
  
  df = df %>% 
    filter(fusion == 1) %>%
    filter(nb_tours_present == 2)
  
  if(nrow(df) < 2){stop("Pas de fusion dans cette ville")}
  
  if (length(unique(df$tete_liste_t2)) > 1 & nom_liste == ""){
    
    print("Plusieurs fusions dans cette commune, sélection de celle avec le plus haut score au T1")
    df = df %>%
      filter(t_voix_exprimes_t2 == max(t_voix_exprimes_t2)) %>% 
      ungroup()
    
  } else if (length(unique(df$tete_liste_t2)) > 1 & nom_liste != ""){
    df = df %>%
      filter(liste == nom_liste)
  }
  
  # Création du tableau de données
  
  # Nombre de sièges au total
  nb_sieges_total = unique(df$length_list)
  data = data.frame()
  
  for (simulation in 1:nb_sieges_total){
    data = bind_rows(data, df %>% 
                       filter(num_candidat_t2 <= simulation) %>% 
                       group_by(tete_liste_t1) %>% 
                       summarise(x=n()) %>%
                       pivot_wider(names_from = tete_liste_t1, values_from = x))
  }
  
  data = data[, order(names(data))]

  # On va rajouter les noms de listes et les nuances
  liste_correspondante <- setNames(df$liste_ext, df$tete_liste_t1)[colnames(data)]
  colnames(data) <- liste_correspondante
  data = rbind(0, data)
  
  # Replace NA with 0
  for(j in 1:ncol(data)){
    data[[j]][is.na(data[[j]])] = 0}
  
  data$nb_sieges <- as.numeric(row.names(data))-1
  
  # On colore en fonction des nuances, si elles existent
  if (all(is.na(unique(df$nuance)))){
    scores = df %>% group_by(tete_liste_t1) %>%
      summarise(score = na.omit(unique(t_voix_exprimes)),
                nuance = "NC",
                liste = unique(liste_ext)) %>%
      mutate(score = score/sum(score)) %>%
      arrange(tete_liste_t1)
  } else {
    scores = df %>% 
      group_by(tete_liste_t1) %>% 
      summarise(score = na.omit(unique(t_voix_exprimes)), 
                nuance = na.omit(unique(nuance)),
                liste = unique(liste_ext)) %>%
      mutate(score = score/sum(score)) %>%
      arrange(tete_liste_t1)
  }
  
  scores = scores %>%
    mutate(col = couleurs[nuance])%>% 
    select(tete_liste_t1, liste, nuance, col, score) %>%
    arrange(tete_liste_t1)
  
  data_long <- tidyr::pivot_longer(data, cols = -nb_sieges, names_to = "column", values_to = "value")
  data_long = merge(data_long, 
                    scores, by.x = c("column"), by.y = c("liste"), all = T)
  data_long = data_long %>% 
    arrange(column) %>% 
    mutate(column = paste0(column, " (", nuance, ")"))
  scores = scores  %>%
    mutate(liste = paste0(liste, " (", nuance, ")"))
  
  # Représentation du graphique
  plot <- ggplot() + 
    labs(x = "Nombre de sièges de la liste finale",
         y = "Nombre de sièges obtenus par chaque sous-liste",
         color = "Liste : ", title = paste0("Ville : ", ville))+
    theme_bw()+
    theme(legend.position="bottom", legend.direction="vertical", plot.title = element_text(hjust = 0.5)) +
    scale_y_continuous(expand=c(0,0))+
    scale_x_continuous(expand=c(0,0))+
    geom_step(data = data_long, linewidth = 1, aes(x = nb_sieges, y = value, color = column))
  
  # On ajoute les lignes pointillées
  if (length(unique(data_long$col)) == length(unique(data_long$column))){
    plot <- plot +
      geom_abline(data = scores, linewidth = 1, aes(slope = score, color = liste, intercept = 0), 
                  linetype = "dashed", show.legend = T) +
      scale_color_manual(values = unique(data_long$col))
    
  } else {
    plot <- plot +
      geom_abline(data = scores, linewidth = 1,
                  aes(slope = score, color = liste, intercept = 0), 
                  linetype = "dashed", show.legend = T)
  }
  plot
  return(list("graph" = plot, "data" = data_long %>%
                rename("Nb sieges sur la fusion (xaxis)" = nb_sieges,
                       "Nb sieges obtenus (yaxis)" = value,
                       "Couleur" = col)))
}

