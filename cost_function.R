# ------------------------------------------------------------------------- # 
#
# PARAMETERS                                                           ----
#
# ------------------------------------------------------------------------- # 

p_sexe         <- mean(df$Sexe) %>% round(3)
optim_bilangue <- sum(df$Bilangue) / 2
optim_sport    <- sum(df$Sport)


# *** Constraints ----

# et en respectant certaines recommandations d'affinites
# - mettre ensemble
# contrast1 <- data.frame(
#   ID1 = sample(1:185, 20, replace = T),
#   ID2 = sample(1:185, 20, replace = T),
#   W   = 1
# )
# # - a separer
# contrast2 <- data.frame(
#   ID1 = sample(1:185, 20, replace = T),
#   ID2 = sample(1:185, 20, replace = T),
#   W   = 1
# )




# ------------------------------------------------------------------------- # 
# COST FUNCTION                                                        ----
# ------------------------------------------------------------------------- # 


f <- function(df, print = F, final = F){
  
  ## - Sexe
  if(print == T){
    print("Optimisation genre")
    df %>% group_by(Classe) %>% summarise(Sexe = mean(Sexe))
  } 
  cost_sex <- df %>% group_by(Classe) %>% 
    summarise(cost = abs(p_sexe - (sum(Sexe == 1) / n()))) %>% .$cost %>% sum()
 
  
  ## - Bilangue
  if(print == T){
    print("Optimisation option bilangue")
    df %>% group_by(Classe) %>% summarise(Bilangue = sum(Bilangue))
  } 
  cost_bilangue1 <- df %>% group_by(Classe) %>% 
    summarise(cost = sqrt(abs(optim_bilangue - sum(Bilangue)))) %>% .$cost %>% sum()
  cost_bilangue2 <- df %>% group_by(Classe) %>% 
    summarise(cost = sqrt(abs(0 - sum(Bilangue)))) %>% .$cost %>% sum()
  cost_bilangue <- cost_bilangue1 + cost_bilangue2

  
  ## - Sport
  if(print == T){
    print("Optimisation option sport")
    df %>% group_by(Classe) %>% summarise(Sport = sum(Sport))
  } 
  cost_sport1 <- df %>% group_by(Classe) %>% 
    summarise(cost = sqrt(abs(optim_sport - sum(Sport)))) %>% .$cost %>% sum()
  cost_sport2 <- df %>% group_by(Classe) %>% 
    summarise(cost = sqrt(abs(0 - sum(Sport)))) %>% .$cost %>% sum()
  cost_sport <- cost_sport1 + cost_sport2
  
  ## - Origine  
  cost_origine <- df %>% count(Classe, Origine) %>%
    mutate(n = 1/n) %>% 
    .$n %>% sum()

  
  ## - Niveau
  # cost_level <- DF %>% group_by(CLASS) %>% 
  #   summarise(cost = abs(p_level - mean(LEVEL))) %>% .$cost %>% sum()
  # 
  
  
  ## - Affinity
  cost_affinity <- 0
  for(i in 1:nrow(list_affinity)){
    
    i1 <- which(df$Nom == list_affinity$NOM1[i])
    i2 <- which(df$Nom == list_affinity$NOM2[i])
    if(df$Classe[i1] == df$Classe[i2])
      cost_affinity <- cost_affinity + list_affinity$score[i]
    
  }
  
  ## - TOTAL
  costs <- cost_sex + cost_bilangue + cost_sport + cost_origine + cost_affinity
  
  if(print == T){
    print("Ecart par contraintes")
    print(paste0("Sexe: ", round(cost_sex, 3)))
    print(paste0("Bilangue: ", round(cost_bilangue, 3)))
    print(paste0("Sport: ", round(cost_sport, 3)))
    print(paste0("Origine: ", round(cost_origine, 3)))
    print(paste0("Affinity: ", round(cost_affinity, 3)))
    print(paste0("TOTAL = ", round(costs, 3)))
  }
  
  if(final == T){
    
    origin_group <- df %>% count(Classe, Origine) %>% 
      mutate(n = paste0('origin', ifelse(n > 4, 5, n))) %>% 
      count(Classe, n) %>% 
      spread(key = n, value = nn, fill = 0)
    
    df %>%
      group_by(Classe) %>%
      summarise(
        Size  = n(),
        Sexe  = mean(Sexe),
        Bilangue = sum(Bilangue),
        Sport    = sum(Sport),
        Dys      = sum(Dys)
      ) %>%
      mutate_at(vars(-Classe), funs(round(., 2))) %>%
      left_join(origin_group) %>%
      print()
  }
  
  return(list(costs = costs))
}




