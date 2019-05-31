

# ------------------------------------------------------------------------- # 
# 
#   CLASS ROOM ALGORITHM                                               ----
#
# ------------------------------------------------------------------------- # 

library(openxlsx)
#library(xlsx)
library(magrittr)
library(tidyverse)

# ------------------------------------------------------------------------- # 
#
# DATA                                                                 ----
#
# ------------------------------------------------------------------------- # 

# *** received file ----

list_names <- openxlsx::read.xlsx(
  xlsxFile = 'C:/Users/Sylvain/Dropbox/Data Scientist/Recherche Operationnelle/exemple de fichier de travail pour Delph.xlsx',
  sheet    = 2
) %>% 
  set_colnames(c('ID', 'Origine', 'Classe', 'Nom', 'Prenom', 'Sexe', 'Date', 'Famille',
                 'Bilangue', 'Sport', 'Niveau', 'Dys', 'PAP', 'Aide_Fr', 'Aide_Maths',
                 'Avec', 'Sans', 'Commentaires', 'Sante')) %>% 
  select(-Date, -Famille, -PAP, -Aide_Fr, -Aide_Maths, -Commentaires, -Sante, -Avec, -Sans) %>% 
  mutate_at(
    vars(Origine, Classe, Sexe, Niveau), list(~factor)
  ) %>% 
  mutate(
    Sexe     = 1 * (Sexe == 'F'),
    Bilangue = 1 * (Bilangue == 'B'),
    Sport    = 1 * (Sport    == 'O'),
    Dys      = 1 * (Dys %in% c('dys', 'DYS')),
    Nom      = paste(Prenom, Nom)
  ) %>% 
  select(-Prenom)

summary(list_names)
head(list_names)

list_affinity <- openxlsx::read.xlsx(
  xlsxFile = 'C:/Users/Sylvain/Dropbox/Data Scientist/Recherche Operationnelle/exemple de fichier de travail pour Delph.xlsx',
  sheet    = 3
) %>% 
  mutate(score = Préférences * (-20) - 50 * (!is.na(Priorité)))

summary(list_affinity$NOM1 %in% list_names$Nom)
summary(list_affinity$NOM2 %in% list_names$Nom)
head(list_affinity)

# *** created file ----

N <- 177 - nrow(list_names)        # on recoit une liste de N eleves
K <- nlevels(list_names$Classe)    # que l'on veut repartir dans K classes

list_simulate <- data.frame(
  ID       = nrow(list_names) + 1:N,
  Sexe     = rbinom(N, 1, .5),
  Origine  = runif(N, 0, 1) %>% cut(breaks = seq(0, 1, length = nlevels(list_names$Origine)+1)) %>% lvls_revalue(levels(list_names$Origine)),
  Niveau   = rep_len(list_names$Niveau, length.out = N),
  Dys      = 0,
  Bilangue = ifelse(runif(N, 0, 1) < .20, 1, 0),
  Sport    = ifelse(runif(N, 0, 1) < .03, 1, 0),
  Classe   = runif(N, 0, 1) %>% cut(breaks = seq(0, 1, length = K+1)) %>% lvls_revalue(levels(list_names$Classe))
)

df_init <- bind_rows(list_names, list_simulate) %>%
  mutate(Classe = rep_len(levels(list_names$Classe), length.out = n()) %>% factor) %>% 
  arrange(Classe) %>% 
  select(ID, Nom, Classe, Origine, everything())

summary(df_init)
head(df_init)





# ------------------------------------------------------------------------- # 
# COST FUNCTION                                                        ----
# ------------------------------------------------------------------------- # 

f(df_init)



# ------------------------------------------------------------------------- # 
# IMPROVMENTS ITERATIONS                                               ----
# ------------------------------------------------------------------------- # 

N <- nrow(df_init)

# *** Random switch ----
max_iter <- 2e2
df <- df_init

for(i in 1:max_iter){
  
  cost_current <- f(df)$costs
  print(paste0(i, "/ Cost current: ", round(cost_current, 4)))
  
  switch <- sample(1:N, 2)
  temp <- df
  temp$Classe[switch[1]] <- df$Classe[switch[2]] 
  temp$Classe[switch[2]] <- df$Classe[switch[1]] 
  
  cost_new <- f(temp)$costs
  
  if(cost_new < cost_current){ 
    print("cost reduced !")
    df <- temp 
  }
}

f(df_init, final = T)
f(df, final = T)


# *** Switch one by one (by individu) ----
inds <- 1:N

for(ind in inds){

  class <- df$Classe[ind]
  cost_current <- f(df)$costs
  print(paste0(ind, "/ Cost current: ", round(cost_current, 4)))

  cost_j <- list()
  for(j in 1:N){
    if(df$Classe[j] != class){
      temp <- df
      temp$Classe[ind] <- df$Classe[j]
      temp$Classe[j]   <- df$Classe[ind]
      cost_j[j] <- f(temp)$costs
    } else {
      cost_j[j] <- cost_current
    }
  }

  minima <- which.min(unlist(cost_j))
  if(cost_j[[minima]] < cost_current){
    print("cost reduced !")
    df$Classe[ind]    <- df$Classe[minima]
    df$Classe[minima] <- class
  }

}

# ------------------------------------------------------------------------- # 
# Results                                                              ----
# ------------------------------------------------------------------------- # 

f(df_init, final = T)
f(df, final = T)

