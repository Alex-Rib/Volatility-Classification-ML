
#===============================================================================
# INITIALISATION
#===============================================================================



rm(list = ls())
gc()


#===============================================================================
# IMPORT DES PACKAGES
#===============================================================================



suppressPackageStartupMessages({
  #liste des packages 
  to_be_loaded <- c("httr","jsonlite","dplyr","lubridate","tidyquant","TTR","PerformanceAnalytics","caret"
  )
  #vérification que le package est installé sinon l'installe
  for (pck in to_be_loaded) {
    #si le package est indisponible ou pas installé
    if (!require(pck, character.only = TRUE)) {
      #installation depuis cran
      install.packages(pck, repos="http://cran.rstudio.com/")
      #recharge le package après installation 
      stopifnot(require(pck, character.only = TRUE))
    }
  }
  print(to_be_loaded)
})




#===============================================================================
# CREATIONS DES FONCTIONS
#===============================================================================


meteo_jours_saison <- function(latitude, longitude, city, start_date, end_date) {
  url <- "https://archive-api.open-meteo.com/v1/era5"
  resp <- GET(url = url,query = list(latitude = latitude,longitude = longitude,start_date = start_date,end_date = end_date,daily = paste(c("temperature_2m_max","temperature_2m_min","precipitation_sum"),collapse = ","), timezone = "UTC"))
  stop_for_status(resp)
  raw  <- content(resp, as = "text", encoding = "UTF-8")
  json <- fromJSON(raw)
  daily <- as.data.frame(json$daily)
  
  df <- daily %>%
  mutate(date = as.Date(time),temperature = (temperature_2m_max + temperature_2m_min) / 2,ville = city,pluie = precipitation_sum,jour = wday(date, label = TRUE, week_start = 1),mois = month(date),annee = year(date),saison = case_when(mois %in% c(3, 4, 5) ~ "printemps",mois %in% c(6, 7, 8) ~ "été",mois %in% c(9, 10, 11) ~ "automne",TRUE ~ "hiver"))%>%
  select(date, ville, temperature, pluie, jour, mois, saison)
  return(df)
}




data_frame_cleaner <- function(df) {
  df_clean <- df %>% filter(complete.cases(open, high, low, close)) %>%
  arrange(date)
  return(df_clean)
  }



Garman_Klass_vol <- function(df) {
  df_vol <- df %>%
  tq_mutate(select = c(open, high, low, close), mutate_fun = volatility,calc = "garman.klass",col_rename = "volatilite") %>%
  filter(!is.na(volatilite))
  return(df_vol)
  }



indice_meteo_fusion <- function(df,df2) {
  df <- df %>% mutate(date = as.Date(date))
  df2 <- df2 %>% mutate(date = as.Date(date))
  result <- df %>%
  left_join(df2, by = "date")%>%
  select(-c(symbol,open,high,low,close,volume,adjusted))
  return(result)
  }




creer_classes_and_split <- function(df, proportion) {
  set.seed(123)
  
  df <- df %>% arrange(date)
  
  index <- floor(nrow(df)*proportion)
  train <- df[1:index, ]
  test  <- df[(index+1):nrow(df), ]
  
  seuil_train <- median(train$volatilite, na.rm = TRUE)
  
  train <- train %>%
  mutate(classe = ifelse(volatilite > seuil_train, "High", "Low"))
  
  test <- test %>%
  mutate(classe = ifelse(volatilite > seuil_train, "High", "Low"))
  
  train <- train %>%
  mutate(across(c(classe, jour, saison), as.factor),mois = as.factor(mois))
  
  test <- test %>%
  mutate(across(c(classe, jour, saison), as.factor),mois = as.factor(mois))
  

  cat("Répartition des classes pour le TRAIN :", table(train$classe), "\n")
  cat("Répartition des classes pour le TEST  :", table(test$classe), "\n")
  
  return(list(train = train, test = test, seuil = seuil_train))
}



prep_for_model <- function(df) {
  df %>%
  dplyr::select(-date, -ville, -volatilite)
}



#===============================================================================
# IMPORT DATA (METEO ET INDICE BOURSIER)
#===============================================================================


meteo_paris <- meteo_jours_saison(48.8566, 2.3522, "Paris","2010-01-01","2025-01-01")
meteo_francfort <- meteo_jours_saison(50.1109, 8.6821, "Francfort","2010-01-01","2025-01-01")
meteo_ny <- meteo_jours_saison(40.7128, -74.0060, "New-York","2010-01-01","2025-01-01")



CAC40_brut <- tq_get("^FCHI", from = "2010-01-01", to = "2025-01-01")
DAX_brut <- tq_get("^GDAXI", from = "2010-01-01", to = "2025-01-01")
SP500_brut <- tq_get("^GSPC", from = "2010-01-01", to = "2025-01-01")


#===============================================================================
# VERIFICATION ET NETTOYAGE DES DATA FRAMES 
#===============================================================================


sapply(CAC40_brut, function(colonne) sum(is.na(colonne)))
sapply(CAC40_brut, function(colonne) sum(is.nan(colonne)))



sapply(DAX_brut, function(colonne) sum(is.na(colonne)))
sapply(DAX_brut, function(colonne) sum(is.nan(colonne)))


sapply(SP500_brut, function(colonne) sum(is.na(colonne)))
sapply(SP500_brut, function(colonne) sum(is.nan(colonne)))



CAC40_clean <- data_frame_cleaner(CAC40_brut)
DAX_clean <- data_frame_cleaner(DAX_brut)
SP500_clean <- data_frame_cleaner(SP500_brut)



sapply(CAC40_clean, function(colonne) sum(is.na(colonne)))
sapply(DAX_clean, function(colonne) sum(is.na(colonne)))
sapply(SP500_clean, function(colonne) sum(is.na(colonne)))


#===============================================================================
# DERNIERS AJOUTS (VOLATILITE, CLASSES) + FUSION ET SPLIT
#===============================================================================


CAC40_VOL <- Garman_Klass_vol(CAC40_clean)
DAX_VOL <- Garman_Klass_vol(DAX_clean)
SP500_VOL <- Garman_Klass_vol(SP500_clean)



CAC40_ALL <- indice_meteo_fusion(CAC40_VOL,meteo_paris)
DAX_ALL <- indice_meteo_fusion(DAX_VOL,meteo_francfort)
SP500_ALL <- indice_meteo_fusion(SP500_VOL,meteo_ny)



cat("\nVérification CAC40_ALL\n")
print(colSums(is.na(CAC40_ALL)))

cat("\nVérification DAX_ALL\n")
print(colSums(is.na(DAX_ALL)))

cat("\nVérification SP500_ALL\n")
print(colSums(is.na(SP500_ALL)))





CAC40_split <- creer_classes_and_split(CAC40_ALL,0.8)
DAX_split <- creer_classes_and_split(DAX_ALL,0.8)
SP500_split <- creer_classes_and_split(SP500_ALL,0.8)



CAC40_train <- CAC40_split$train
CAC40_test <- CAC40_split$test

DAX_train <- DAX_split$train
DAX_test <- DAX_split$test

SP500_train <- SP500_split$train
SP500_test <- SP500_split$test




CAC40_train <- prep_for_model(CAC40_train)
CAC40_test <- prep_for_model(CAC40_test)

DAX_train <- prep_for_model(DAX_train)
DAX_test <- prep_for_model(DAX_test)

SP500_train <- prep_for_model(SP500_train)
SP500_test <- prep_for_model(SP500_test)



#===============================================================================
# SAUVEGARDE
#===============================================================================


if (!dir.exists("DATA")) {
  dir.create("DATA", recursive = TRUE)
}
saveRDS(CAC40_ALL, file = "DATA/CAC40.rds")
saveRDS(DAX_ALL,   file = "DATA/DAX.rds")
saveRDS(SP500_ALL, file = "DATA/SP500.rds")

saveRDS(CAC40_train, file = "DATA/CAC40_train.rds")
saveRDS(CAC40_test,  file = "DATA/CAC40_test.rds")

saveRDS(DAX_train,   file = "DATA/DAX_train.rds")
saveRDS(DAX_test,    file = "DATA/DAX_test.rds")

saveRDS(SP500_train, file = "DATA/SP500_train.rds")
saveRDS(SP500_test,  file = "DATA/SP500_test.rds")

