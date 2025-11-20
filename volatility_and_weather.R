
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
  to_be_loaded <- c("dplyr","tibble","ggplot2","tidyr","caret","xgboost","randomForest","gbm","pROC","purrr","scales")
  
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
# IMPORT DES DATA FRAMES
#===============================================================================


CAC40 <- readRDS("DATA/CAC40.rds")
DAX   <- readRDS("DATA/DAX.rds")
SP500 <- readRDS("DATA/SP500.rds")

CAC40_train <- readRDS("DATA/CAC40_train.rds") 
CAC40_test  <- readRDS("DATA/CAC40_test.rds")

DAX_train   <- readRDS("DATA/DAX_train.rds")
DAX_test    <- readRDS("DATA/DAX_test.rds")

SP500_train <- readRDS("DATA/SP500_train.rds")
SP500_test  <- readRDS("DATA/SP500_test.rds")


#===============================================================================
# FONCTIONS
#===============================================================================




train_control <- function(nrowtrain, proportion,k) {
  caret::trainControl(method = "timeslice",initialWindow = floor(proportion*nrowtrain), horizon =21 , skip =  floor((nrowtrain - floor(proportion*nrowtrain))/k), fixedWindow = FALSE,classProbs = TRUE,summaryFunction = twoClassSummary,savePredictions = "final",verboseIter = FALSE )
}



train_one_modele <- function(df, methode, controle) {
  caret::train(classe ~ .,data = df,method = methode, metric = "ROC",trControl = controle, preProcess = c("center", "scale","zv"))
}



train_all_modele <- function(df, methode, controle) {
  set.seed(123)
  modeles <- list()
  for (m in methode) {
    cat("Entraînement du modèle :", m, "\n")
    
    if (m == "xgbTree") {
      modeles[[m]] <- caret::train(classe ~ .,data = df,method = "xgbTree",metric = "ROC",trControl = controle,verbosity = 0,preProcess = c("center", "scale","zv"))
    } else if (m == "gbm") {
      modeles[[m]] <- caret::train(classe ~ .,data = df,method = "gbm",metric = "ROC",trControl = controle,verbose = FALSE,preProcess = c("center", "scale","zv"))
    } else {
      modeles[[m]] <- train_one_modele(df, m, controle)
    }
  }
  return(modeles)
}



evaluation_modele <- function(modele, df_test) {
  proba_high <- predict(modele, newdata = df_test, type = "prob")[, "High"]
  prediction_class <- ifelse(proba_high >= 0.5, "High", "Low")
  prediction_class <- factor(prediction_class, levels = levels(df_test$classe))
  
  ROC <- pROC::roc(response = df_test$classe,predictor = proba_high,levels = rev(levels(df_test$classe)))
  AUC <- pROC::auc(ROC)
  
  MATRICE <- caret::confusionMatrix(prediction_class, df_test$classe)
  
  list(modele= modele$method,auc = as.numeric(AUC),roc = ROC, confusion = MATRICE)
}





comparaison_modele <- function(modeles, df_test) {
  resultats <- lapply(names(modeles), function(nom) {
    evaluation <- evaluation_modele(modeles[[nom]], df_test)
    list(modele = nom,AUC = evaluation$auc, ROC = evaluation$roc,confusion = evaluation$confusion)
  })
  
  table_auc <- data.frame(modele = sapply(resultats, function(r) r$modele),AUC = sapply(resultats, function(r) r$AUC)) %>%
    arrange(desc(AUC))
  
  confusions <- lapply(resultats, function(r) r$confusion)
  names(confusions) <- sapply(resultats, function(r) r$modele)

  return(list(table = table_auc, rocs = resultats, confusion = confusions))
}




importance_var <- function(modele, nom){
  imp <- caret::varImp(modele[[nom]])
  imp_df <- imp$importance %>%
    tibble::rownames_to_column(var = "variable") %>%
    arrange(desc(Overall))
  return(imp_df)
}
  





best_of3 <- function(resultat){
  best_of_df <- resultat$table %>%
    arrange(desc(AUC)) %>%
    dplyr::slice(1:3) %>%
    dplyr::pull(modele)
  return(best_of_df)
}




importances_top <- function(modele_liste, top_modele_names, top) {
  top_vars_all <- purrr::map_dfr(top_modele_names, function(nom) {
    imp <- caret::varImp(modele_liste[[nom]])$importance %>%
      tibble::rownames_to_column("variable") %>%
      arrange(desc(Overall)) %>%
      slice_head(n = top) %>%
      mutate(modele = nom)
  })
  
  resume <- top_vars_all %>%
    group_by(variable) %>%
    summarise(
      nombre_modeles = n(),
      frequence = n() / length(top_modele_names),
      importance_moyenne = mean(Overall),
      score_pondere = frequence * importance_moyenne,
      .groups = "drop"
    ) %>%
    arrange(desc(frequence), desc(score_pondere), variable) 
  
  list(
    resume = resume,
    details = top_vars_all
  )
}




save_plot <- function(plot_obj, filename, folder,width = 8, height = 6, dpi = 300) {
  if (!dir.exists(folder)) dir.create(folder, recursive = TRUE)
  ggsave(filename = file.path(folder, filename),plot = plot_obj,width = width,height = height,dpi = dpi)
}




#===============================================================================
# FONCTIONS PLOT
#===============================================================================


plot_volatilite <- function(df, nom) {
  ggplot(df, aes(x = date, y = volatilite)) +
    geom_line(color = "darkgreen", alpha = 0.7, linewidth = 0.6) +
    labs(title = paste("Volatilité quotidienne du", nom,"entre janvier 2010 et janvier 2025"),x = "Date",y = "Volatilité") +
    geom_smooth(formula = y ~ x, method = "loess", span = 0.1, color = "red", se = FALSE, linewidth = 1) +
    theme_minimal() +
    theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 14))
}



plot_AUC <- function(resultat, titre) {
  ggplot(resultat, aes(x = reorder(modele, AUC), y = AUC, fill = modele)) + geom_col(width = 0.6) + coord_flip() +
    geom_text(aes(label = round(AUC, 3)),hjust = 1.4, color = "white", size = 3.5, fontface = "bold") +
    labs(title = titre, x = "Modèle", y = "AUC") +
    theme_minimal() +
    theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 14))
}




plot_roc <- function(resultat, nom, titre) {
  roc_info <- purrr::keep(resultat$rocs, ~ .x$modele == nom)[[1]]
  
  roc_df <- data.frame(FPR = 1 - roc_info$ROC$specificities,TPR = roc_info$ROC$sensitivities)
  
  auc <- round(roc_info$AUC, 3)
  
  ggplot(roc_df, aes(x = FPR, y = TPR)) +geom_line(color = "red", linewidth = 1) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey") +coord_equal() +
    labs(title = titre ,subtitle = paste(" valeur AUC =", auc),x = "False positive rate",y = "True positive rate"
    ) + theme_minimal()+
    theme(axis.text.x = element_text(angle = 45, hjust = 1),plot.title = element_text(face = "bold", hjust = 0.5, size = 14),plot.subtitle = element_text(hjust = 0.5, size = 11))
}





plot_heatmap <- function(confusions_list, auc_table, titre) {
  
  stats_df <- purrr::map_dfr(confusions_list, function(cm) {tibble::tibble(
    Accuracy = cm$overall['Accuracy'],
    Sensitivity = cm$byClass['Sensitivity'],
    Specificity = cm$byClass['Specificity'],
    Balanced_Acc = cm$byClass['Balanced Accuracy'])},
    .id = "modele")
  
 
  stats_df <- stats_df %>%
    left_join(auc_table, by = "modele")
  
  stats_df[is.na(stats_df)] <- 0
  
  stats_long <- stats_df %>%
    pivot_longer(cols = -modele, names_to = "observation", values_to = "valeur")
  
  model_order <- stats_df %>%
    arrange(desc(AUC)) %>%
    pull(modele)
  
  stats_long$modele <- factor(stats_long$modele, levels = model_order)
  
  palette <- c("#f7fcf5", "#74c476", "#00441b")
  
  ggplot(stats_long, aes(x = observation, y = modele, fill = valeur)) +geom_tile(color = "white", linewidth = 0.5) +
    geom_text(aes(label = round(valeur, 2)), color = "white", size = 3.5, fontface = "bold") +
    scale_fill_gradientn(colors = palette,limits = c(0, 1),na.value = "grey") +
    labs(
      title = titre,x = "performance",y = "Modèles",fill = "Valeur") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),axis.text.y = element_text(face = "bold"),
          plot.title = element_text(size = 14, face = "bold", hjust = 0.5),legend.position = "bottom",legend.title = element_text(face = "bold"))
}



plot_importance <- function(modele, nom, top, titre) {
  imp <- importance_var(modele, nom)
  
  imp %>%
    slice_head(n = top) %>%
    ggplot(aes(x = reorder(variable, -Overall), y = Overall, fill = variable)) +
    geom_col(width = 0.7, show.legend = FALSE) +
    geom_text(aes(label = paste0(round(Overall))),vjust = -0.4,size = 2.5,fontface = "bold") +
    labs(title = titre,
         subtitle = paste("Top", top, " des variables les plus importantes pour le modèle"),
         x = "Variable",y = "Importance") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),plot.title = element_text(face = "bold", hjust = 0.5, size = 14),plot.subtitle = element_text(hjust = 0.5, size = 11))
}



plot_importances_top <- function(analyse, top, titre) {
  analyse$resume %>%
    slice_head(n = top) %>%
    ggplot(aes(x = reorder(variable, -frequence), y = frequence, fill = variable)) +
    geom_col(width = 0.7, show.legend = FALSE) +
    geom_text(aes(label = paste0(round(frequence * 100, 0), "%")),
              vjust = -0.4,size = 3.5,fontface = "bold") +
    scale_y_continuous(labels = scales::percent,limits = c(0, 1.05)) +
    labs(title = titre,
         subtitle = paste("Top", top, " des variables les plus récurrentes parmi les meilleurs modèles"),x = "Variable",
         y = "Fréquence d'apparition (en % des modèles)") +
    theme_minimal() +
    theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 14),plot.subtitle = element_text(hjust = 0.5, size = 11),
          axis.text.x = element_text(angle = 45, hjust = 1))
}



#===============================================================================
# VERIFICATION DES DATA FRAMES
#===============================================================================






cat("Répartition des classes (train / test) CAC40:\n")
print(prop.table(table(CAC40_train$classe)))
print(prop.table(table(CAC40_test$classe)))

cat("Répartition des classes (train / test) DAX:\n")
print(prop.table(table(DAX_train$classe)))
print(prop.table(table(DAX_test$classe)))

cat("Répartition des classes (train / test) S&P500:\n")
print(prop.table(table(SP500_train$classe)))
print(prop.table(table(SP500_test$classe)))



#===============================================================================
# MODELISATION POUR LE CAC 40 
#===============================================================================



plot_volatilite(CAC40, "CAC40")
save_plot(plot_volatilite(CAC40, "CAC40"), "volatilite_CAC40.png",folder = "RESULTATS/CAC40")




controle_CAC40 <- train_control(nrow(CAC40_train),0.8,5)
modele_CAC40 <- suppressWarnings(suppressMessages(train_all_modele(CAC40_train, c("glm","glmnet", "rpart", "rf", "gbm", "xgbTree"), controle_CAC40)))



resultat_CAC40 <- comparaison_modele(modele_CAC40, CAC40_test)
resultat_CAC40$table




modele_a_print_cac40 <- best_of3(resultat_CAC40) 

for (m in modele_a_print_cac40){
  print(plot_roc(resultat_CAC40,m,paste("courbe ROC pour le modèle", m, "évalué sur le CAC40")))
  save_plot(plot_roc(resultat_CAC40,m,paste("courbe ROC pour le modèle", m, "évalué sur le CAC40")), paste0("ROC_",m,"_CAC40.png"),folder = "RESULTATS/CAC40")
}




plot_AUC(resultat_CAC40$table , "Comparaison des differents modèles pour le CAC40")
save_plot(plot_AUC(resultat_CAC40$table , "Comparaison des differents modèles pour le CAC40"), "AUC_comparaison_CAC40.png",folder ="RESULTATS/CAC40")



plot_heatmap(resultat_CAC40$confusion, resultat_CAC40$table,"Heatmap des performances du CAC40")
save_plot(plot_heatmap(resultat_CAC40$confusion, resultat_CAC40$table,"Heatmap des performances du CAC40"), "heatmap_performance_CAC40.png",folder = "RESULTATS/CAC40")







for (m in modele_a_print_cac40) {
  print(plot_importance(modele_CAC40, m, top = 10, paste("Variables importantes -", m, "- CAC40")))
  save_plot(plot_importance(modele_CAC40, m, top = 10, paste("Variables importantes -", m, "- CAC40")), paste0("importance_des_variables_", m, "_CAC40.png"),folder = "RESULTATS/CAC40")
}





importance_CAC40_top3 <- importances_top(modele_CAC40, modele_a_print_cac40, top = 5)
plot_importances_top(importance_CAC40_top3, top = 5, titre = "Variables importantes - CAC40")
save_plot(plot_importances_top(importance_CAC40_top3, top = 5, titre = "Variables importantes - CAC40"),"top_variables_CAC40.png",folder = "RESULTATS/CAC40")



#===============================================================================
# MODELISATION POUR LE DAX 
#===============================================================================


plot_volatilite(DAX, "DAX")
save_plot(plot_volatilite(DAX, "DAX"), "volatilite_DAX.png",folder = "RESULTATS/DAX")




controle_DAX <- train_control(nrow(DAX_train),0.8,5)
modele_DAX <- suppressWarnings(suppressMessages(train_all_modele(DAX_train, c("glm", "glmnet", "rpart", "rf", "gbm", "xgbTree"), controle_DAX)))



resultat_DAX <-comparaison_modele(modele_DAX, DAX_test)
resultat_DAX$table



modele_a_print_dax <- best_of3(resultat_DAX)

for (m in modele_a_print_dax){
  print(plot_roc(resultat_DAX,m,paste("courbe ROC pour le modèle", m, "évalué sur le DAX")))
  save_plot(plot_roc(resultat_DAX,m,paste("courbe ROC pour le modèle", m, "évalué sur le DAX")), paste0("ROC_",m,"_DAX.png"),folder = "RESULTATS/DAX")
}




plot_AUC(resultat_DAX$table, "Comparaison des differents modèles pour le DAX")
save_plot(plot_AUC(resultat_DAX$table, "Comparaison des differents modèles pour le DAX"), "AUC_comparaison_DAX.png",folder = "RESULTATS/DAX")



plot_heatmap(resultat_DAX$confusion, resultat_DAX$table,"Heatmap des performances du DAX")
save_plot(plot_heatmap(resultat_DAX$confusion, resultat_DAX$table,"Heatmap des performances du DAX"), "heatmap_performance_DAX.png",folder = "RESULTATS/DAX")




for (m in modele_a_print_dax) {
  print(plot_importance(modele_DAX, m, top = 10, titre = paste("Variables importantes -", m, "- DAX")))
  save_plot(plot_importance(modele_DAX, m, top = 10, titre = paste("Variables importantes -", m, "- DAX")), paste0("importance_des_variables_", m, "_DAX.png"),folder = "RESULTATS/DAX")
}





importance_DAX_top3 <- importances_top(modele_DAX, modele_a_print_dax, top = 5)
plot_importances_top(importance_DAX_top3, top = 5, titre = "Variables importantes - DAX")
save_plot(plot_importances_top(importance_DAX_top3, top = 5, titre = "Variables importantes - DAX"),"top_variables_DAX.png",folder = "RESULTATS/DAX")



#===============================================================================
# MODELISATION POUR LE S&P500
#===============================================================================


plot_volatilite(SP500, "S&P500")
save_plot(plot_volatilite(SP500, "S&P500"), "volatilite_SP500.png",folder = "RESULTATS/SP500")



controle_SP500 <- train_control(nrow(SP500_train),0.8,5)
modele_SP500 <- suppressWarnings(suppressMessages(train_all_modele(SP500_train, c("glm", "glmnet", "rpart", "rf", "gbm", "xgbTree"), controle_SP500)))



resultat_SP500 <- comparaison_modele(modele_SP500, SP500_test)
resultat_SP500$table




modele_a_print_sp500 <- best_of3(resultat_SP500) 

for (m in modele_a_print_sp500){
  print(plot_roc(resultat_SP500,m,paste("courbe ROC pour le modèle", m, "évalué sur le S&P500")))
  save_plot(plot_roc(resultat_SP500,m,paste("courbe ROC pour le modèle", m, "évalué sur le S&P500")), paste0("ROC_",m,"_SP500.png"),folder = "RESULTATS/SP500")
}




plot_AUC(resultat_SP500$table, "Comparaison des differents modèles pour le S&P500")
save_plot(plot_AUC(resultat_SP500$table, "Comparaison des differents modèles pour le S&P500"), "AUC_comparaison_SP500.png",folder = "RESULTATS/SP500")





plot_heatmap(resultat_SP500$confusion, resultat_SP500$table,"Heatmap des performances du S&P500")
save_plot(plot_heatmap(resultat_SP500$confusion, resultat_SP500$table,"Heatmap des performances du S&P500"), "heatmap_performance_SP500.png",folder = "RESULTATS/SP500")




for (m in modele_a_print_sp500) {
  print(plot_importance(modele_SP500, m, top = 10, titre = paste("Variables importantes -", m, "- SP500")))
  save_plot(plot_importance(modele_SP500, m, top = 10, titre = paste("Variables importantes -", m, "- SP500")), paste0("importance_des_variables_", m, "_SP500.png"),folder = "RESULTATS/SP500")
}




importance_SP500_top3 <- importances_top(modele_SP500, modele_a_print_sp500, top = 5)
plot_importances_top(importance_SP500_top3, top = 5, titre = "Variables importantes - S&P500")
save_plot(plot_importances_top(importance_SP500_top3, top = 5, titre = "Variables importantes - S&P500"),"top_variables_SP500.png",folder = "RESULTATS/SP500")




