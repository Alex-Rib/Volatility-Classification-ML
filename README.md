# Volatilité boursière et facteurs exogènes : une analyse comparative sur le CAC40, le DAX et le S&P500

![R](https://img.shields.io/badge/R-4.0%2B-blue)
![Machine Learning](https://img.shields.io/badge/Machine_Learning-Classification-orange)
![Finance](https://img.shields.io/badge/Finance-Market_Prediction-green)
![Tidyverse](https://img.shields.io/badge/Tidyverse-Enabled-9cf)
![Status](https://img.shields.io/badge/Status-Educational-yellow)

Projet d'analyse cherchant à regarder s'il existe une relation entre la volatilité des marchés boursiers (CAC40, DAX, S&P500) les conditions météorologiques(dans les villes hôtes de ces indices : Paris, Francfort etNew York) et le calendrier.

## 📊 Description

Ce projet combine des données météorologiques et des données calendaires avec des données boursières pour entraîner des modèles de machine learning dans l'objectif de voir s'il est possible de prédire un régime de volatilité à l'aide de ces facteurs. 

### Indices analysés
- **CAC40** (France) - Paris
- **DAX** (Allemagne) - Francfort  
- **S&P500** (USA) - New York

### Période d'analyse
Janvier 2010 - Janvier 2025

## 🚀 Installation

### Prérequis
- R (version 4.0 ou supérieure)
- RStudio (recommandé)

### Packages R requis

Le script installe automatiquement les packages manquants, mais vous pouvez les installer manuellement :

```r
install.packages(c(
  "httr", "jsonlite", "dplyr", "lubridate", "tidyquant", 
  "TTR", "PerformanceAnalytics", "caret", "tibble", "ggplot2",
  "tidyr", "xgboost", "randomForest", "gbm", "pROC", "purrr", "scales"
))
```

## 📝 Utilisation

### Étape 1 : Préparation des données

Exécutez d'abord le script de préparation des données :

```bash
Rscript data.frame.R
```

Ce script :
- Récupère les données météorologiques via l'API Open-Meteo
- Télécharge les données boursières via Yahoo Finance
- Calcule la volatilité (méthode Garman-Klass)
- Crée les ensembles train/test
- Sauvegarde les datasets dans le dossier `DATA/`

### Étape 2 : Modélisation et analyse

Ensuite, exécutez le script d'analyse :

```bash
Rscript volatility_and_weather.R
```

Ce script :
- Charge les données préparées
- Entraîne 6 modèles différents (GLM, GLMNet, RPART, Random Forest, GBM, XGBoost)
- Évalue les performances (AUC, ROC, matrice de confusion)
- Génère les visualisations
- Sauvegarde les résultats dans `RESULTATS/`

## 📂 Structure du projet

```
volatility/
├── README.md
├── data.frame.R              # Préparation des données
├── volatility_and_weather.R  # Modélisation et analyse
├── DATA/                     # Datasets générés (non versionné)
│   ├── CAC40.rds
│   ├── DAX.rds
│   ├── SP500.rds
│   └── *_train.rds / *_test.rds
└── RESULTATS/                # Graphiques générés (non versionné)
    ├── CAC40/
    ├── DAX/
    └── SP500/
```

## 🔍 Méthodologie

### Variables utilisées
- **Volatilité** : Calculée avec la méthode Garman-Klass
- **Météo** : Température, précipitations
- **Temporelles** : Jour de la semaine, mois, saison

### Modèles entraînés
1. Régression logistique (GLM)
2. Régression logistique régularisée (GLMNet)
3. Arbre de décision (RPART)
4. Forêt aléatoire (Random Forest)
5. Gradient Boosting Machine (GBM)
6. XGBoost

### Validation
- Split temporel 80/20
- Validation croisée avec fenêtre glissante (time series)
- Métriques : AUC, Sensibilité, Spécificité, Accuracy

## 📈 Résultats

Les résultats incluent :
- Courbes ROC pour chaque modèle
- Comparaison des performances (AUC)
- Heatmaps des métriques
- Importance des variables
- Graphiques de volatilité temporelle

## 🌐 Sources de données

- **Données météo** : [Open-Meteo Archive API](https://open-meteo.com/)
- **Données boursières** : Yahoo Finance (via package `tidyquant`)

## 👨‍💻 Auteur

Alexandre R. - Master mathématiques appliquées - Université Paris Cité
