# =============================================================================
# PROJET     : Panel Déséquilibré · Données Fiscales UEMOA 2010-2022
# Auteur     : NdiayeMameDiarra
# Date       : 2026
# Cours      : Traitement Statistique avec R — ISEP2
# Question   : Le panel fiscal UEMOA est-il exploitable ?
# =============================================================================
# ── INSTALLATION DES PACKAGES ─────────────────────────────────────────────────
packages <- c(
  "WDI",        # Données Banque Mondiale
  "mice",       # Imputation multiple
  "naniar",     # Visualisation données manquantes
  "visdat",     # Visualisation données manquantes
  "plm",        # Modèles de panel
  "MissMech",   # Test MCAR de Little
  "tidyverse",  # Manipulation des données
  "ggplot2",    # Graphiques
  "patchwork"   # Combiner les graphiques
)

# Installer uniquement les packages manquants
packages_manquants <- packages[!packages %in%
                                 installed.packages()]
if(length(packages_manquants) > 0){
  install.packages(packages_manquants)
}
# ── CHARGEMENT DES PACKAGES ───────────────────────────────────────────────────
library(WDI)
library(mice)
library(naniar)
library(visdat)
library(plm)
library(MissMech)
library(tidyverse)
library(ggplot2)
library(patchwork)
# ── TÉLÉCHARGEMENT DES DONNÉES WDI ────────────────────────────────────────────

# Les 8 pays UEMOA
pays_uemoa <- c("BJ","BF","CI","GW","ML","NE","SN","TG")

# Les indicateurs
indicateurs <- c(
  "GC.TAX.TOTL.GD.ZS",  # Recettes fiscales (% PIB)
  "GC.REV.XGRT.GD.ZS",  # Recettes totales (% PIB)
  "GC.XPN.TOTL.GD.ZS",  # Dépenses publiques (% PIB)
  "GC.NLD.TOTL.GD.ZS",  # Solde budgétaire (% PIB)
  "GC.DOD.TOTL.GD.ZS",  # Dette publique (% PIB)
  "NY.GDP.MKTP.KD.ZG",  # Croissance du PIB (%)
  "FP.CPI.TOTL.ZG",     # Inflation (%)
  "NE.TRD.GNFS.ZS",     # Commerce (% PIB)
  "SP.POP.TOTL"         # Population totale
)

# Téléchargement
wdi_raw <- WDI(
  country   = pays_uemoa,
  indicator = indicateurs,
  start     = 2010,
  end       = 2022
)
# ── VÉRIFICATION DES DONNÉES ──────────────────────────────────────────────────

# Dimensions du tableau
cat("Nombre de lignes :", nrow(wdi_raw), "\n")
cat("Nombre de colonnes :", ncol(wdi_raw), "\n")

# Aperçu des premières lignes
head(wdi_raw)

# Aperçu visuel complet
View(wdi_raw)

# Résumé statistique
summary(wdi_raw)