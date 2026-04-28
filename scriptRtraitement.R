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
# =============================================================================
# ÉTAPE 1 — Telecharment des donnees
# =============================================================================
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

# Créer le dossier data s'il n'existe pas
dir.create("data", showWarnings = FALSE)

# Sauvegarder les données
write.csv(wdi_raw, "data/wdi_raw.csv", row.names = FALSE)

cat("Données sauvegardées dans data/wdi_raw.csv\n")

# =============================================================================
# ÉTAPE 2 — CARTOGRAPHIE DES DONNÉES MANQUANTES
# =============================================================================

# ── 2.1 RENOMMER LES COLONNES ─────────────────────────────────────────────────
# On renomme les colonnes pour que ce soit plus lisible

panel <- wdi_raw %>%
  rename(
    pays            = country,
    annee           = year,
    recettes_fisc   = GC.TAX.TOTL.GD.ZS,
    recettes_totales= GC.REV.XGRT.GD.ZS,
    depenses        = GC.XPN.TOTL.GD.ZS,
    solde_budg      = GC.NLD.TOTL.GD.ZS,
    dette           = GC.DOD.TOTL.GD.ZS,
    croissance_pib  = NY.GDP.MKTP.KD.ZG,
    inflation       = FP.CPI.TOTL.ZG,
    commerce        = NE.TRD.GNFS.ZS,
    population      = SP.POP.TOTL
  ) %>%
  arrange(iso2c, annee)

# Vérification
head(panel)
# ── 2.2 RÉSUMÉ DES DONNÉES MANQUANTES ────────────────────────────────────────

# Pourcentage de NA par variable
resume_na <- panel %>%
  select(recettes_fisc, recettes_totales,
         depenses, solde_budg, dette,
         croissance_pib, inflation) %>%
  miss_var_summary()

print(resume_na)
# ── 2.3 NA PAR PAYS ───────────────────────────────────────────────────────────

na_par_pays <- panel %>%
  group_by(pays) %>%
  summarise(
    pct_na_recettes = round(
      mean(is.na(recettes_fisc)) * 100, 1),
    pct_na_depenses = round(
      mean(is.na(depenses)) * 100, 1),
    pct_na_dette    = round(
      mean(is.na(dette)) * 100, 1)
  ) %>%
  arrange(desc(pct_na_recettes))

print(na_par_pays)
# ── 2.4 GRAPHIQUE 1 : Vue globale des manquants ───────────────────────────────

vis_miss(panel %>%
           select(pays, annee,
                  recettes_fisc, recettes_totales,
                  depenses, solde_budg, dette,
                  croissance_pib, inflation))
# ── 2.5 GRAPHIQUE 2 : Carte thermique par pays et année ──────────────────────

p_heatmap <- panel %>%
  mutate(manquant = is.na(recettes_fisc)) %>%
  ggplot(aes(x = annee,
             y = pays,
             fill = manquant)) +
  geom_tile(color = "white",
            linewidth = 0.5) +
  scale_fill_manual(
    values = c("FALSE" = "#2196F3",
               "TRUE"  = "#F44336"),
    labels = c("Disponible", "Manquant")
  ) +
  scale_x_continuous(breaks = 2010:2022) +
  labs(
    title    = "Données manquantes · Recettes Fiscales UEMOA",
    subtitle = "2010–2022 · 8 pays",
    x        = "Année",
    y        = "Pays",
    fill     = "Statut"
  ) +
  theme_minimal() +
  theme(
    axis.text.x     = element_text(angle = 45,
                                   hjust = 1),
    plot.title      = element_text(face = "bold"),
    legend.position = "bottom"
  )

print(p_heatmap)

# Sauvegarder le graphique
dir.create("outputs", showWarnings = FALSE)
ggsave("outputs/heatmap_manquants.png",
       p_heatmap,
       width  = 10,
       height = 6,
       dpi    = 300)

cat("Graphique sauvegardé dans outputs/\n")

