# LOT 1 : EXPLORATION ET PRÉPARATION

# 1. Chargement des bibliothèques
library(tidyverse)

# 2. Importation des données
Index <- read_csv("AI_index_db.csv")
Impact <- read_csv("Global_AI_Content_Impact_Dataset.csv")

# 3. Nettoyage des noms de pays 
# On aligne df_impact sur les noms de df_index pour éviter de perdre des données
Impact <- Impact %>%
  mutate(Country = case_when(
    Country == "USA" ~ "United States",
    Country == "UK"  ~ "United Kingdom",
    TRUE ~ Country
  ))

# 4. Jointure des fichiers 
# On utilise left_join pour ne garder que les pays présents dans les deux tables
Complet <- left_join(Impact, Index, by = "Country")

# 5. Transformation de la variable Régulation 
# On transforme le texte en facteur numérique pour les futures statistiques
Complet <- Complet %>%
  mutate(
    `AI Talent Index` = Talent, 
    `Adoption Rate (%)` = `AI Adoption Rate (%)`,
    `Public Trust Score (%)` = `Consumer Trust in AI (%)`,
    Reg_Score = case_when(
      `Regulation Status` == "Strict" ~ 3,
      `Regulation Status` == "Moderate" ~ 2,
      `Regulation Status` == "Lenient" ~ 1,
      TRUE ~ 0
    )
  )

# 6. Vérification du résultat
print(head(Complet$`AI Talent Index`))

# Sauvegarde de la base propre pour le Lot 2
write_csv(Complet, "Complet_final.csv")


# LOT 2 : Statistiques Descriptives et Premiers Graphiques

# 1. Analyse Univariée : Répartition par Secteur
# On regarde quels secteurs (Industry) sont les plus présents
ggplot(Complet, aes(x = fct_infreq(Industry))) +
  geom_bar(fill = "darkgreen") +
  coord_flip() +
  labs(title = "Répartition des observations par Secteur",
       x = "Secteurs d'activité", 
       y = "Nombre de lignes") +
  theme_minimal()

# 2. Analyse Bivariée : Talent vs Adoption 
# ATTENTION : Utilise bien les ` ` pour les noms avec espaces
ggplot(Complet, aes(x = `AI Talent Index`, y = `Adoption Rate (%)`)) +
  geom_jitter(aes(color = Industry), width = 0.5, alpha = 0.6) + 
  geom_smooth(method = "lm", color = "darkred", se = FALSE) + 
  labs(title = "Analyse de la corrélation : Talent vs Adoption",
       subtitle = "Source : AI Index & Global Impact Dataset",
       x = "Index de Talent (National)",
       y = "Taux d'Adoption (%)",
       color = "Secteur") +
  theme_minimal()

# 3. Analyse Bivariée : Régulation vs Confiance 
# On utilise le Reg_Score (1, 2, 3) créé au Lot 1
ggplot(Complet, aes(x = as.factor(Reg_Score), y = `Public Trust Score (%)`)) +
  geom_boxplot(aes(fill = as.factor(Reg_Score)), alpha = 0.7, show.legend = FALSE) +
  scale_x_discrete(labels = c("1" = "Lenient", "2" = "Moderate", "3" = "Strict")) +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "La régulation influence-t-elle la confiance ?",
       x = "Niveau de Régulation", y = "Score de Confiance (%)") +
  theme_light()

# 4.Top 5 des secteurs qui adoptent le plus l'IA
Complet %>%
  group_by(Industry) %>%
  summarise(Moyenne_Adoption = mean(`Adoption Rate (%)`, na.rm = TRUE)) %>%
  slice_max(Moyenne_Adoption, n = 5) %>%
  ggplot(aes(x = reorder(Industry, Moyenne_Adoption), y = Moyenne_Adoption)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Top 5 des Secteurs leaders en Adoption",
       x = "Secteur", y = "Taux d'Adoption Moyen (%)") +
  theme_minimal()

# LOT 3 : STATISTIQUES INFÉRENTIELLES (TESTS)

# 1. Test ANOVA : La régulation a-t-elle un impact significatif sur la confiance ?
# On compare les moyennes des 3 groupes
anova_confiance <- aov(`Public Trust Score (%)` ~ as.factor(Reg_Score), data = Complet)
summary(anova_confiance)

# 2. Test de Corrélation : Le lien Talent / Adoption est-il statistiquement solide ?
test_cor <- cor.test(Complet$`AI Talent Index`, Complet$`Adoption Rate (%)`)
print(test_cor)

# 3. Interprétation rapide :
# Si "Pr(>F)" (p-value) est < 0.05, alors l'impact est statistiquement significatif !