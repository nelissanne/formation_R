# GESTION DE l'ENVIRONNEMENT ------------

library(dplyr)
library(ggplot2)

api_token=yaml::read_yaml("secrets.yaml")$API_TOKEN

# SOURCE FONCTIONS ---------------

source("R/functions.R", encoding = "UTF-8")

# IMPORT DES DONNEES ---------------

df <- arrow::read_parquet(
  "data/individu_reg.parquet",
  col_select = c("region", "aemm", "aged", "anai",
                 "catl", "cs1", "cs2", "cs3", "couple",
                 "na38", "naf08", "pnai12", "sexe",
                 "surf", "tp", "trans", "ur")
)

# RETRAITEMENT DES DONNEES  ---------------

df <- df %>%
  mutate(aged = as.numeric(aged))

# stats trans par statut
df3 <- df %>%
  group_by(couple, trans) %>%
  summarise(x = n()) %>%
  group_by(couple) %>%
  mutate(y = 100 * x / sum(x))

df$sexe <- df$sexe %>%
  as.character() %>%
  forcats::fct_recode(Homme = "1", Femme = "2")

# STATS DESC  ---------------

## AGE ========

summarise(group_by(df, aged), n())

stats_age <- df %>%
  group_by(decennie = decennie_a_partir_annee(aged)) %>%
  summarise(n())

table_age <- gt::gt(stats_age) %>%
  gt::tab_header(
    title = "Distribution des âges dans notre population"
  ) %>%
  gt::fmt_number(
    columns = `n()`,
    sep_mark = " ",
    decimals = 0
  ) %>%
  gt::cols_label(
    decennie = "Tranche d'âge",
    `n()` = "Population"
  )

## SEXE =========

indicateur_stat(df %>% filter(sexe == "Homme") %>% pull(aged))
indicateur_stat(df %>% filter(sexe == "Femme") %>% pull(aged))


# GRAPHIQUES  ---------------

ggplot(df) +
  geom_histogram(aes(x = 5 * floor(as.numeric(aged) / 5)), stat = "count")


p <- # part d'homme dans chaque cohort
  df %>%
  group_by(aged, sexe) %>%
  summarise(SH_sexe = n()) %>%
  group_by(aged) %>%
  mutate(SH_sexe = SH_sexe / sum(SH_sexe)) %>%
  filter(sexe == 1) %>%
  ggplot() +
  geom_bar(aes(x = aged, y = SH_sexe), stat = "identity") +
  geom_point(aes(x = aged, y = SH_sexe), stat = "identity", color = "red") +
  coord_cartesian(c(0, 100))


ggsave("p.png", p)


# MODELISATION  ---------------

df3 <- df %>%
  select(surf, cs1, ur, couple, aged) %>%
  filter(surf != "Z")
df3[, "surf"] <- factor(df3$surf, ordered = TRUE)
df3[, "cs1"] <- factor(df3$cs1)
df3 %>%
  filter(couple == "2" & aged > 40 & aged < 60)
MASS::polr(surf ~ cs1 + factor(ur), df3)
