library(readxl)
library(dplyr)

#SCOFF ####
# Lecture complète (si besoin plus tard)
Meta_SCOFF_all <- readxl::read_excel("./Raw_data/Metaanalyse_bibliographique.xlsx", sheet = 2) 


  
# Filtrer les autres études
Meta_SCOFF <- Meta_SCOFF_all %>%
  filter(Category != "Our_study") %>%
  mutate(
    SCOFF_sup_2_number = as.numeric(SCOFF_sup_2_number),
    SCOFF_sup_2_proportion = as.numeric(SCOFF_sup_2_proportion),
    Number_of_participants = as.numeric(Number_of_participants),
    SCOFF_sup_2_number = ifelse(
      is.na(SCOFF_sup_2_number),
      SCOFF_sup_2_proportion * Number_of_participants,
      SCOFF_sup_2_number
    )
  )

# Récupérer notre étude
SCOFF_our_study <- Meta_SCOFF_all %>%
  filter(Category == "Our_study") %>%
  
  mutate(
    SCOFF_sup_2_number = as.numeric(SCOFF_sup_2_number),
    SCOFF_sup_2_proportion = as.numeric(SCOFF_sup_2_proportion),
    Number_of_participants = as.numeric(Number_of_participants),
    SCOFF_sup_2_number = ifelse(
      is.na(SCOFF_sup_2_number),
      SCOFF_sup_2_proportion * Number_of_participants,
      SCOFF_sup_2_number
    )
  )

# Variables de notre étude
our_n <- SCOFF_our_study$Number_of_participants
our_pos <- SCOFF_our_study$SCOFF_sup_2_number
our_neg <- our_n - our_pos
our_prop <- SCOFF_our_study$SCOFF_sup_2_proportion

# Initialiser les résultats
results_metaSCOFF <- Meta_SCOFF %>%
  mutate(
    chi2_stat = NA_real_,
    p_value = NA_real_,
    proportion_ratio = NA_real_
  )

# Boucle pour le test du Chi² et le ratio
for (i in seq_len(nrow(Meta_SCOFF))) {
  study_n <- Meta_SCOFF$Number_of_participants[i]
  study_pos <- Meta_SCOFF$SCOFF_sup_2_number[i]
  study_neg <- study_n - study_pos
  study_prop <- Meta_SCOFF$SCOFF_sup_2_proportion[i]
  
  # Tableau de contingence
  contingency_table <- matrix(c(study_pos, study_neg, our_pos, our_neg), nrow = 2, byrow = TRUE)
  
  # Test du Chi²
  test <- chisq.test(contingency_table, correct = FALSE)
  
  # Stockage des résultats
  results_metaSCOFF$chi2_stat[i] <- as.numeric(test$statistic)
  results_metaSCOFF$p_value[i] <- test$p.value
  results_metaSCOFF$proportion_ratio[i] <- our_prop / study_prop
}

# Ajouter la ligne de notre étude pour comparaison
results_metaSCOFF <- bind_rows(
  results_metaSCOFF,
  SCOFF_our_study %>%
    mutate(
      chi2_stat = NA_real_,
      p_value = NA_real_,
      proportion_ratio = 1  # ratio de soi à soi = 1
    )
)

# Ajouter colonne de rang basé sur SCOFF_sup_2_proportion (1 = plus élevée)
results_metaSCOFF <- results_metaSCOFF %>%
  arrange(desc(SCOFF_sup_2_proportion)) %>%
  mutate(
    rank_SCOFF_prop = row_number(),
    significant = ifelse(is.na(p_value), NA, p_value < 0.05)
  ) %>%
  mutate(adj_p_val = p.adjust(p_value))

# Affichage des colonnes clés
results_metaSCOFF %>%
  writexl::write_xlsx("./Result/results_metaSCOFF.xlsx")


# MBISS ####

MBISS <-
  readxl::read_excel("./Raw_data/Metaanalyse_bibliographique.xlsx", sheet = 3) 

# Fonction t-test de Welch analytique à partir de moyennes, SD et effectifs
welch_t_test <- function(mean1, sd1, n1, mean2, sd2, n2) {
  if (n1 <= 1 || n2 <= 1 || sd1 == 0 || sd2 == 0) {
    return(list(t = NA_real_, p = NA_real_, df = NA_real_))
  }
  
  se1 <- (sd1^2) / n1
  se2 <- (sd2^2) / n2
  
  t_stat <- (mean1 - mean2) / sqrt(se1 + se2)
  
  df <- (se1 + se2)^2 / ((se1^2) / (n1 - 1) + (se2^2) / (n2 - 1))
  
  p_val <- 2 * pt(-abs(t_stat), df)
  
  list(t = t_stat, p = p_val, df = df)
}

# Nettoyage + conversion numériques
MBISS <- MBISS %>%
  mutate(
    AE_mean = as.numeric(AE_mean),
    AE_SD = as.numeric(AE_SD),
    CY_mean = as.numeric(CY_mean),
    CY_SD = as.numeric(CY_SD),
    EE_mean = as.numeric(EE_mean),
    EE_SD = as.numeric(EE_SD),
    nb_participants = as.numeric(nb_participants)
  )

# Séparation des données
MBISS_our_study <- MBISS %>% filter(Category == "Our_study")
MBISS_others <- MBISS %>% filter(Category != "Our_study")

# Initialisation résultats
results_MBISS <- MBISS_others %>%
  mutate(
    AE_t_stat = NA_real_, AE_p_value = NA_real_, AE_ratio = NA_real_,
    CY_t_stat = NA_real_, CY_p_value = NA_real_, CY_ratio = NA_real_,
    EE_t_stat = NA_real_, EE_p_value = NA_real_, EE_ratio = NA_real_
  )

# Boucle sur chaque étude autre que Our_study
for (i in seq_len(nrow(MBISS_others))) {
  study <- MBISS_others[i, ]
  
  # AE
  if (!any(is.na(c(study$AE_mean, study$AE_SD, study$nb_participants,
                   MBISS_our_study$AE_mean, MBISS_our_study$AE_SD, MBISS_our_study$nb_participants)))) {
    t_ae <- welch_t_test(
      mean1 = study$AE_mean, sd1 = study$AE_SD, n1 = study$nb_participants,
      mean2 = MBISS_our_study$AE_mean, sd2 = MBISS_our_study$AE_SD, n2 = MBISS_our_study$nb_participants
    )
    results_MBISS$AE_t_stat[i] <- t_ae$t
    results_MBISS$AE_p_value[i] <- t_ae$p
    results_MBISS$AE_ratio[i] <- MBISS_our_study$AE_mean / study$AE_mean
  }
  
  # CY
  if (!any(is.na(c(study$CY_mean, study$CY_SD, study$nb_participants,
                   MBISS_our_study$CY_mean, MBISS_our_study$CY_SD, MBISS_our_study$nb_participants)))) {
    t_cy <- welch_t_test(
      mean1 = study$CY_mean, sd1 = study$CY_SD, n1 = study$nb_participants,
      mean2 = MBISS_our_study$CY_mean, sd2 = MBISS_our_study$CY_SD, n2 = MBISS_our_study$nb_participants
    )
    results_MBISS$CY_t_stat[i] <- t_cy$t
    results_MBISS$CY_p_value[i] <- t_cy$p
    results_MBISS$CY_ratio[i] <- MBISS_our_study$CY_mean / study$CY_mean
  }
  
  # EE
  if (!any(is.na(c(study$EE_mean, study$EE_SD, study$nb_participants,
                   MBISS_our_study$EE_mean, MBISS_our_study$EE_SD, MBISS_our_study$nb_participants)))) {
    t_ee <- welch_t_test(
      mean1 = study$EE_mean, sd1 = study$EE_SD, n1 = study$nb_participants,
      mean2 = MBISS_our_study$EE_mean, sd2 = MBISS_our_study$EE_SD, n2 = MBISS_our_study$nb_participants
    )
    results_MBISS$EE_t_stat[i] <- t_ee$t
    results_MBISS$EE_p_value[i] <- t_ee$p
    results_MBISS$EE_ratio[i] <- MBISS_our_study$EE_mean / study$EE_mean
  }
}

# Ajouter Our_study avec NA pour stats, ratio = 1
MBISS_our_study_results <- MBISS_our_study %>%
  mutate(
    AE_t_stat = NA_real_, AE_p_value = NA_real_, AE_ratio = 1,
    CY_t_stat = NA_real_, CY_p_value = NA_real_, CY_ratio = 1,
    EE_t_stat = NA_real_, EE_p_value = NA_real_, EE_ratio = 1
  )

results_MBISS <- bind_rows(results_MBISS, MBISS_our_study_results)

# Classement décroissant selon chaque moyenne
results_MBISS <- results_MBISS %>%
  arrange(desc(AE_mean)) %>%
  mutate(rank_AE = row_number()) %>%
  arrange(desc(CY_mean)) %>%
  mutate(rank_CY = row_number()) %>%
  arrange(desc(EE_mean)) %>%
  mutate(rank_EE = row_number()) %>%
  mutate(adj_CY_p_value = p.adjust(CY_p_value)) %>%
  mutate(adj_EE_p_value = p.adjust(EE_p_value)) %>%
  mutate(adj_AE_p_value = p.adjust(AE_p_value))

results_MBISS %>% 
  writexl::write_xlsx("./Result/results_MBISSmeta.xlsx")
# Résultat final — sélectionner colonnes clés

# WEMWBS ####

WEMWBS <-
  readxl::read_excel("./Raw_data/Metaanalyse_bibliographique.xlsx", sheet = 1) %>%
  dplyr::select(-...15, -doi)


# Nettoyage des données
WEMWBS <- WEMWBS %>%
  mutate(
    mean_score = as.numeric(mean_score),
    SD = as.numeric(SD),
    nb_participants = as.numeric(nb_participants)
  )

# Séparer notre étude
WEMWBS_our_study <- WEMWBS %>%
  filter(Category == "Our_study") 

# Études comparables = celles avec moyenne ET SD
WEMWBS_comparable <- WEMWBS %>%
  filter(Category != "Our_study") %>%
  filter(!is.na(mean_score) & !is.na(SD))

# Études avec moyenne mais sans SD
WEMWBS_mean_only <- WEMWBS %>%
  filter(Category != "Our_study") %>%
  filter(!is.na(mean_score) & is.na(SD))

# Études sans moyenne ni SD
WEMWBS_missing <- WEMWBS %>%
  filter(Category != "Our_study") %>%
  filter(is.na(mean_score) & is.na(SD))

# Test de Welch pour les études comparables
results_WEMWBS <- WEMWBS_comparable %>%
  mutate(
    t_stat = NA_real_,
    p_value = NA_real_,
    df = NA_real_,
    mean_ratio = NA_real_
  )

for (i in seq_len(nrow(WEMWBS_comparable))) {
  study <- WEMWBS_comparable[i, ]
  t_res <- welch_t_test(
    mean1 = study$mean_score,
    sd1 = study$SD,
    n1 = study$nb_participants,
    mean2 = WEMWBS_our_study$mean_score,
    sd2 = WEMWBS_our_study$SD,
    n2 = WEMWBS_our_study$nb_participants
  )
  
  results_WEMWBS$t_stat[i] <- t_res$t
  results_WEMWBS$p_value[i] <- t_res$p
  results_WEMWBS$df[i] <- t_res$df
  results_WEMWBS$mean_ratio[i] <- WEMWBS_our_study$mean_score / study$mean_score
}

# Ajouter notre étude
WEMWBS_our_study_results <- WEMWBS_our_study %>%
  mutate(
    t_stat = NA_real_,
    p_value = NA_real_,
    df = NA_real_,
    mean_ratio = 1
  )

# Ajouter aussi les études avec moyenne seule
WEMWBS_mean_only <- WEMWBS_mean_only %>%
  mutate(
    t_stat = NA_real_,
    p_value = NA_real_,
    df = NA_real_,
    mean_ratio = WEMWBS_our_study$mean_score / mean_score
  )

# Ajouter les études sans moyenne/SD
WEMWBS_missing <- WEMWBS_missing %>%
  mutate(
    t_stat = NA_real_,
    p_value = NA_real_,
    df = NA_real_,
    mean_ratio = NA_real_
  )

# Regroupement final
WEMWBS_final <- bind_rows(
  results_WEMWBS,
  WEMWBS_mean_only,
  WEMWBS_missing,
  WEMWBS_our_study_results
)

# Classement décroissant des scores moyens
WEMWBS_final <- WEMWBS_final %>%
  arrange(-desc(mean_score)) %>%
  mutate(rank_mean_score = row_number()) %>%
  mutate(adj_p_value = p.adjust(p_value))

View(WEMWBS_final)
WEMWBS_final %>%
  writexl::write_xlsx("./Result/WEMWBS_final.xlsx")

# Table 2 not used #


Onlystudentscoff <-
  Meta_SCOFF_all %>%
  filter( Category == "Student" | Category == "Our_study")

Onlystudentscoff <- 
  Onlystudentscoff %>%
  mutate(
    Health_student = ifelse(
      str_detect(tolower(Type_of_participants), "med|health|dental|pharmacy|veterinary"),
      "Health_student",
      "Other_student"
    )
  )

Onlystudentscoff_comparison <-
  Onlystudentscoff %>%
  select(SCOFF_sup_2_proportion,Health_student) %>%
  tbl_summary(by = "Health_student") %>%
  add_p() 

Studentvspop <-
  Meta_SCOFF_all %>%
  filter( Category != "Longitudinal_cohort_QLSCD") %>%
  filter( Category != "Other") %>%
  mutate(Category = ifelse(Category == "Youth", "Population", Category)) %>%
  mutate(Category = ifelse(Category == "Our_study", "Student", Category)) %>%
  select(SCOFF_sup_2_proportion,Category) %>%
  tbl_summary(by = "Category") %>%
  add_p() 


Meta_SCOFF_all %>%
  filter( Category != "Longitudinal_cohort_QLSCD") %>%
  filter( Category != "Other") %>%
  mutate(Category = ifelse(Category == "Youth", "Population", Category)) %>%
  dplyr::group_by(Category) %>%
  dplyr::summarize(n =n(),
                   mean_SCOFF_sup_2_proportion = mean(SCOFF_sup_2_proportion),
                   median = median(SCOFF_sup_2_proportion))



WEMWBS %>%
  filter( Category != "Other") %>%
  dplyr::group_by(Category) %>%
  dplyr::summarize(n =n(), mean = mean(mean_score, na.rm = T))


WEMWBS %>%
  filter( Category != "Other") %>%
  mutate(Category = ifelse(Category == "Our_study", "Student", Category)) %>%
  tbl_summary(by = "Category") %>%
  add_p() 


WEMWBS %>%
  filter( Category != "Other") %>%
  dplyr::group_by(Category) %>%
  dplyr::summarize(n =n(), mean = mean(mean_score, na.rm = T))


WEMWBS_student_only <-
  WEMWBS %>%
  filter( Category == "Student" | Category == "Our_study")
WEMWBS_student_only$Type_of_population
WEMWBS_student_only <- 
  WEMWBS_student_only %>%
  mutate(
    Health_student = ifelse(
      str_detect(tolower(Type_of_population), "med|health|dental|pharmacy|veterinary|nursing"),
      "Health_student",
      "Other_student"
    )
  )

# Onlystudentscoff_comparison <-
  WEMWBS_student_only %>%
  select(mean_score,Health_student) %>%
  tbl_summary(by = "Health_student") %>%
  add_p() 
  
  

  # Figure 3 ####
  
  category_colors <- c(
    "Population" = "#003366",              # bleu
    "Student" = "#cc5500",                 # orange
    "Longitudinal_cohort" = "#2ca02c", # vert
    "Other" = "#6A0DAD",                   # violet
    "Our_study" = "darkred"                # rouge
  )
  
  #SCOFF ####

  

  results_metaSCOFF_for_plot <- 
    results_metaSCOFF %>%
    mutate(Category = ifelse(Category == "Youth", "Population", Category)) %>%
    mutate(Category = ifelse(Category == "Longitudinal_cohort_QLSCD", "Longitudinal_cohort", Category)) %>%
    mutate(name = paste0(First_author, " et al, ",  Year_of_publication, "\n", Type_of_participants)) %>%
    mutate(name = make.unique(name)) %>%  # rendre les noms uniques
    mutate(color = Category) %>%
    mutate(name = gsub("_", " ", name)) %>%
    mutate(significancy = case_when(
      adj_p_val < 0.001 ~ "***",
      adj_p_val < 0.01  ~ "**",
      adj_p_val < 0.05  ~ "*",
      adj_p_val >= 0.05 ~ "NS",
      TRUE              ~ ""
    ),
    significancy = paste0(significancy, " (n=", Number_of_participants, ")")) %>%
    arrange(-desc(SCOFF_sup_2_proportion)) %>%
    mutate(name = factor(name, levels = name))  %>%
    mutate(
      signif_bold = ifelse(adj_p_val < 0.05, TRUE, FALSE),
      # Use markdown-style bold text for labels where applicable
      signif_label = ifelse(signif_bold, paste0("**", significancy, "**"), significancy)
    )
  
  
  barplot_scoff_meata <- 
    ggplot(results_metaSCOFF_for_plot, aes(x = name, y = SCOFF_sup_2_proportion, fill = Category)) +
    geom_bar(stat = "identity") +
    geom_hline(yintercept = 0.5049, linetype = "dashed", color = "darkred", linewidth = 0.5, alpha = 0.9) +
    geom_text(aes(label = significancy, 
                  fontface = ifelse(!is.na(adj_p_val) & adj_p_val < 0.05, "bold", "plain")), 
              vjust = 0.5, size = 4, hjust = -0.01) +
    geom_text(aes(label = scales::percent(SCOFF_sup_2_proportion, accuracy = 0.01)),
              position = position_stack(vjust = 0.86), # centers label vertically inside the bar
              color = "white", size = 4) +            # white text for contrast inside bars
    coord_flip(clip = "off") +
    labs(
      title = "Population at risk of eating disorder (SCOFF≥2)",
      x = "Study",
      y = "",
      fill = "Catégorie"
    ) +
    scale_y_continuous(
      labels = percent_format(accuracy = 1),
      limits = c(0, 1),
      expand = c(0, 0)
    ) +
    scale_fill_manual(values = category_colors) +  # apply your colors here
    theme(
      plot.margin = margin(10, 30, 10, 10),
      axis.text.y = element_text(size = 10, lineheight = 0.65),
      plot.title = element_text(hjust = 0.5)
    ) +
    theme_minimal() +
    my_theme +
    theme(legend.position = "none")
  

  barplot_scoff_meata_legend <-
    barplot_scoff_meata + 
    theme(legend.position = "left")
  
ggsave("./Result/Figures/Figuer3legend.png", barplot_scoff_meata_legend, width = 10, height = 10)
ggsave("./Result/Figures/Figuer3_barplot_scoff.png", barplot_scoff_meata, width = 15, height = 25)

# Warick ####

WEMWBS_final_for_plot <- 
  WEMWBS_final %>%
  filter(!is.na(mean_score)) %>%   # ← Remove NA mean_score
  mutate(Category = ifelse(Category == "Youth", "Population", Category)) %>%
  mutate(name = paste0(First_author, " et al, ",  Year_of_publication, "\n", Type_of_population)) %>%
  mutate(name = make.unique(name)) %>%  # rendre les noms uniques
  mutate(color = Category) %>%
  mutate(name = gsub("_", " ", name)) %>%
  mutate(significancy = case_when(
    is.na(adj_p_value)  & Category != "Our_study"     ~ "NA",
    adj_p_value < 0.001 ~ "***",
    adj_p_value < 0.01  ~ "**",
    adj_p_value < 0.05  ~ "*",
    adj_p_value >= 0.05 ~ "NS",
    TRUE              ~ ""
  ),
  significancy = paste0(significancy, " (n=", nb_participants, ")")) %>%
  arrange(-desc(mean_score)) %>%
  mutate(name = factor(name, levels = name))  %>%
  mutate(
    signif_bold = ifelse(adj_p_value < 0.05, TRUE, FALSE),
    # Use markdown-style bold text for labels where applicable
    signif_label = ifelse(signif_bold, paste0("**", significancy, "**"), significancy)
  )

WEMWBS_final_plot_final <-
  ggplot(WEMWBS_final_for_plot, aes(x = mean_score, y = name)) +
  geom_point(size = 4, aes(color = color)) +
  geom_errorbarh(aes(xmin = mean_score - SD, xmax = mean_score + SD, color = color), height = 0.3) +
  geom_text(aes(label = significancy, 
            fontface = ifelse(!is.na(adj_p_value) & adj_p_value < 0.05, "bold", "plain")), 
            hjust = -0.5, vjust = -0.1, 
            size = 4) +
  geom_vline(xintercept = 41.21, linetype = "dashed", color = "darkred", linewidth = 0.7) +  # cutoff line
  scale_x_continuous(limits = c(14, 70), breaks = seq(14, 70, by = 6)) +
  labs(
    title = "WEMWBS Scores Across Studies",
    x = "WEMWBS Score",
    y = "",
    color = "Category"
  ) +
  scale_colour_manual(values = category_colors) +  # apply your colors here
  theme_minimal() +
  theme(
    plot.margin = margin(10, 30, 10, 10),
    axis.text.y = element_text(size = 10, lineheight = 0.65),
    plot.title = element_text(hjust = 0.5)
  ) +
  my_theme +
  theme(legend.position = "none")
WEMWBS_final_plot_final
ggsave("./Result/Figures/Figuer3_barplot_warwick.png", WEMWBS_final_plot_final, width = 15 * 71/89, height = 25* 71/89)

# MBISS ####

results_MBISS_long_mean_long <- 
  results_MBISS %>%
  mutate(name = paste0(First_author, " et al, ", Year_of_publication, "\n", Type_of_population)) %>%
  mutate(name = make.unique(name)) %>%
  mutate(name = gsub("_", " ", name)) %>%
  mutate(color = Category) %>%
  select(name, AE_mean, EE_mean, CY_mean) %>%
  pivot_longer(
    cols = c(AE_mean, EE_mean, CY_mean),
    names_to = "Subscale",
    values_to = "Mean"
  ) %>%
  mutate(Subscale = gsub("_mean", "", Subscale))  # Keep only AE, EE, CY


results_MBISS_long_SD_long <- 
  results_MBISS %>%
  mutate(name = paste0(First_author, " et al, ", Year_of_publication, "\n", Type_of_population)) %>%
  mutate(name = make.unique(name)) %>%
  mutate(name = gsub("_", " ", name)) %>%
  mutate(color = Category) %>%
  select(name, AE_SD, EE_SD, CY_SD) %>%
  pivot_longer(
    cols = c(AE_SD, EE_SD, CY_SD),
    names_to = "Subscale",
    values_to = "SD"
  ) %>%
  mutate(Subscale = gsub("_SD", "", Subscale))


results_MBISS_long_adjpval_long <- 
  results_MBISS %>%
  mutate(name = paste0(First_author, " et al, ", Year_of_publication, "\n", Type_of_population)) %>%
  mutate(name = make.unique(name)) %>%
  mutate(name = gsub("_", " ", name)) %>%
  mutate(color = Category) %>%
  select(name, adj_AE_p_value, adj_EE_p_value, adj_CY_p_value) %>%
  pivot_longer(
    cols = c(adj_AE_p_value, adj_EE_p_value, adj_CY_p_value),
    names_to = "Subscale",
    values_to = "adj_p_value"
  ) %>%
  mutate(Subscale = gsub("adj_|_p_value", "", Subscale))


results_MBISS_merged_long <- 
  results_MBISS_long_mean_long %>%
  left_join(results_MBISS_long_SD_long, by = c("name", "Subscale")) %>%
  left_join(results_MBISS_long_adjpval_long, by = c("name", "Subscale")) %>%
  left_join(results_MBISS %>%   mutate(name = paste0(First_author, " et al, ", Year_of_publication, "\n", Type_of_population)) %>%
              mutate(name = make.unique(name)) %>%
              mutate(name = gsub("_", " ", name))) %>%
  mutate(significancy = case_when(
    is.na(adj_p_value)  & Category != "Our_study"     ~ "NA",
    adj_p_value < 0.001 ~ "***",
    adj_p_value < 0.01  ~ "**",
    adj_p_value < 0.05  ~ "*",
    adj_p_value >= 0.05 ~ "NS",
    TRUE              ~ ""
  ),
  significancy = paste0(significancy, " (n=", nb_participants, ")")) %>%
  arrange(-desc(EE_mean)) %>%
  mutate(
    signif_bold = ifelse(adj_p_value < 0.05, TRUE, FALSE),
    # Use markdown-style bold text for labels where applicable
    signif_label = ifelse(signif_bold, paste0("**", significancy, "**"), significancy)
  )

# Step 1: Order subscales
results_MBISS_merged_long$Subscale <- factor(results_MBISS_merged_long$Subscale, levels = c("EE", "CY", "AE"))

# Step 2: Reorder 'name' by descending EE mean
EE_order <- results_MBISS_merged_long %>%
  filter(Subscale == "EE") %>%
  arrange(-desc(Mean)) %>%
  pull(name)

results_MBISS_merged_long$name <- factor(results_MBISS_merged_long$name, levels = EE_order)

# Create a reference line dataframe
ref_lines <- data.frame(
  Subscale = c("EE", "CY", "AE"),
  cutoff = c(20.43, 10.57, 23.72)
)

results_MBISS_merged_long <- results_MBISS_merged_long %>%
  mutate(Subscale = factor(Subscale, levels = c("EE", "CY", "AE"))) %>%
  arrange(Subscale) %>%
  mutate(Category = ifelse(First_author == "H Chae", "Longitudinal_cohort", Category))


MBISS_final_plot_final <-
  ggplot(results_MBISS_merged_long, aes(x = Mean, y = name)) +
  geom_point(size = 4, aes(color = Category)) +
  geom_errorbarh(aes(xmin = Mean - SD, xmax = Mean + SD, color = Category), height = 0.3) +
  geom_text(aes(label = significancy,
                fontface = ifelse(!is.na(adj_p_value) & adj_p_value < 0.05, "bold", "plain")),
            hjust = -0.4, vjust= -0.6,
            size = 4) +
  geom_vline(xintercept = 41.21, linetype = "dashed", color = "darkred", linewidth = 0.7) +
  
  # Add subscale-specific hline
  geom_vline(data = ref_lines, aes(yintercept = -Inf, xintercept = cutoff), 
             inherit.aes = FALSE, linetype = "dashed", color = "darkred", linewidth = 0.7) +
  scale_x_continuous(limits = c(-2.4, 36), breaks = seq(0, 36, by = 4)) +
  labs(
    title = "MBI SS Scores Across Studies",
    x = "MBI SS Score",
    y = "",
    color = "Category"
  ) +
  scale_colour_manual(values = category_colors) +
  theme_minimal() +
  theme(
    plot.margin = margin(10, 30, 10, 10),
    axis.text.y = element_text(size = 10, lineheight = 1),
    plot.title = element_text(hjust = 0.5)
  ) +
  my_theme +
  theme(legend.position = "none") +
  facet_wrap(~Subscale)

print(MBISS_final_plot_final)
ggsave("./Result/Figures/Figure3_barplot_mbiss.png", MBISS_final_plot_final, 
       width = 10, height = 8)
ggsave("./Result/Figures/Figure3_barplot_mbiss.png", MBISS_final_plot_final, 
       width = 15 * 22/89* 3 * 1.7, height = 25  *22/89 * 1.5)
