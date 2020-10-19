library(tidyverse)

read_delim("https://www.data.gouv.fr/fr/datasets/r/61533034-0f2f-4b16-9a6d-28ffabb33a02",";") %>%
  select(epci2020) %>%
  distinct() %>%
  unlist() %>%
  map(function(x) {
    read_delim("https://www.data.gouv.fr/fr/datasets/r/61533034-0f2f-4b16-9a6d-28ffabb33a02",";") %>%
      left_join(read_delim("https://www.banatic.interieur.gouv.fr/V5/fichiers-en-telechargement/telecharger.php?zone=N&date=01/10/2020&format=A","\t",locale = locale(encoding = "ISO-8859-1")),by = c("epci2020" = "N° SIREN")) %>%
      mutate(dernier_jour = as.Date(substr(semaine_glissante,12,21))) %>%
      select(epci2020,`Nom du groupement`,dernier_jour,clage_65,ti) %>%
      pivot_wider(names_from = clage_65,values_from = ti,names_prefix = "age_") %>%
      mutate(alerte_max_0 = 250,alerte_max_65 = 100) %>%
      filter(epci2020 == x) %>%
      write_csv(paste0("chiffres_",x,".csv"))
  })