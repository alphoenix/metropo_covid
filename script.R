library(tidyverse)

read_delim("https://www.data.gouv.fr/fr/datasets/r/61533034-0f2f-4b16-9a6d-28ffabb33a02",";") %>%
  mutate(epci2020 = ifelse(epci2020 == "245900410",as.character("200093201"),as.character(epci2020))) %>%
  select(epci2020) %>%
  distinct() %>%
  unlist() %>%
  map(function(x) {
    read_delim("https://www.data.gouv.fr/fr/datasets/r/61533034-0f2f-4b16-9a6d-28ffabb33a02",";") %>%
      mutate(epci2020 = ifelse(epci2020 == "245900410",as.character("200093201"),as.character(epci2020))) %>%
      left_join(read_delim("https://www.banatic.interieur.gouv.fr/V5/fichiers-en-telechargement/telecharger.php?zone=N&date=01/10/2020&format=A","\t",locale = locale(encoding = "ISO-8859-1")) %>%
                  mutate(`N° SIREN` = as.character(`N° SIREN`)),
                by = c("epci2020" = "N° SIREN")) %>%
      mutate(dernier_jour = as.Date(substr(semaine_glissante,12,21))) %>%
      select(epci2020,`Nom du groupement`,dernier_jour,clage_65,ti) %>%
      pivot_wider(names_from = clage_65,values_from = ti,names_prefix = "age_") %>%
      mutate(alerte_max_0 = 250,alerte_max_65 = 100) %>%
      filter(epci2020 == x) %>%
      write_csv(paste0("chiffres_",x,".csv"))
  })

read_delim("https://www.data.gouv.fr/fr/datasets/r/61533034-0f2f-4b16-9a6d-28ffabb33a02",";") %>%
  mutate(epci2020 = ifelse(epci2020 == "245900410",as.character("200093201"),as.character(epci2020))) %>%
  left_join(read_delim("https://www.banatic.interieur.gouv.fr/V5/fichiers-en-telechargement/telecharger.php?zone=N&date=01/10/2020&format=A","\t",locale = locale(encoding = "ISO-8859-1")) %>%
              mutate(`N° SIREN` = as.character(`N° SIREN`)),
            by = c("epci2020" = "N° SIREN")) %>%
  mutate(dernier_jour = as.Date(substr(semaine_glissante,12,21))) %>%
  select(epci2020,`Nom du groupement`,dernier_jour,clage_65,ti) %>%
  pivot_wider(names_from = clage_65,values_from = ti,names_prefix = "age_") %>%
  filter(dernier_jour == max(dernier_jour)) %>%
  filter(age_65 > 100 & age_0 > 250) %>% arrange(desc(age_0))

read_delim("https://www.data.gouv.fr/fr/datasets/r/61533034-0f2f-4b16-9a6d-28ffabb33a02",";") %>%
  mutate(epci2020 = ifelse(epci2020 == "245900410",as.character("200093201"),as.character(epci2020))) %>%
  left_join(read_delim("https://www.banatic.interieur.gouv.fr/V5/fichiers-en-telechargement/telecharger.php?zone=N&date=01/10/2020&format=A","\t",locale = locale(encoding = "ISO-8859-1")) %>%
              mutate(`N° SIREN` = as.character(`N° SIREN`)),
            by = c("epci2020" = "N° SIREN")) %>%
  mutate(dernier_jour = as.Date(substr(semaine_glissante,12,21))) %>%
  select(epci2020,`Nom du groupement`,dernier_jour,clage_65,ti) %>%
  pivot_wider(names_from = clage_65,values_from = ti,names_prefix = "age_") %>%
  filter(dernier_jour == max(dernier_jour)) %>%
  arrange(desc(age_0))

read_delim("https://www.data.gouv.fr/fr/datasets/r/57d44bd6-c9fd-424f-9a72-7834454f9e3c",";") %>%
  filter(cl_age90 == "0") %>%
  mutate(taux_incid_glissant = slider::slide_dbl(P,sum,.before=6,.after=0,.complete=TRUE)/pop*100000) %>%
  filter(jour > "2020-09-01") %>%
  select(jour,taux_incid_glissant) %>% 
  write_csv("chiffres_france.csv")
  
