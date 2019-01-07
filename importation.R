library(pacman)
p_load(tidyverse, httr, rvest)

NUTS <- GET('http://simap.ted.europa.eu/web/simap/nuts') %>%
  content %>% html_table %>% first %>% as_tibble %>%
  transmute(
    id_anc    = `Previously used code`,
    id        = `2016 NUTS`,
    nom_anc   = `Previously used name`,
    nom       = `2016 NUTS name`,
    chgt      = Change %>% replace(., .=='', NA) %>% factor,
    # certains NUTS 2 ont été regroupés
    anc       = str_split(`Mapping with previous`, ', ?')
  )

NUTS2 <- NUTS %>% filter(str_length(id_anc)==4)

population <- read_tsv("data/demo_r_d2jan.tsv.gz", na = ':') %>%
  separate(1, into=c('unité', 'genre', 'age_groupe', 'id_anc'), sep=',') %>%
  select(-unité) %>%
  gather(année, population, `2017`:`1990`) %>%
  separate(population, into=c('population', 'comments'), sep=' ') %>%
  # mutate_at(c('année', 'population'), as.integer) %>%
  mutate(
    population = population %>% replace(population==':', NA) %>% as.integer,
    année      = as.integer(année)
  ) %>%
  spread(genre, population) %>%
  rename(population=T, population_femmes=F, population_hommes=M) %>%
  mutate(
    age_groupe2 = str_replace(age_groupe, 'Y', '') %>% as.integer,
    age_groupe2 = ifelse(is.na(age_groupe2), age_groupe, floor(age_groupe2/10)*10)
  ) %>%
  select(-age_groupe, -comments) %>%
  group_by(id_anc, année, age_groupe2) %>%
  summarise_all(sum) %>%
  mutate(
    population_0_19    = sum(population[age_groupe2 %in% c('0', '10')]),
    population_20_59   = sum(population[age_groupe2 %in% c('20', '30', '40', '50')]),
    population_60_plus = sum(population[age_groupe2 %in% c('60', '70', '80', '90')], na.rm = TRUE)
  ) %>%
  filter(age_groupe2 =='TOTAL') %>%
  ungroup %>%
  select(-age_groupe2)

# Total area represents the total area of the region including inland waters; it
# is expressed in km2. Land area represents the total land area of the region,
# excluding the area under inland water; it is expressed in km2.
# https://data.europa.eu/euodp/data/dataset/HZKBS2y8ycdZijX0PMHPA
superficie <- read_tsv("data/demo_r_d3area.tsv.gz", na = ':', guess_max = 3000) %>%
  separate(1, into=c('unité', 'type', 'id_anc'), sep=',') %>%
  filter(type=='L0008') %>% select(-unité, -type) %>% 
  gather(année, superficie, `2015`:`1990`) %>%
  separate(superficie, into=c('superficie', 'comments'), sep=' ') %>%
  mutate(
    superficie = superficie %>% replace(superficie==':', NA) %>% as.numeric(),
    année      = as.integer(année)
  )

NUTS2_year <- full_join(
  superficie,
  population,
  by=c('id_anc', "année"),
  suffix = c("_superficie", "_population")
) %>%
  filter(str_length(id_anc) == 4, !id_anc %in% c("EU27", "EU28", "EFTA", "AL01", "AL02", "AL03", "MKXX")) %>%
  full_join(NUTS2)

save(NUTS2_year, file='data/NUTS2_year.RData')

## certaines données ne correspondent à aucune NUTS
# NUTS2_year %>% filter(is.na(id_nv)) %>% pull(id) %>% unique
# inversement, certains codes n'ont pas de données associées
## (surtout des ZZ, "autres territoires")
# NUTS2_year %>% filter(is.na(année)) %>% pull(id) %>% unique
# NUTS2_year %>% filter(is.na(année)) %>% pull(id) %>% unique %>% str_subset('[^Z]{2}$')

data <- read_csv("x,y
1,4
1,5
1,6
2,4
2,5
2,6
7,4
8,5
7,7
7,8
8,7
8,8
") %>%
  mutate_all(funs(. + rnorm(length(.), sd=0.5))) %>%
  mutate(label=str_to_upper(letters[1:n()]))

save(data, file='data/simulated_data.Rdata')