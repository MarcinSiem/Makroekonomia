### Instalacja i załadowanie potrzebnych pakietów ###
install.packages(c("tidyverse", "lubridate", "readr"))
library(tidyverse)
library(lubridate)
library(readr)

### Wczytywanie danych z plików CSV ###
estr_raw <- read.csv("C:/Users/marci/Downloads/PrezentacjaMGO/ECB Data Portal_20250517164604.csv", skip = 1)
polonia_raw <- read.csv("C:/Users/marci/Downloads/PrezentacjaMGO/Stawka_POLONIA.csv", skip = 0, stringsAsFactors = FALSE)
eurpln_raw <- read.csv("C:/Users/marci/Downloads/PrezentacjaMGO/eurpln_d.csv")

### Oczyszczanie danych ###

# ESTR: wybór kolumny z datą i stawką
estr <- estr_raw %>%
  select(date = 1, estr = 3) %>%
  mutate(
    date = as.Date(date),
    estr = as.numeric(estr)
  ) %>%
  drop_na()


# Zakładam, że już wczytałeś plik jako polonia_raw
colnames(polonia_raw)[1:2] <- c("date", "polonia")

polonia <- polonia_raw %>%
  select(1:2) %>%
  rename(date = 1, polonia = 2) %>%
  mutate(
    # Oczyszczamy i konwertujemy daty
    date = as.Date(str_trim(iconv(date, to = "UTF-8")), format = "%Y-%m-%d"),
    
    # Zamiana przecinka na kropkę i konwersja na numeric
    polonia = as.numeric(str_replace(polonia, ",", "."))
  ) %>%
  arrange(date) %>%
  fill(polonia, .direction = "down") %>%
  drop_na()

# EURPLN: wybór daty i kursu
eurpln <- eurpln_raw %>%
  select(date = Data, eurpln = Zamkniecie) %>%
  mutate(
    date = as.Date(date),
    eurpln = as.numeric(eurpln)
  ) %>%
  drop_na()

### Łączenie danych ###
merged <- eurpln %>%
  inner_join(estr, by = "date") %>%
  inner_join(polonia, by = "date") %>%
  arrange(date)

### Obliczenia: różnica stóp i zmiana logarytmiczna kursu ###
merged <- merged %>%
  mutate(
    diff_rate = polonia - estr,
    log_return = c(NA, diff(log(eurpln)))
  ) %>%
  drop_na()
range(estr$date)
range(polonia$date)
range(eurpln$date)

head(merged)

### Regresja liniowa ###
model <- lm(log_return ~ diff_rate, data = merged)
summary(model)

### Wykres regresji ###
ggplot(merged, aes(x = diff_rate, y = log_return)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", color = "blue", se = TRUE) +
  labs(
    title = "Regresja parytetu stóp procentowych (POLONIA - ESTR vs. zmiana kursu EUR/PLN)",
    x = "Różnica stóp procentowych (POLONIA - ESTR)",
    y = "Zmiana logarytmiczna kursu EUR/PLN"
  ) +
  theme_minimal()

