# Instalacja pakietów

install.packages(c("quantmod", "tidyverse", "lubridate"))
library(quantmod)
library(tidyverse)
library(lubridate)

# Dane dla stóp ESTR i FFR

getSymbols("DEXUSEU", src = "FRED") # dane dla EUR/USD z FRED
getSymbols("FEDFUNDS", src = "FRED") # dane dla effr z FRED
estr_raw <- read.csv("C:/Users/marci/Downloads/PrezentacjaMGO/ECB Data Portal_20250517164604.csv", header = TRUE) # dane dla estr pobrane z ECB
colnames(estr_raw) <- c("date", "estr")

# Porządkowanie danych

kurs <- data.frame(date = index(DEXUSEU), eur_usd = as.numeric(DEXUSEU$DEXUSEU))
effr <- data.frame(date = index(FEDFUNDS), effr = as.numeric(FEDFUNDS$FEDFUNDS))

estr <- estr_raw %>%
  select(date = 1, estr = 3) %>%
  mutate(
    date = ymd(date),
    estr = as.numeric(estr)
  ) %>%
  filter(!is.na(date) & !is.na(estr)) %>%
  arrange(date)

dane <- reduce(list(kurs, effr, estr), full_join, by = "date") %>%
  drop_na() %>%
  arrange(date)

# Logarytmiczna zmiana kursu i różnica kursów

dane <- dane %>%
  mutate(
    diff_rate = effr - estr,
    log_return = c(NA, diff(log(eur_usd)))
  ) %>%
  drop_na()

# Model regresji liniowej

model <- lm(log_return ~ diff_rate, data = dane)
summary(model)

# Wykres

ggplot(dane, aes(x = diff_rate, y = log_return)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "blue") +
  labs(
    title = "Regresja: EUR/USD a różnica stóp (EFFR - €STR)",
    x = "Różnica stóp procentowych (%)",
    y = "Logarytmiczna zmiana kursu EUR/USD"
  ) +
  theme_minimal()


