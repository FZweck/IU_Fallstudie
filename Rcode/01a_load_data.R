rm(list = ls())

# Laden relevanter R packages
library(readxl)
library(dplyr)
library(lubridate)
library(zoo)
library(lares)

# Globale Objekte
source_file <- "~/Master/Kurse/08 Fallstudie/Aufgabenstellung/Zusatzdokumente-20240103/use_case_1/PSP_Jan_Feb_2019.xlsx"

###################################
## Datenaufbereitung ##############
###################################

## Laden der Rohdaten
raw_data <- read_excel(source_file)

# Transportiere Daten in letztes Jahr
raw_data$tmsp <- raw_data$tmsp %m+% years(year(Sys.time()) - unique(year(raw_data$tmsp))-1)

## Ausgabe der Spaltennamen und Datentypen
names(raw_data)
sapply(raw_data, class) 

## Die erste Spalte hat keinen Namen, und scheint eine Laufnummer zu sein:
length(unique(raw_data$...1))
names(raw_data)[1] <- "row_id"

## Spalten mit führenden Zahlen sind nicht schön, deshalb Umbenennung
names(raw_data)[names(raw_data)=="3D_secured"] <- "secured"

## Fehlende Werte
colSums(is.na(raw_data))

## Duplikate
sum(duplicated(raw_data[,-1]))
raw_data <- raw_data[!duplicated(raw_data[,-1]),] # -> müsste mit Department abgeklärt werden

## Datentypen
sapply(raw_data, class)

unique(raw_data$success)
unique(raw_data$secured)

raw_data <- raw_data %>% 
  mutate(success = success==1,
         secured = secured==1,
         country = factor(country),
         PSP = factor(PSP),
         card = factor(card))

## Hinzufügen weiterer möglicherweise relevanter Variablen
raw_data <- raw_data %>% 
  mutate(amount_log = log(amount), # Log des Überweisungsbetrag
         tmsp_wday = factor(wday(tmsp, week_start = 1), labels = weekdays(seq(Sys.Date() - wday(Sys.Date(), week_start = 1)-6, Sys.Date() - wday(Sys.Date(), week_start = 1), by="day"))),
         tmsp_is_weekend = tmsp_wday %in% c("Saturday", "Sunday"),
         tmsp_hour = hour(tmsp),
         tmsp_minute = minute(tmsp)
         )

# Indikator für Feiertage in is_weekend aufnehmen
holidays_mapping <- holidays(countries = levels(raw_data$country), years = unique(year(raw_data$tmsp)))
holidays_mapping <- holidays_mapping %>% 
  filter(!season) %>% 
  mutate(date = holiday, is_holiday = 1) %>% 
  select(date, country, is_holiday) %>% 
  distinct()

raw_data <- raw_data %>% 
  mutate(date = as.Date(raw_data$tmsp)) %>% 
  left_join(holidays_mapping) %>% 
  mutate(is_holiday = ifelse(is.na(is_holiday), 0, is_holiday), 
         tmsp_is_weekend = sign(tmsp_is_weekend + is_holiday)==1) %>% 
  select(-date, -is_holiday)

# Überweisungsversuch
raw_data$attempt <- NA
raw_data <- raw_data[order(raw_data$country, raw_data$amount, raw_data$tmsp),] # falls der Datensatz nicht richtig geordnet ist
raw_data$attempt <- sequence(rle(paste(raw_data$country, raw_data$amount))$lengths)
temp <- raw_data %>% 
  mutate(attempt = attempt + 1) %>% 
  select(country, amount, prev_tmsp = tmsp, attempt)

raw_data <- merge(raw_data, temp, all.x = T)
raw_data$ind_timediff <- as.numeric(difftime(raw_data$tmsp, raw_data$prev_tmsp, unit = "mins")) < 1
raw_data$ind_timediff[is.na(raw_data$ind_timediff)] <- F

raw_data$transaction_id[!raw_data$ind_timediff] <- 1:sum(!raw_data$ind_timediff)
raw_data$transaction_id <- na.locf(raw_data$transaction_id)
raw_data$attempt <- sequence(rle(raw_data$transaction_id)$lengths)

raw_data <- raw_data[order(raw_data$row_id),]

# Plausibilitätsprüfung
test <- raw_data %>% 
  group_by(transaction_id) %>% 
  summarise(attempt = max(attempt), test = max(success), test = sum(success)) %>% 
  left_join(raw_data[, c("transaction_id", "attempt", "success")]) %>% 
  mutate(test1 = as.numeric(success)!=test)

sum(test$test1)
raw_data[raw_data$transaction_id%in%test$transaction_id[test$test1],]

## Finales Datenset 
# Entferne Hilfsspalten
cols <- c("prev_tmsp", "ind_timediff")
raw_data <- raw_data[,!names(raw_data)%in%cols]

# Ordne Spalten
final_data <- raw_data %>% 
  select(row_id, transaction_id, PSP, tmsp, success, amount, amount_log, everything())

# Test & Train split mit Undersampling
set.seed(1348)
sample <- sample(c(TRUE, FALSE), nrow(final_data), replace=TRUE, prob=c(0.8,0.2))

train_data <- final_data[sample,]
test_data <- final_data[!sample,]

df_pos <- train_data[train_data$success,]
df_neg <- train_data[!train_data$success,]
sample <- sample(1:nrow(df_neg), nrow(df_pos), replace = T)
train_data_us <- rbind(df_pos, df_neg[sample,])

# Daten für Hyperparameter-Optimierung
sample <- sample(c(TRUE, FALSE), nrow(train_data), replace=TRUE, prob=c(0.2,0.8))
hpo_data <- train_data[sample,]

sample <- sample(c(TRUE, FALSE), nrow(train_data_us), replace=TRUE, prob=c(0.2,0.8))
hpo_data_us <- train_data_us[sample,]

## Kosten ######
costs <- data.frame(PSP = c("Goldcard", "Moneycard", "Simplecard", "UK_Card"),
                    cost_success = c(10, 5, 1, 3),
                    cost_failure = c(5, 2, 0.5, 1))

# Speichere finalen Datensatz
save(final_data, train_data, train_data_us, test_data, hpo_data, hpo_data_us, costs, file = "Robjects/input_data.Rdata")


