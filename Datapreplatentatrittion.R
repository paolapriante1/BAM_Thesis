rm(list = ls())

###
library(dplyr)
library(data.table)
library(ggplot2)
library(openxlsx)
library(readxl)
library("tidymodels")


# Load data 
transactions <- read_excel("Hearing Aid repurchase/20210504 Venta todas las familias al 4 de mayo.xlsb.xlsx", 
                                                                   sheet = "For analysis")
channel <- read_excel("Hearing Aid repurchase/20210504 Venta todas las familias al 4 de mayo.xlsb.xlsx", 
                           sheet = "channel1")
Age <- read_excel("Insurance Renewal/20210505 Age and address.xlsx", sheet = "age") 
Address <- read_excel("Hearing Aid repurchase/20210505 Age and address.xlsx", 
                                        sheet = "address.1")
sapply(channel, function(x) sum(is.na(x)))
# rename
transactions <- transactions %>% 
  rename(client_id = (CLIENTE_ID),
         company = (Empresa),
         product_category = (Producto),
         quantity = (Unidades),
         date = (Fecha),
         price = (Precio_total),
         seller = (Vendedor))
Address <- Address %>% 
  rename(client_id = (CLIENTE_ID),
         suburb1 = (Colonia1),
         city1 = (Ciudad1),
         state1 = (Estado1))
Age <- Age %>% 
  select(-Empresa) %>%
  rename(client_id = (CLIENTE_ID),
         age = (Edad))
channel <- channel %>% 
  rename(client_id = (CLIENTE_ID), 
         channel= ('Como Supo Nos'))

#change feature types
transactions <- transactions %>% 
  mutate(client_id = as.integer(client_id),
         company = as.factor(company),
         product_category = as.factor(product_category),
         quantity = as.numeric(quantity),
         date = as.Date(date),
         price = as.numeric(price),
         seller = as.factor(seller))
Address <- Address %>% 
  mutate(client_id = as.integer(client_id),
         suburb1 = as.factor(suburb1),
         city1 = as.factor(city1),
         state1 = as.factor(state1))
Age <- Age %>% 
  mutate(client_id = as.integer(client_id),
         age = as.numeric(age))


sapply(channel, function(x) sum(is.na(x)))
# merge covariates
covariates <- merge(Address, Age, by.x="client_id",by.y="client_id",all.x=TRUE)
covariates <- merge(covariates, channel, by.x="client_id",by.y="client_id",all.x=TRUE)
sapply(covariates, function(x) sum(is.na(x)))
rm(Address,Age,channel)

#No missing values for transactions
### NOT CAPTURED TO NA FOR AGE
covariates$age <- na_if(covariates$age,"NO CAPTURADA")
covariates$age <- na_if(covariates$age,"-")
covariates$channel <- replace_na(covariates$channel,"UNKNOWN")
covariates$channel.referral <- replace_na(covariates$channel.referral,0)
covariates$channel.internet <- replace_na(covariates$channel.internet,0)
covariates$channel.doctor <- replace_na(covariates$channel.doctor,0)
###Check NAs
#summary(Age)


sapply(covariates, function(x) sum(is.na(x)))
#missing state1 (423), age(10362), channel(62)
sapply(transactions, function(x) sum(is.na(x)))

#subset the transactions to have only those ids from which we have information on the covariates (even if some info is missing)
transactions <- transactions %>%
  filter(client_id %in% covariates$client_id)

Imputed_age <- covariates %>% filter(is.na(age)) 
covariates$age[is.na(covariates$age)] = mean(covariates$age, na.rm=TRUE)


save(covariates, file='covariates.RData')
save(transactions, file='transactions.RData')





