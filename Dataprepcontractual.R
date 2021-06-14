rm(list = ls())

###
library(dplyr)
library(tidymodels)
library(data.table)
library(ggplot2)
library(openxlsx)
library(readxl)


# Load data 
ir2016 <- read_excel("Insurance Renewal/20210425 Insurance renewal per year.xlsx", 
                                                   sheet = "2016")
ir2017 <- read_excel("Insurance Renewal/20210425 Insurance renewal per year.xlsx", 
                     sheet = "2017")
ir2018 <- read_excel("Insurance Renewal/20210425 Insurance renewal per year.xlsx", 
                     sheet = "2018")
ir2019 <- read_excel("Insurance Renewal/20210425 Insurance renewal per year.xlsx", 
                     sheet = "2019")
ir2020 <- read_excel("Insurance Renewal/20210425 Insurance renewal per year.xlsx", 
                     sheet = "2020")
#Age <- read_excel("Insurance Renewal/20210505 Age and address.xlsx", sheet = "age") # this is for repurchase analysis
Address <- read_excel("Insurance Renewal/20210505 Age and address.xlsx", sheet = "address")

InRenew <- rbind(ir2016,ir2017,ir2018,ir2019,ir2020)
rm(ir2016,ir2017,ir2018,ir2019,ir2020)


#Hearing Aid technology
tech <- read_excel("Insurance Renewal/20210531 hearing aid technology.xlsx")



#### Renaming ###
InRenew <- InRenew %>% 
  rename(
    client_id = CLIENTE_ID,
    name = `Nombre (Nombre Comercial)`,
    age=Edad,
    company=Empresa,
    branch=`Sucursal Remisión`,
    date=`Fecha Remisión`,
    discount=Descuento,
    channel1=`Como Supo de Nosotros`,
    channel2=`Como Supo de Nos. detalle`,
    quantity=`Cantidad de Aparatos`,
    model1 =Modelo1,
    model2 =Modelo2,
    typepayment=`Condiciones de Pago (Dado  por Especialista)`,
    price=`Importe Neto (Sin IVA) (Aparatos + Acc+Pol)`,
    date_1st_transaction = `Fecha Venta 1er Aparato Cliente`, 
    diff_transactions=`Dif Fecha Venta menos Fecha 1er Apar Cliente`,
    repurchase=`Recompra=1 (Núm. de Remisiones Recompra)`,
    churn1=`Churn 1st year`,
    churn2=`Churn 2nd year`,
    we_call =`We call/not`
  )

Address <- Address %>% 
  rename(   client_id = CLIENTE_ID,
            suburb1 = Colonia1,
            city1 = Ciudad1,
            state1 = Estado1...4,
            suburb2 = Colonia2,
            city2 = Ciudad2,
            state2 = Estado1...7,
            suburb3 = Colonia3,
            city3 = Ciudad3,
            state3 = Estado3
)

# Merge address and InRenew
InRenew <- merge(InRenew, Address, by.x="client_id",by.y="client_id",all.x=TRUE)
rm(Address)

# Merge model and technology
InRenew <- merge(InRenew, tech, by.x="model1",by.y="Model",all.x=TRUE)
rm(tech)

###Check NAs
sapply(InRenew, function(x) sum(is.na(x)))

### NOT CAPTURED TO NA FOR AGE
InRenew$age <- na_if(InRenew$age,"NOT RECORDED")
InRenew$age <- na_if(InRenew$age,"-")

### NA TO UNKNOWN FOR SUBURB1, CITY1, STATE1
InRenew$suburb1 <- replace_na(InRenew$suburb1,"UNKNOWN")
InRenew$city1 <- replace_na(InRenew$city1,"UNKNOWN")
InRenew$state1 <- replace_na(InRenew$state1,"UNKNOWN")



InRenew$channel1[InRenew$channel1 == "DOOD- FACEBOOK"] <- "OTHER"
InRenew$channel1[InRenew$channel1 == "DOOD- OTROS"] <- "OTHER"
InRenew$channel1[InRenew$channel1 == "OTRAS SUCURSALES AK"] <- "AK"
InRenew$channel1[InRenew$channel1 == "OTRAS SUCURSALES AKUSTIKUM"] <- "AK"
InRenew$channel1[InRenew$channel1 == "-"] <- "UNKNOWN"
InRenew$channel1[InRenew$channel1 == "NO SABE"] <- "UNKNOWN"
InRenew$channel1[InRenew$channel1 == "ANUNCIO ESPECTACULAR"] <- "OTHER"
InRenew$channel1[InRenew$channel1 == "FOLLETOS"] <- "OTHER"
InRenew$channel1[InRenew$channel1 == "SECCIÓN AMARILLA LIBRO"] <- "OTHER"
InRenew$channel1[InRenew$channel1 == "INSTITUCIÓN"] <- "OTHER"
InRenew$channel1[InRenew$channel1 == "Z CANAL-COMO SUPO DE NOSOTROS"] <- "OTHER"
InRenew$channel1[InRenew$channel1 == "EVENTOS PARA ADULTOS MAYORES"] <- "OTHER"
InRenew$channel1[InRenew$channel1 == "FERIA"] <- "OTHER"
InRenew$channel1[InRenew$channel1 == "PUBLICIDAD EN CENTRO COMERCIAL"] <- "OTHER"
InRenew$channel1[InRenew$channel1 == "RADIO"] <- "OTHER"
InRenew$channel1[InRenew$channel1 == "TV"] <- "OTHER"
InRenew$channel1[InRenew$channel1 == "SAME DIAGNOSTIC (ES UNA EMPRESA)"] <- "OTHER"
InRenew$channel1[InRenew$channel1 == "INTERNET X CHAT.COM"] <- "INTERNET"
InRenew$channel1[InRenew$channel1 == "INTERNET x TELEFONO.COM"] <- "INTERNET"
InRenew$channel1[InRenew$channel1 == "PERIÓDICO"] <- "OTHER"
InRenew$channel1[InRenew$channel1 == "FACHADA"] <- "OTHER"
InRenew$channel1[InRenew$channel1 == "CONOCIDOS DE EMPLEADOS"] <- "REFERRAL"
InRenew$channel1[InRenew$channel1 == "RECOMENDACIÓN PACIENTE"] <- "REFERRAL"




InRenew$state1[InRenew$state1 == "AGS."] <- "CENTERNORTH"
InRenew$state1[InRenew$state1 == "GRO."] <- "SOUTH"
InRenew$state1[InRenew$state1 == "B.C."] <- "NORTH"
InRenew$state1[InRenew$state1 == "GTO."] <- "CENTER"
InRenew$state1[InRenew$state1 == "OAX."] <- "SOUTH"
InRenew$state1[InRenew$state1 == "VER."] <- "SOUTH"
InRenew$state1[InRenew$state1 == "HGO."] <- "CENTER"
InRenew$state1[InRenew$state1 == "PUE."] <- "CENTER"
InRenew$state1[InRenew$state1 == "YUC."] <- "SOUTH"
InRenew$state1[InRenew$state1 == "CHIS."] <- "SOUTH"
InRenew$state1[InRenew$state1 == "JAL."] <- "CENTERNORTH"
InRenew$state1[InRenew$state1 == "ZAC."] <- "NORTH"
InRenew$state1[InRenew$state1 == "COAH."] <- "NORTH"
InRenew$state1[InRenew$state1 == "MICH."] <- "CENTERNORTH"
InRenew$state1[InRenew$state1 == "S.L.P."] <- "CENTERNORTH"
InRenew$state1[InRenew$state1 == "DGO."] <- "NORTH"
InRenew$state1[InRenew$state1 == "MOR."] <- "CENTER"
InRenew$state1[InRenew$state1 == "TAB"] <- "SOUTH"
InRenew$state1[InRenew$state1 == "TAMPS."] <- "NORTH"
InRenew$state1[InRenew$state1 == "EDO. MEX."] <- "CENTER"
InRenew$state1[InRenew$state1 == "CDMX"] <- "CENTER"
InRenew$state1[InRenew$state1 == "QRO."] <- "CENTER"
InRenew$state1[InRenew$state1 == "N.L."] <- "NORTH"
InRenew$state1[InRenew$state1 == "NO CAPTURADO"] <- "UNKNOWN"




InRenew$typepayment[InRenew$typepayment == "Efectivo-"] <- "CASH"
InRenew$typepayment[InRenew$typepayment == "Pagarés 50%Enganche y 6 pagos mensuales"] <- "OTHER"
InRenew$typepayment[InRenew$typepayment == "TDC-3m"] <- "OTHER"

## change variable types
InRenew <- InRenew %>% 
  dplyr::select(-client_id,-company,-name,-model1,-model2,-channel2, -date_1st_transaction, -R1Date, -R1Year, -R1Month, -R1Units, -R1TotalPrice, -R2Date, -R2Year, - R2Month, -R2Units, - R2TotalPrice, -churn2, -OSR12, -OSR12avg, -we_call, -ASR01, -ASR12,-suburb2,-city2,-state2,-suburb3,-city3,-state3, -city1, -suburb1) %>% #remove unwanted column 
  mutate(age = as.numeric(age),
         branch = as.factor(branch),
         date = as.Date(date),
         channel1 = as.factor(channel1),
         quantity = as.factor(quantity),
         technology = as.factor(technology),
         typepayment = as.factor(typepayment),
         diff_transactions = as.numeric(diff_transactions),
         repurchase = as.factor(repurchase),
         churn1 = as.factor(churn1),
         state1 = as.factor(state1))



### Impute factor variables with missing values
#this is for me to know which client_id has an imputated age and i can generate a new variable saying that there is one imputation
InRenew$imputed_age <- ifelse(is.na(InRenew$age), 1, 0)
#InRenew$imputed_diff_transactions <- ifelse(is.na(InRenew$diff_transactions), 1, 0)
#Imputed_age <- InRenew %>% filter(is.na(age)) 
#Imputed_diff_transactions <- InRenew %>% filter(is.na(diff_transactions))
InRenew$age[is.na(InRenew$age)] = mean(InRenew$age, na.rm=TRUE)
InRenew$OSR01avg[is.na(InRenew$OSR01avg)] = mean(InRenew$OSR01avg, na.rm=TRUE)
#InRenew$diff_transactions[is.na(InRenew$diff_transactions)] = mean(InRenew$diff_transactions, na.rm=TRUE)




InRenew$imputed_age <- as.factor(InRenew$imputed_age)


###Check NAs
sapply(InRenew, function(x) sum(is.na(x))) # NO NAs
save(InRenew, file='processed_dataset.RData')


 
