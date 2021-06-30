<<<<<<< HEAD
setwd("~/Development/DataScienceAcademy/FCD/BigDataRAzure/ProjetoFinal/Grupo-Bimbo-Inventory-Demand/EDA")
getwd()


## Libraries
library(data.table)
library(stringr) #str_extract_all
=======
# Exploratory Data Analysis

### Setting work directory and loading libraries 

## Setting the working directory
setwd("~/Development/DataScienceAcademy/FCD/BigDataRAzure/ProjetoFinal/Grupo-Bimbo-Inventory-Demand/EDA")
## Libraries
library(data.table)
library(stringr) 
>>>>>>> 10th commit: Adjusts made; adding code 1.0 in html
library(dplyr)
library(Hmisc)
library(treemap)
library(ggplot2)
library(forcats)
library(corrplot)

<<<<<<< HEAD
##### 0 Loading Datasets; EDA and pre-processing in auxiliaries Datasets
## 0.1 Loading 'train' dataset
train_orig <- fread(file.choose(), header=T) #train <- fread("Datasets/train.csv", header=T)
gc()
set.seed(123);
train_sample =  train_orig[sample(nrow(train_orig), size=1000000), ]

## 0.2 Loading 'producto_tabla' dataset 
product <- fread("../Datasets/producto_tabla.csv", header=T)
## EDA and pre-processing
dim(product)
glimpse(product) #str(product)
# verifying the duplicated values in 'Cliente_ID' column
table(duplicated(product$NombreProducto))
# quantity of unique values in each columns
sapply(product, function(x) length(unique(x)))
# verifying records quantity without 'NombreProducto.'
nrow(product[product$NombreProducto == 'NO IDENTIFICADO 0', ])
nrow(train_sample[train_sample$Producto_ID == 0, ]) # none
# verifying null values 
sapply(product, function(x) sum(is.na(x)))
# Merging productNombre in train sample
m <- str_extract_all(product$NombreProducto, '^(\\D*)', simplify = TRUE)
product$Prod_nombre <- as.vector(m)
train_sample$Prod_nombre <- product$Prod_nombre[match(train_sample$Producto_ID, product$Producto_ID)]


## 0.3 Loading 'cliente_tabla' dataset
client <- fread("../Datasets/cliente_tabla.csv", header=T)
## EDA and pre-processing
dim(client)
glimpse(client) #str(client)
# verifying the duplicated values in 'Cliente_ID' column
table(duplicated(client$Cliente_ID))
# removing duplicated rows
client <- client[!duplicated(client$Cliente_ID), ]
# quantity of unique values in each columns
sapply(client, function(x) length(unique(x)))
# verifying records quantity without client name.
nrow(client[client$NombreCliente == 'SIN NOMBRE', ])
# quantity of null values in each columns
sapply(client, function(x) sum(is.na(x)))
# verifying records quantity without: 'SIN NOMBRE'
nrow(client[client$NombreCliente == 'SIN NOMBRE', ])
nrow(train_sample[train_sample$Producto_ID == 0, ]) # none
# Merging NombreCliente in train_sample
train_sample$NombreCliente <- client$NombreCliente[match(train_sample$Cliente_ID, client$Cliente_ID)]

## 0.4 Loading 'town_state' dataset
town <- fread("../Datasets/town_state.csv", header=T)
## EDA and pre-processing
dim(town)
glimpse(town) #str(town)
# Duplicated rows analysis
table(duplicated(town))
# verifying the duplicated values in 'Cliente_ID' column
table(duplicated(town$Town))
table(duplicated(town$State))
# quantity of unique values in each columns
sapply(town, function(x) length(unique(x)))
# verifying null values 
=======

## 1. Collecting the data: 
### Loading Datasets; EDA and pre-processing in auxiliaries Datasets

### 1.1 Loading 'train' dataset
# train_sample <- fread("../Datasets/train_sample.csv", header=T)
train_orig <- fread("../Datasets/train.csv", header=T)

# Sets a seed to allow the same experiment result to be reproducible:
set.seed(123);
# as the dataset is very large, let's get a random sample of it
train_sample =  train_orig[sample(nrow(train_orig), size=1000000), ]

# the complete train dataset will not be necessary
rm('train_orig')

### 1.2 Loading 'producto_tabla' dataset 
product <- fread("../Datasets/producto_tabla.csv", header=T)

## EDA and pre-processing
dim(product) # there are 2.592 products
# Checking the data type of dataset variables.
glimpse(product) # or str(product)
# Checking quantity of unique values in each column
sapply(product, function(x) length(unique(x)))
# knowing more the rows
head(product)
# notice there are a lot of information beyond the product's name. For us, it will be necessary just the product's name, so let's extract it 
m <- str_extract_all(product$NombreProducto, '^(\\D*)', simplify = TRUE)
product$Prod_nombre <- as.vector(m)

# Notice 'NombreProducto', it stands for without product name. 
# So, let's check the duplicated in each columns
table(duplicated(product$Producto_ID)) # it was found nothing
table(duplicated(product$NombreProducto) ) # it was found nothing as well
# Checking null values 
sapply(product, function(x) sum(is.na(x))) # there isn't any null value
# Merging productNombre in train_sample
train_sample$Prod_nombre <- product$Prod_nombre[match(train_sample$Producto_ID, product$Producto_ID)]
nrow(train_sample[train_sample$Prod_nombre == 'NO IDENTIFICADO', ]) # none

### 1.3 Loading 'cliente_tabla' dataset 
client <- fread("../Datasets/cliente_tabla.csv", header=T)

## EDA and pre-processing
dim(client) 
glimpse(client) # or str(client)
head(client)
# there are 935362 clients, but let's check for duplicated,
table(duplicated(client)) # apparently none
table(duplicated(client$Cliente_ID)) # now, observe it was found 4862 clients duplicated
# removing duplicated rows
client <- client[!duplicated(client$Cliente_ID), ]
# Checking quantity of unique values in each column
sapply(client, function(x) length(unique(x)))
# Checking quantity of records without client name.
nrow(client[client$NombreCliente == 'SIN NOMBRE', ]) 
# Checking null values 
sapply(client, function(x) sum(is.na(x)))
# Checking quantity of records without name 
nrow(client[client$NombreCliente == 'SIN NOMBRE', ])
# Merging NombreCliente in train_sample
train_sample$NombreCliente <- client$NombreCliente[match(train_sample$Cliente_ID, client$Cliente_ID)]
  
### 1.4 Loading 'town_state' dataset
town <- fread("../Datasets/town_state.csv", header=T)

## EDA and pre-processing
dim(town)
glimpse(town) # or str(town)
head(town)
# Duplicated rows analysis
table(duplicated(town)) # none
# Checking the duplicated values in 'Cliente_ID' column
table(duplicated(town$Town)) # 530
table(duplicated(town$State)) # 757
# Quantity of unique values in each columns
sapply(town, function(x) length(unique(x)))
# Checking null values 
>>>>>>> 10th commit: Adjusts made; adding code 1.0 in html
sapply(town, function(x) sum(is.na(x)))
# Merging town and sate columns in train_sample
train_sample <- merge(train_sample, town, by=c("Agencia_ID"))

<<<<<<< HEAD
## Removing datasets and objects
rm('train_orig', 'product', 'client', 'town', 'm')
gc()

# ordering dataset
=======
### 1.5 Removing objects and ordering columns
## Removing objects
rm('product', 'client', 'town', 'm')

# ordering columns
>>>>>>> 10th commit: Adjusts made; adding code 1.0 in html
train_sample <- train_sample[, c("Semana", "Cliente_ID", "NombreCliente", "Producto_ID", "Prod_nombre", 
                                 "Agencia_ID", "Town", "State", "Ruta_SAK", "Canal_ID", "Venta_uni_hoy",
                                 "Venta_hoy", "Dev_uni_proxima", "Dev_proxima", "Demanda_uni_equil")]

<<<<<<< HEAD

##### Stage 1: knowing the data as they are; and pre-processing
dim(train_sample) #74.180.464
glimpse(train_sample) #str(train)
# verifying null values 
sapply(train_sample, function(x) sum(is.na(x)))

# There are 32 federal entities in Mexico (31 states and the capital, Mexico City, as a separate entity without being formally a state)
# but in the dataset there are 33 records, let's to transform it
=======
## 2.0 knowing the data as they are; and pre-processing
dim(train_sample) #74.180.464
glimpse(train_sample) # or str(train)
head(train_sample)
# verifying null values 
sapply(train_sample, function(x) sum(is.na(x))) # none
# Duplicated rows analysis
table(duplicated(train_sample))
# quantity of unique values in each columns
sapply(train_sample, function(x) length(unique(x)))
# There are 32 federal entities in Mexico (31 states and the capital, Mexico City, as a separate entity without being formally a state), but in the dataset there are 33 records: Queretaro has two nomenclatures. Let's to transform it
>>>>>>> 10th commit: Adjusts made; adding code 1.0 in html
train_sample$State[which(train_sample$State=="QUERETARO")]<-"Querétaro"
train_sample$State[which(train_sample$State=="Queretaro de Arteaga")]<-"Querétaro"
train_sample$State[which(train_sample$State=="AGUASCALIENTES")]<-"Aguascalientes"
train_sample$State[which(train_sample$State=="BAJA CALIFORNIA NORTE")]<-"Baja California"
train_sample$State[which(train_sample$State=="BAJA CALIFORNIA SUR")]<-"Baja California Sur"
train_sample$State[which(train_sample$State=="CAMPECHE")]<-"Campeche"
train_sample$State[which(train_sample$State=="COAHUILA")]<-"Coahuila"
train_sample$State[which(train_sample$State=="COLIMA")]<-"Colima"
train_sample$State[which(train_sample$State=="CHIAPAS")]<-"Chiapas"
train_sample$State[which(train_sample$State=="CHIHUAHUA")]<-"Chihuahua"
train_sample$State[which(train_sample$State=="ESTADO DE MÉXICO")]<-"Estado de México"
train_sample$State[which(train_sample$State=="DURANGO")]<-"Durango"
train_sample$State[which(train_sample$State=="GUANAJUATO")]<-"Guanajuato"
train_sample$State[which(train_sample$State=="GUERRERO")]<-"Guerrero"
train_sample$State[which(train_sample$State=="HIDALGO")]<-"Hidalgo"
train_sample$State[which(train_sample$State=="JALISCO")]<-"Jalisco"
train_sample$State[which(train_sample$State=="MÉXICO, D.F.")]<-"Ciudade de México D.F."
train_sample$State[which(train_sample$State=="MICHOACÁN")]<-"Michoacán"
train_sample$State[which(train_sample$State=="MORELOS")]<-"Morelos"
train_sample$State[which(train_sample$State=="NAYARIT")]<-"Nayarit"
train_sample$State[which(train_sample$State=="NUEVO LEÓN")]<-"Nuevo León"
train_sample$State[which(train_sample$State=="OAXACA")]<-"Oaxaca"
train_sample$State[which(train_sample$State=="PUEBLA")]<-"Puebla"
train_sample$State[which(train_sample$State=="QUINTANA ROO")]<-"Quintana Roo"
train_sample$State[which(train_sample$State=="SAN LUIS POTOSÍ")]<-"San Luis Potosí"
train_sample$State[which(train_sample$State=="SINALOA")]<-"Sinaloa"
train_sample$State[which(train_sample$State=="SONORA")]<-"Sonora"
train_sample$State[which(train_sample$State=="TABASCO")]<-"Tabasco"
train_sample$State[which(train_sample$State=="TAMAULIPAS")]<-"Tamaulipas"
train_sample$State[which(train_sample$State=="TLAXCALA")]<-"Tlaxcala"
train_sample$State[which(train_sample$State=="VERACRUZ")]<-"Veracruz"
train_sample$State[which(train_sample$State=="YUCATÁN")]<-"Yucatán"
train_sample$State[which(train_sample$State=="ZACATECAS")]<-"Zacatecas"

<<<<<<< HEAD

# changing character columns to factor
#cols <- c("Semana", "Ruta_SAK", "Canal_ID") #1, 9, 10
train_sample$Semana <- as.factor(train_sample$Semana)
train_sample$Ruta_SAK <- as.factor(train_sample$Ruta_SAK)
train_sample$Canal_ID <- as.factor(train_sample$Canal_ID)
train_sample <- train_sample %>% mutate_if(is.character, as.factor)

# Duplicated rows analysis
table(duplicated(train_sample))
# quantity of unique values in each columns
sapply(train_sample, function(x) length(unique(x)))
# verifying null values 
sapply(train_sample, function(x) sum(is.na(x)))
str(train_sample)
summary(train_sample)

# (This is the target that will be predicted)
=======
# giving the correct type for factor columns
train_sample$Semana <- as.factor(train_sample$Semana)
train_sample$Ruta_SAK <- as.factor(train_sample$Ruta_SAK)
train_sample$Canal_ID <- as.factor(train_sample$Canal_ID)
train_sample$Cliente_ID <- as.factor(train_sample$Cliente_ID)
train_sample$Producto_ID <- as.factor(train_sample$Producto_ID)
train_sample <- train_sample %>% mutate_if(is.character, as.factor)
str(train_sample)

# Now, the summary of each colum
summary(train_sample)
# 'Demanda_uni_equil' is the target that will be predicted
>>>>>>> 10th commit: Adjusts made; adding code 1.0 in html
head(train_sample$Demanda_uni_equil, 10) 
describe(train_sample$Demanda_uni_equil)
summary(train_sample$Demanda_uni_equil)

# to better understanding
names(train_sample)[11] <- "WeekSales_unit"       #Venta_uni_hoy
names(train_sample)[12] <- "WeekSales_Pesos"      #Venta_hoy
names(train_sample)[13] <- "ReturnNextWeek_unit"  #Dev_uni_proxima
names(train_sample)[14] <- "ReturnNextWeek_Pesos" #Dev_proxima

<<<<<<< HEAD



##### Stage 2: EDA
#names(train_sample)

# Feature Engeneering
# creating a list with the same dataset rows length 
df_subsets <- list(1:nrow(train_sample))

# creting a aux dataset with only columns in df_subsets
cols <- c(1,3,5,6,7,8,9,10, 11:15)
df_aux <- train_sample[, c(1,3,5,6,7,8,9,10, 11:15)] 
names(df_aux)
### 2.1 Creating dataset for each variable counting its frequency    
=======
## 3.0 Exploratory Data Analysis
# Creating a list with the same dataset rows length
df_subsets <- list(1:nrow(train_sample))

# Creating a aux dataset with only some columns in df_subsets
cols <- c(1,3,5,6,7,8,9,10, 11:15)
df_aux <- train_sample[, c(1,3,5,6,7,8,9,10, 11:15)] 

#### Creating variables transformed and graphs functions 
# Creating dataset for each variable counting its frequency    
>>>>>>> 10th commit: Adjusts made; adding code 1.0 in html
for(i in 1:length(cols)){
  df_subsets[[i]] <- df_aux %>%
    group_by_at(i) %>%
    summarise(Units = sum(WeekSales_unit),
              Pesos = sum(WeekSales_Pesos),
              Return_Units = sum(ReturnNextWeek_unit),
              Return_Pesos = sum(ReturnNextWeek_Pesos),
              Sum_Demanda = sum(Demanda_uni_equil),
              counts = n()) %>%
    mutate(Remaining_Pesos = Pesos - Return_Pesos,
           Return_Rate = round(Return_Units / (Units+Return_Units),4)*100)%>%
    mutate(percentOcur = round(prop.table(counts),4)*100) %>%
    mutate(percentOcur = paste(percentOcur, "%", sep ="")) %>%
    mutate(percentUnit = round(prop.table(Units),4)*100) %>%
    mutate(percentUnit = paste(percentUnit, "%", sep ="")) %>%
    mutate(percentRR = paste(Return_Rate, "%", sep =""))%>%
    mutate(percentRP = round(prop.table(Remaining_Pesos),4)*100) %>%
    mutate(percentRP = paste(percentRP, "%", sep ="")) %>%
    mutate(percentSD = round(prop.table(Sum_Demanda),4)*100) %>%
    mutate(percentSD = paste(percentSD, "%", sep ="")) %>%
    arrange(desc(percentUnit)) 
}
<<<<<<< HEAD
View(df_subsets)
=======
#View(df_subsets)
>>>>>>> 10th commit: Adjusts made; adding code 1.0 in html

# create barplot function
barPlot <- function(col, data) {
  data %>%
    mutate_at(c(var = col), as.factor) %>%
    group_by(var) %>%
    ggplot(aes(x = var, y = counts)) +
    geom_bar(stat = 'identity', alpha = 0.5, fill = '#00A4DEF7') +
    #geom_text(aes(label = counts), vjust = 1, size = 3)+
    ylab('Frequency (Client / Product deliveries)') +
    xlab(col) +
    labs(title = paste('Bar plot for variable:', col)) +
    #scale_y_continuous(labels=function(x)paste(x/1000, 'k')) +
    theme_bw()
}

# create boxPlot function
boxPlot <- function(col, data) {
  data %>%
<<<<<<< HEAD
    # mutate_at(c(var = col), as.factor) %>%
    # group_by_at(col) %>%
    # summarise(counts=n())%>%
=======
>>>>>>> 10th commit: Adjusts made; adding code 1.0 in html
    ggplot(aes(x = counts)) +
    geom_boxplot(fill = '#9D7660', color = '#634432', alpha = 0.7) +
    theme(axis.text.y = element_blank()) +
    xlab(paste(col, 'frequency')) +
    scale_x_continuous(labels=function(x)paste(x/1000, 'k')) +
    labs(title = paste('Boxplot for variable:', col)) +
    theme_bw() 
}

# treemap function
treeMap <- function(col, data, interval, title_, vSize_, vColor_, index_) { 
  data[interval, ] %>%
    mutate_at(c(var = col, index_=index_, ord=vSize_), as.factor) %>%  
    group_by(var)%>%
    arrange(desc(ord))%>%
    mutate(col.index=paste(var, index_, sep ="\n"))%>%
    treemap(
      index="col.index", vSize=vSize_, vColor=vColor_,
      palette=c("White","White", "#499894"), # "YlOrRd"
      type="value", title.legend=vColor_, title= title_)
}

<<<<<<< HEAD

## 3.1 week
df_week <- as.data.frame(df_subsets[1])
barPlot("Semana", df_week) 
treeMap("Semana", df_week, 1:nrow(df_week),
        "Treemap: day percentate", "counts", "counts", "percentOcur") 
# there is an almost uniform distribution


## 3.2 Client
df_client <- as.data.frame(df_subsets[2])
#head(df_client[, c(1,10,11,12)],10)

boxPlot('NombreCliente', df_client)
# there is one client who is an extreme outlier
=======
### 3.1 Analysing the variables
### 3.1.1 Week
## 3.1 week
df_week <- as.data.frame(df_subsets[1])
barPlot("Semana", df_week) # There is almost an uniform distribution.
treeMap("Semana", df_week, 1:nrow(df_week),
        "Treemap: day percentate", "counts", "counts", "percentOcur") 

### 3.1.2 Client
df_client <- as.data.frame(df_subsets[2])
#head(df_client[, c(1,10,11,12)],10)
boxPlot('NombreCliente', df_client) # there is one client who is an extreme outlier.
>>>>>>> 10th commit: Adjusts made; adding code 1.0 in html

# Clients with more ocurrence
df_client %>% arrange(desc(counts))%>%
  select(NombreCliente,percentOcur)%>%
  head(10)
<<<<<<< HEAD
# this extreme outlier is 'NO IDENTIFICADO'
# this client is not one customer, it's a group of clients without identification
# 'NO IDENTIFICADO' is associated with ~17.81% of the records in the dataset
# 'Lupita' is on the 2nd as the more occurrences


# however does more occurrences mean more sold units?
df_client %>% arrange(desc(Units))%>%
  select(NombreCliente,percentUnit,percentRR, percentSD)%>%
  head(10)
# 15,22% of the sold units are for 'NO IDENTIFICADO' clients
# 'PUEBLA REMISSION' who is the 7/10th in occurrences, it appears here as 2nd
# so, more occurrences don't mean more sold units, 
# since in one occurrence can have a lot of sold units, and in another can have only a few
# 'PUEBLA REMISION' has a return rate highest than 'NO IDENTIFICADO'
# Important insight: it's almost proportional the quantity of units sold (percentUnit) to the demand (percentSD)
=======
# this extreme outlier is 'NO IDENTIFICADO'.
# this client is not one customer, it's a group of clients without identification.
# 'NO IDENTIFICADO' is associated with ~17.81% of the records in the dataset.
# 'Lupita' is on the 2nd as the more occurrences.

# however do more occurrences mean more sold units?
df_client %>% arrange(desc(Units))%>%
  select(NombreCliente,percentUnit,percentRR, percentSD)%>%
  head(10)
# 15,22% of the sold units are for 'NO IDENTIFICADO' clients.
# 'PUEBLA REMISSION' who is the 7-10th in occurrences, it appears here as 2nd.
# so, more occurrences don't mean more sold units, since in one occurrence can have a lot of sold units, and in another occurrence can have only a few sold units.
# 'PUEBLA REMISION' has a return rate highest than 'NO IDENTIFICADO'.
# It's almost proportional the quantity of units sold (percentUnit) to the demand (percentSD).
>>>>>>> 10th commit: Adjusts made; adding code 1.0 in html

# Clients with more remained Pesos
df_client %>% arrange(desc(Remaining_Pesos))%>%
  select(NombreCliente,Remaining_Pesos, percentRP)%>%
  head(10)
# 'NO IDENTIFICADO' shows the highest remained Pesos
<<<<<<< HEAD
# Important insight: the more units are sold, the demand increases
# 'PUEBLA REMISION' shows a considered rate compared to others

## 3.3 Produto
=======
#  the more units are sold, the demand increases
# 'PUEBLA REMISION' shows a considered rate compared to others

### 3.1.3 Product
>>>>>>> 10th commit: Adjusts made; adding code 1.0 in html
df_prod <- as.data.frame(df_subsets[3])

boxPlot('Prod_nombre', df_prod)
# There are many outliers in product frequencies
# this group deviates from the sales pattern of other products

treeMap("Prod_nombre", df_prod %>% arrange(desc(counts)), 
        1:25, "Top 25 Clients with more occurrences", 
        "counts", "counts", "percentOcur") 

treeMap("Prod_nombre", df_prod %>% arrange(desc(Units)), 
        1:25, "Top 25 Clients for Sold Units ",
        "Units", "Sum_Demanda", "percentSD") 
# the more units are sold, the demand increases

treeMap("Prod_nombre", df_prod %>% arrange(desc(Remaining_Pesos)), 
        1:25, "Top 10 remained Pesos",
        "Remaining_Pesos", "Sum_Demanda", "percentSD")
# "Pan Blanco" and "Pan Integral" have the highest quantity of remained Pesos
# 'Nito' has relatively more demand than them and than all others, since it has more sold units


<<<<<<< HEAD
## 3.4 Agencia
=======
### 3.1.4 Agency
>>>>>>> 10th commit: Adjusts made; adding code 1.0 in html
df_agencia <- as.data.frame(df_subsets[4])
town <- fread("../Datasets/town_state.csv", header=T)
df_agencia$Town <- town$Town[match(df_agencia$Agencia_ID, town$Agencia_ID)]
df_agencia$State <- town$State[match(df_agencia$Agencia_ID, town$Agencia_ID)]

boxPlot('Agencia_ID', df_agencia)
# there are a few outliers

treeMap("Agencia_ID", df_agencia %>% arrange(desc(counts)), 
        1:20, "Top 20 Agencias with more occurrences", 
        "counts", "counts", "percentOcur") 

# where are these agencies?
df_agencia %>% arrange(desc(counts))%>%
  select(Agencia_ID,percentOcur, State)%>%
  head(10)
# ESTADO DE MÉXICO, JALISCO, AGUASCALIENTES states stand out here

# however does more occurrences mean more sold units?
df_agencia %>% arrange(desc(Units))%>%
  select(Agencia_ID,percentUnit,percentRR, percentSD, State)%>%
  head(10)
# In unit, ESTADO DE MÉXICO appeared less compared to occurrences.
# now it appeared MÉXICO, D.F., it isn't in top 10 occurrences 
# JALISCO and AGUASCALIENTES are also here in a stand out way
# 1114 agency has a lower demand percent compared to the percentUnit

<<<<<<< HEAD
# Clients with more remained Pesos
=======
# Agencia with more remained Pesos
>>>>>>> 10th commit: Adjusts made; adding code 1.0 in html
df_agencia %>% arrange(desc(Remaining_Pesos))%>%
  select(Agencia_ID,Remaining_Pesos, percentRP)%>%
  head(10)
# 1114 agency has the highest remained Pesos
# 1911 agency that is in the top 1 of the others two lists, it's here as the 5th

<<<<<<< HEAD
# 3.4.1 State
=======

#### 3.1.4.1 State
>>>>>>> 10th commit: Adjusts made; adding code 1.0 in html
df_state <- as.data.frame(df_subsets[6])
boxPlot('State', df_state)
# there are 3 outliers state

# States with more ocurrence
train_sample %>%
  group_by(State) %>%
  summarise(counts =n()) %>%
  mutate(percent = round(prop.table(counts),4)*100) %>%
  mutate(percent = paste(percent, "%", sep ="")) %>%
  mutate(name = fct_reorder(State, counts)) %>%
  ggplot( aes(x=name, y=counts)) +
  geom_bar(stat="identity", fill="#f68060", alpha=.6, width=.4) +
  geom_text(aes(label = percent), vjust = 1, size = 2.5)+
  labs(title = 'The most frequent states:') +
  coord_flip() +
  xlab("") +
  theme_bw()
# "ESTADO DE MÉXICO" is the state of highest frequency
# the tree outliers are ESTADO DE MÉXICO, MÉXICO, D.F. and JALISCO

# Top 10 States with more sold units
df_state %>% arrange(desc(Units))%>%
  select(State,percentUnit,percentOcur, percentRR)%>%
  head(10)
# 'NUEVO LEÓN' stands out for climbing 3 positions

# Top 10 States with more remained Pesos
df_state %>% arrange(desc(Remaining_Pesos))%>%
  select(State,Remaining_Pesos, percentRP, percentUnit)%>%
  head(10)


<<<<<<< HEAD
# 3.4.2 Town
=======
#### 3.1.4.2 Town
>>>>>>> 10th commit: Adjusts made; adding code 1.0 in html
df_town <- as.data.frame(df_subsets[5])
boxPlot('Town', df_town)
# there are two outliers towns

# The 10 most frequent cities
df_town %>% arrange(desc(counts))%>%
  select(Town,percentOcur)%>%
  head(10)
# these outliers are '2017 AG. SANTA CLARA' and '2309 NORTE'

# Top 10 cities with more sold units
df_town %>% arrange(desc(Units))%>%
  select(Town,percentUnit,percentOcur, percentRR)%>%
  head(10)

# Top 10 cities with more remained Pesos
df_town %>% arrange(desc(Remaining_Pesos))%>%
  select(Town,Remaining_Pesos, percentRP, percentUnit)%>%
  head(10)


<<<<<<< HEAD
## 3.5 Ruta_SAK
=======
### 3.1.5 Ruta_SAK
>>>>>>> 10th commit: Adjusts made; adding code 1.0 in html
df_Ruta <- as.data.frame(df_subsets[7])
boxPlot("Ruta_SAK", df_Ruta)
# There are a lot of outliers

# Ruta_SAK with more ocurrences
df_Ruta %>% arrange(desc(counts))%>%
  select(Ruta_SAK,percentOcur)%>%
  head(10)
# the top 5 is compound by routes between 1201 and 1205

# Top 10 Ruta_SAK with more sold units
df_Ruta %>% arrange(desc(Units))%>%
  select(Ruta_SAK,percentUnit,percentOcur, percentRR)%>%
  head(10)
# but just 1201 is in top 10 of sold units

# Top 10 Ruta_SAK with more remained Pesos
df_Ruta %>% arrange(desc(Remaining_Pesos))%>%
  select(Ruta_SAK,Remaining_Pesos, percentRP, percentUnit)%>%
  head(10)

<<<<<<< HEAD

## 3.6 Channel
=======
### 3.1.6 Channel
>>>>>>> 10th commit: Adjusts made; adding code 1.0 in html
df_channel <- as.data.frame(df_subsets[8])
boxPlot("Canal_ID", df_channel)
# There are two outliers

# Channels occurrences
treeMap("Canal_ID", df_channel %>% arrange(desc(counts)), 
        1:nrow(df_channel), "Treemap: channels", 
        "counts", "counts", "percentOcur") 
# The two outliers are channels 1 and 4

# Channels with more sold units
df_channel %>% arrange(desc(Units))%>%
<<<<<<< HEAD
  select(Canal_ID,percentUnit,percentOcur, percentRR)%>%
=======
  select(Canal_ID,Units, percentUnit,percentOcur, percentRR)%>%
>>>>>>> 10th commit: Adjusts made; adding code 1.0 in html
  head(10)
# channels 2 and 5 have relatively a lot of sold units compared with occurrences

# Top 10 Channel with more remained Pesos
df_channel %>% arrange(desc(Remaining_Pesos))%>%
  select(Canal_ID,Remaining_Pesos, percentRP, percentUnit)%>%
  head(10)
# the channel 2 has a high remained pesos as well

<<<<<<< HEAD

## 3.7 WeekSales_unit (Venta_uni_hoy)]
=======
# for further correlation analysis
train_sample$unitsChannel <- df_channel$Units[match(train_sample$Canal_ID, df_channel$Canal_ID)]

### 3.1.7 WeekSales_unit (Venta_uni_hoy)
>>>>>>> 10th commit: Adjusts made; adding code 1.0 in html
df_unit <- as.data.frame(df_subsets[9])

# distribution data
summary(train_sample$WeekSales_unit) # occurences summary

# top 10 WeekSales_unit occurences
df_unit %>% arrange(desc(counts))%>%
  select(WeekSales_unit,counts,percentOcur)%>%
  head(10)
# sales with 2 units are the most frequent
# the 1, 2 and 3 units represent 50% of occurrences in dataset
# and the number of the 10 most frequent units varies between 1 and 10.

# top 10 WeekSales_unit sold units
df_unit %>% arrange(desc(Units))%>%
  select(WeekSales_unit,Units, percentUnit, percentOcur)%>%
  head(10)
# It's interesting, unit 1 appears in the 2nd of the occurrences with 18%, but it does not appear among the 10 most sold units
# now it appears the units 40, 20, and 15 in the top 10

# Top 10 WeekSales_unit with more remained Pesos
df_unit %>% arrange(desc(Remaining_Pesos))%>%
  select(WeekSales_unit,Remaining_Pesos, percentRP, percentUnit)%>%
  head(10)
# Unit 2 in all the reports stay at the 1st
# almost all the rows are between 1 and 10, except by unit 12 that appeared now, and unit 9 isn't. 


<<<<<<< HEAD
## 3.8 WeekSales_Pesos (Venta_hoy)
=======
### 3.1.8 WeekSales_Pesos (Venta_hoy)
>>>>>>> 10th commit: Adjusts made; adding code 1.0 in html
df_Pesos <- as.data.frame(df_subsets[10])
# distribution data
summary(train_sample$WeekSales_Pesos) # occurences summary

<<<<<<< HEAD
## 3.9 ReturnNextWeek_unit (#Dev_uni_proxima)
=======

### 3.1.9 ReturnNextWeek_unit (#Dev_uni_proxima)
>>>>>>> 10th commit: Adjusts made; adding code 1.0 in html
df_retNexWeek <- as.data.frame(df_subsets[11])
# distribution data
summary(train_sample$ReturnNextWeek_unit) # occurences summary

<<<<<<< HEAD
## 3.9 ReturnNextWeek_Pesos (Dev_proxima)
=======

### 3.1.10 ReturnNextWeek_Pesos (Dev_proxima)
>>>>>>> 10th commit: Adjusts made; adding code 1.0 in html
df_retNexWeekP <- as.data.frame(df_subsets[12])
# distribution data
summary(train_sample$ReturnNextWeek_Pesos) # occurences summary

<<<<<<< HEAD
## 3.10 Demanda_uni_equil
=======

### 3.1.11 Demanda_uni_equil
>>>>>>> 10th commit: Adjusts made; adding code 1.0 in html
df_demanda <- as.data.frame(df_subsets[13])

# distribution data
summary(train_sample$Demanda_uni_equil) # quantile(train_sample$Demanda_uni_equil)
<<<<<<< HEAD
boxplot(train_sample$Demanda_uni_equil,horizontal=TRUE,axes=TRUE)
# There are a lot of outliers, let's see the boxplot without them
=======
boxplot(train_sample$Demanda_uni_equil,horizontal=TRUE,axes=TRUE) # There are a lot of outliers

# Creating a density graph to visualize the data distribution of the variable Demand_uni_equil.
train_sample %>%
  ggplot(aes(x = Demanda_uni_equil)) + 
  geom_density(fill = '#A6EBC9') + 
  theme_bw() + 
  labs(title = 'Density graph for variable: Demanda uni equil') +
  xlab('Demanda uni equil')
# So there is a distortion in the data

# let's see the boxplot without outliers
>>>>>>> 10th commit: Adjusts made; adding code 1.0 in html
boxplot(train_sample$Demanda_uni_equil,horizontal=TRUE,axes=TRUE,outline=FALSE)

# df_demanda %>% arrange(desc(counts)) %>% 
#   select(Demanda_uni_equil,counts, percentOcur, Units,percentUnit) %>% 
#   head(10)

# so let's investigate more about outliers from 3rd quartile
quantile(train_sample$Demanda_uni_equil, probs = c(0.75, 0.8,0.85,0.9,0.95))
# as we can see, the outliers are increasing a lot, especially after 95%

# let's now investigate from 0.95 percentile
quantile(train_sample$Demanda_uni_equil, probs = c(0.95, 0.96,0.97,0.98,0.99, 0.999))
describe(train_sample$Demanda_uni_equil)
# so 96% of the Demanda_uni_equil is  <= 28

train_sample %>% filter(Demanda_uni_equil > 28) %>% tally
<<<<<<< HEAD
# 38957 rows are bigger than 28

# so, for data modelling, it's interesting to use data just when Demanda_uni_equil <= 28
=======
# 38957 items are bigger than 28

>>>>>>> 10th commit: Adjusts made; adding code 1.0 in html
# df <- df[-which(df$Demanda_uni_equil > 28),] # for to remove data which have Demanda_uni_equil > 28

# let's see the occurrences distribution of these 96%
ggplot(df_demanda, aes(x=Demanda_uni_equil, y = counts))+
  geom_histogram(stat = 'identity', alpha = 0.5, fill = '#00A4DEF7')+
  scale_x_continuous(name="Demandas", lim=c(0, 29), breaks=0:28)+
  scale_y_continuous(name="Frequencia", labels=function(x)paste(x/1000, 'k'))+
  theme_bw()

# let's see the sold units distribution of these 96%
ggplot(df_demanda, aes(x=Demanda_uni_equil, y = Units))+
  geom_histogram(stat = 'identity', alpha = 0.5, fill = '#00A4DEF7')+
  scale_x_continuous(name="Demandas", lim=c(0, 29), breaks=0:28)+
  scale_y_continuous(name="Unidades", labels=function(x)paste(x/1000, 'k'))+
  theme_bw()

<<<<<<< HEAD

##### Stage 4: Correlation
=======
rm(cols, barPlot, boxPlot, df_agencia, treeMap, df_aux, df_channel, df_client, df_demanda, df_Pesos, df_prod, df_retNexWeek, df_retNexWeekP, df_Ruta, df_state, df_subsets, df_town, town, df_unit, df_week, i)

## 3.2 Correlation
>>>>>>> 10th commit: Adjusts made; adding code 1.0 in html
glimpse(train_sample)

# converting columns for numeric intending to correlate variables
sample_num <- train_sample[, c("Semana", "Cliente_ID", "Producto_ID", 
                               "Agencia_ID", "Ruta_SAK", "Canal_ID",
                               "WeekSales_unit", "WeekSales_Pesos", "ReturnNextWeek_unit",
                               "ReturnNextWeek_Pesos", "Demanda_uni_equil")]
sample_num[,1:ncol(sample_num)] <- lapply(sample_num[,1:ncol(sample_num)], as.numeric)
glimpse(sample_num)

## correlations
corrplot(cor(sample_num, method="pearson"), method = 'number',
         number.cex= 7/ncol(sample_num), type="lower")
# The target variable has value 1 of positively correlation associated with WeekSales_unit (Venta_uni_hoy), so this variable will overfit the model
# The target variable is highly positively associated with WeekSales_Pesos (Venta_hoy)
# The target variable is very little positively associated with Canal_ID

# notice that in test dataset there are just id, Semana, Agencia_ID, Canal_ID, Ruta_SAK, Cliente_ID, Producto_ID columns. 
<<<<<<< HEAD
# just Producto_ID and Ruta_SAK are related with target variable, but in a very small correlation, 0.04 and 0.05 respectively
=======
# just Producto_ID and Ruta_SAK are related with target variable, but in a very small correlation, 0.04 and 0.05 respectively


### 3.2.1 Searching new variables for a better correlation
correlation <- sample_num[, c("Cliente_ID", "Producto_ID", "Agencia_ID", "Canal_ID", "Ruta_SAK", "Demanda_uni_equil")]

# 3.2.1.1 Creating correlation variables
demand_mean_PC <- correlation %>%
  group_by(Producto_ID, Cliente_ID) %>%
  summarise_at(vars(Demanda_uni_equil),
               list(Mean_byPC = mean))
correlation <- merge(x=correlation,y=demand_mean_PC,by=c("Producto_ID","Cliente_ID"), all.x = TRUE)

demand_mean_ACR <- correlation %>%
  group_by(Agencia_ID, Canal_ID, Ruta_SAK) %>%
  summarise_at(vars(Demanda_uni_equil),
               list(Mean_byACR = mean))
correlation <- merge(x=correlation,y=demand_mean_ACR,by=c("Agencia_ID", "Canal_ID", "Ruta_SAK"), all.x = TRUE)


demand_mean_prod <- correlation %>%  
  group_by(Producto_ID) %>%                         
  summarise_at(vars(Demanda_uni_equil),              
               list(Mean_byProd = mean))
correlation <- merge(x=correlation,y=demand_mean_prod,by=c("Producto_ID"), all.x = TRUE)

demand_mean_client <- correlation %>%  
  group_by(Cliente_ID) %>%                         
  summarise_at(vars(Demanda_uni_equil),              
               list(Mean_byClient = mean))
correlation <- merge(x=correlation,y=demand_mean_client,by=c("Cliente_ID"), all.x = TRUE)

demand_mean_Ag <- correlation %>%  
  group_by(Agencia_ID) %>%                         
  summarise_at(vars(Demanda_uni_equil),              
               list(Mean_byAg = mean))
correlation <- merge(x=correlation,y=demand_mean_Ag,by=c("Agencia_ID"), all.x = TRUE)

demand_mean_Channel <- correlation %>%  
  group_by(Canal_ID) %>%                         
  summarise_at(vars(Demanda_uni_equil),              
               list(Mean_byChannel = mean))
correlation <- merge(x=correlation,y=demand_mean_Channel,by=c("Canal_ID"), all.x = TRUE)

demand_mean_Ruta <- correlation %>%  
  group_by(Ruta_SAK) %>%                         
  summarise_at(vars(Demanda_uni_equil),              
               list(Mean_byRuta = mean))
correlation <- merge(x=correlation,y=demand_mean_Ruta,by=c("Ruta_SAK"), all.x = TRUE)

correlation <- correlation %>% relocate(Demanda_uni_equil, .after = last_col())

corrplot(cor(correlation, method="pearson"), method = 'number',
         number.cex= 7/ncol(correlation), type="lower")

# We can notice that these new variables showed a considerable correlation, especially 'Mean_byPC'



# Do you remember that 96% of data have Demanda_uni_equil <= 28? So, now we will use just these data for evaluate their correlation.

correlation <- correlation[-which(correlation$Demanda_uni_equil > 28),] # for to remove data which have Demanda_uni_equil > 28
corrplot(cor(correlation, method="pearson"), method = 'number',
         number.cex= 7/ncol(correlation), type="lower")

#As we can observe, data without Demanda_uni_equil > 28 is worst on correlation. So it can be more interesting to use in data modelling the created "mean" variables.

rm(demand_mean_ACR, demand_mean_Ag, demand_mean_Channel, demand_mean_client, demand_mean_PC, demand_mean_prod, demand_mean_Ruta, sample_num, correlation, train_sample)

>>>>>>> 10th commit: Adjusts made; adding code 1.0 in html
