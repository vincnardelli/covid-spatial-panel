library(dplyr)
library(lubridate)
library(sf)
library(spdep)
library(ggplot2)
library(patchwork)
library(plm)
library(splm)
# Download data
data <- read.csv("https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-province/dpc-covid19-ita-province.csv")
data <- data[data$sigla_provincia != "", ]

# Transform data
data$data <- as_date(data$data)
data$week <- week(data$data)
data$data <- as.numeric(data$data) - as.numeric(data$data[1])

# Transform Napoli
data$sigla_provincia <- as.character(data$sigla_provincia)
data$sigla_provincia[is.na(data$sigla_provincia)] <- "NA"

map <- st_read("province.geojson")

data$sigla_provincia <- as.factor(data$sigla_provincia)
levels(data$sigla_provincia) <- map$prov_acr

# Create lagged values
data <- data %>% 
  select(provincia = sigla_provincia, data, totale_casi, long, lat, week) %>% 
  group_by(provincia) %>% 
  mutate(totale_casi_t = totale_casi, 
         totale_casi_lag1 = dplyr::lag(totale_casi_t), 
         totale_casi_lag2 = dplyr::lag(totale_casi_t, 2), 
         totale_casi = max(totale_casi_t), 
         diff_totale_casi1 = totale_casi_t - totale_casi_lag1, 
         diff_totale_casi2 = totale_casi_lag1 - totale_casi_lag2) %>% 
  filter(data > 1) %>% 
  arrange(data, provincia)


write.csv(data, "data.csv", row.names=FALSE)


# Load map

# Create W matrix from poly
#nb <- poly2nb(map, row.names = map@data$prov_acr)
#w<-nb2listw(nb)

# W with centroids
coord<-cbind(data$lat[data$data == 2],data$long[data$data == 2])
knb<-knearneigh(coord,3)
nb<-knn2nb(knb, row.names = data$provincia[data$data == 2])
w<-nb2listw(nb)




# Model for all dataset 

model0<-lm(diff_totale_casi1~totale_casi_lag1, data=data)
summary(model0)


# stima panel random effect non spaziale
model1 <- spml(diff_totale_casi1~totale_casi_lag1, data=data, listw=w, model="random", spatial.error="n", lag=FALSE)
summary(model1)

# random effect spatial lag ML
model2 <- spml(diff_totale_casi1~totale_casi_lag1, data=data, listw=w, model="random", spatial.error="n", lag=TRUE)
summary(model2)

# random effect spatial error Baltagi ML
# model3 <- spml(diff_totale_casi1~totale_casi_lag1, data=data, listw=w, model="random", spatial.error="b", lag=FALSE)
# summary(model3)
# 
# # random effect spatial error Kapoor Keleijan Prucha ML
# model4 <- spml(diff_totale_casi1~totale_casi_lag1, data=data, listw=w, model="random", spatial.error="kkp", lag=FALSE)
# summary(model4)
# 
# # random effect spatial error stima GMM
# model5 <- spgm(diff_totale_casi1~totale_casi_lag1, data=data, listw=w, model="random", spatial.error=TRUE, lag=FALSE)
# summary(model5)

# fixed effect spatial lag
model6 <- spml(diff_totale_casi1~totale_casi_lag1, data=data, listw=w, spatial.error="none", lag=TRUE)
summary(model6)

# fixed effect spatial error
model7 <- spml(diff_totale_casi1~totale_casi_lag1, data=data, listw=w, spatial.error="b", lag=FALSE)
summary(model7)


# calculates fixed effects and plots them on a map
eff6<-effects(model6)
eff6_estimate <- c(eff6$INTTable[, 1], eff6$SETable[,1])
eff6_pvalue <- c(eff6$INTTable[, 4], eff6$SETable[,4])

map$eff6 <- eff6_estimate
map$eff6[eff6_pvalue > 0.05] <- NA




eff7<-effects(model7)
eff7_estimate <- c(eff7$INTTable[, 1], eff7$SETable[,1])
eff7_pvalue <- c(eff7$INTTable[, 4], eff7$SETable[,4])

map$eff7 <- eff7_estimate
map$eff7[eff7_pvalue > 0.05] <- NA

# Maps

a <- ggplot(map) + 
  geom_sf(aes(fill=eff6), lwd=0) + 
  theme_void() +
  scale_fill_gradient(
    "Fixed effect",
    high = "#132B43",
    low = "#56B1F7",
    space = "Lab",
    na.value = "grey80",
    guide = "colourbar",
    aesthetics = "fill") +
  ggtitle("Spatial panel fixed effects lag model")


b <- ggplot(map) + 
  geom_sf(aes(fill=eff7), lwd=0) + 
  theme_void() +
  scale_fill_gradient(
    "Fixed effect",
    high = "#132B43",
    low = "#56B1F7",
    space = "Lab",
    na.value = "grey80",
    guide = "colourbar",
    aesthetics = "fill") +
  ggtitle("Spatial panel fixed effects error model")



a | b

regioni_nord <- c("Trentino-Alto Adige/Südtirol",
                  "Valle d'Aosta/Vallée d'Aoste",
                  "Veneto", 
                  "Piemonte",
                  "Lombardia",
                  "Liguria", 
                  "Friuli-Venezia Giulia", 
                  "Emilia-Romagna")

regioni_centro <- c("Abruzzo",
                    "Marche",
                    "Molise",
                    "Lazio",
                    "Toscana", 
                    "Umbria")

regioni_sud <- c("Basilicata", 
                 "Calabria",
                 "Campania",
                 "Puglia")

regioni_isole <- c("Sardegna", 
                   "Sicilia")

map$area[map$reg_name %in% regioni_nord] <- "Nord"
map$area[map$reg_name %in% regioni_centro] <- "Centro"
map$area[map$reg_name %in% regioni_sud] <- "Sud"
map$area[map$reg_name %in% regioni_isole] <- "Isole"

map$area <- as.factor(map$area)
levels(map$area) <- c("Nord", "Centro", "Sud", "Isole")

c <- ggplot(map) +
  geom_histogram(aes(x=eff6, fill=area)) +
  theme_minimal() +
  scale_x_discrete(name ="Fixed effect") + 
  facet_grid(cols=vars(area)) +
  ggtitle("Spatial panel fixed effects lag model")

c

d <- ggplot(map) +
  geom_histogram(aes(x=eff7, fill=area)) +
  theme_minimal() +
  scale_x_discrete(name ="Fixed effect") + 
  facet_grid(cols=vars(area)) +
  ggtitle("Spatial panel fixed effects error model")


c / d


# alternative


c <- ggplot(map) +
  geom_histogram(aes(x=eff6, fill=area)) +
  theme_minimal() +
  scale_x_discrete(name ="Fixed effect") + 
  ggtitle("Spatial panel fixed effects lag model")



d <- ggplot(map) +
  geom_histogram(aes(x=eff7, fill=area)) +
  theme_minimal() +
  scale_x_discrete(name ="Fixed effect") + 
  ggtitle("Spatial panel fixed effects error model")


c | d



# Weekly models analysis


for(week in unique(data$week)){
  
  cat("\nWeek", week, "\n\n")
  
  data_week <- data[data$week == week, ]
  
  # fixed effect spatial lag
  model6 <- spml(diff_totale_casi1~totale_casi_lag1, data=data_week, listw=w, spatial.error="none", lag=TRUE)
  print(summary(model6))
  
  # fixed effect spatial error
  model7 <- spml(diff_totale_casi1~totale_casi_lag1, data=data_week, listw=w, spatial.error="b", lag=FALSE)
  print(summary(model7))
  
  
  # calculates fixed effects and plots them on a map
  eff6<-effects(model6)
  eff6_estimate <- c(eff6$INTTable[, 1], eff6$SETable[,1])
  eff6_pvalue <- c(eff6$INTTable[, 4], eff6$SETable[,4])
  
  map$eff6 <- eff6_estimate
  map$eff6[eff6_pvalue > 0.05] <- NA
  
  
  
  
  eff7<-effects(model7)
  eff7_estimate <- c(eff7$INTTable[, 1], eff7$SETable[,1])
  eff7_pvalue <- c(eff7$INTTable[, 4], eff7$SETable[,4])
  
  map$eff7 <- eff7_estimate
  map$eff7[eff7_pvalue > 0.05] <- NA
  
  a <- ggplot(map) + 
    geom_sf(aes(fill=eff6), lwd=0) + 
    theme_void() +
    scale_fill_gradient(
      "Fixed effect",
      high = "#132B43",
      low = "#56B1F7",
      space = "Lab",
      na.value = "grey80",
      guide = "colourbar",
      aesthetics = "fill") +
    ggtitle("Spatial panel fixed effects lag model", paste("Week", week))
  
  
  b <- ggplot(map) + 
    geom_sf(aes(fill=eff7), lwd=0) + 
    theme_void() +
    scale_fill_gradient(
      "Fixed effect",
      high = "#132B43",
      low = "#56B1F7",
      space = "Lab",
      na.value = "grey80",
      guide = "colourbar",
      aesthetics = "fill") +
    ggtitle("Spatial panel fixed effects error model", paste("Week", week))
  
  
  a | b
  ggsave(paste0("graph/week", week, ".jpg"))
  
}

