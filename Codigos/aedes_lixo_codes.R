#==============================================================#
# Observatorio de Saneamento e Meio Ambiente do Recife (OSAR)  #
# Departamento de Pesquisa                                     #
#==============================================================#
# Relacao Entre Coleta de Lixo e Doencas                       #
# Transmitidas pelo Aedes Aegypti                              #
#--------------------------------------------------------------#
# Recife, 2018                                                 #
#--------------------------------------------------------------#

# carregar pacotes
library(readxl); library(readr); library(stringr); library(dplyr); library(ggplot2); library(viridis)
library(maps); library(mapdata); library(raster); library(ggmap); library(ggrepel); library(stringi)
library(purrr); library(OpenStreetMap); library(sp); library(maps); library(ggmap)
library(maptools); library(rgdal)

# carregar dados
casos_dengue2016 <- read_delim("Dados/casos-dengue2016.csv", ";", escape_double = FALSE, trim_ws = TRUE)
roteiro_coleta <- read_delim("Dados/roteirizacao.csv", ";", escape_double = FALSE, trim_ws = TRUE)
pop_data <- read_csv("~/Documents/git_projects/diagjuv-recife/Demografia/resultados/demo_jovem_2010.csv")

# importar geo bairro
shp_recife <- shapefile("Dados/geodata/Bairros.shp")

# baixar base cartografico lograoduros
data_url <- "http://dados.recife.pe.gov.br/dataset/c1f100f0-f56f-4dd4-9dcc-1aa4da28798a/resource/18f16fda-32e2-4fe9-a5ab-0e7852258400/download/trechoslogradouros.geojson"
data_file <- "trechoslogradouros.geojson"
download.file(data_url, data_file)
data_json <- readOGR("Dados/logradouros_recife/trechoslogradouros.geojson", "OGRGeoJSON")

#==== casos por bairro ====#

# contagem
bairro_infec <- data.frame(table(casos_dengue2016$no_bairro_infeccao))
bairro_resid <- data.frame(table(casos_dengue2016$no_bairro_residencia))

# merge
bairro_aedes <- merge(bairro_infec, bairro_resid, by = "Var1", all = T)

# tratar variaveis
bairro_aedes <- mutate(bairro_aedes, n_infect = Freq.x)
bairro_aedes <- mutate(bairro_aedes, n_resid = Freq.y)
bairro_aedes <- mutate(bairro_aedes, EBAIRRNOME = Var1)

# mergir com shape
data_aedes <- merge(shp_recife, bairro_aedes, by = "EBAIRRNOME")

# mergir com pop data
pop_data$EBAIRRNOME <- pop_data$localidade
pop_data$EBAIRRNOME = toupper(pop_data$EBAIRRNOME)
pop_data$EBAIRRNOME = stri_trans_general(pop_data$EBAIRRNOME , "Latin-ASCII")
data_aedes <- merge(data_aedes, pop_data[,c("pop_total", "EBAIRRNOME")], by = "EBAIRRNOME")

# substituir NA por 0
data_aedes$n_infect[is.na(data_aedes$n_infect)] <- 0 
data_aedes$n_resid[is.na(data_aedes$n_resid)] <- 0 

# correlacionar
cor.test(data_aedes@data$n_infect, data_aedes@data$n_resid)

# proporcional
data_aedes@data <- mutate(data_aedes@data, dengue_Por1000Hab = (n_infect / pop_total) * 1000 )
data_aedes@data$dengue_Por1000Hab[is.na(data_aedes@data$dengue_Por1000Hab)] <- 0

#---- mapa da dengue no Recife ----#

# tranformar shapefile em polygonsdataframe
data_aedes_fortity = fortify(data_aedes, region = "EBAIRRNOME")

# extrair centroides dos poligonos
centroids.df = as.data.frame(coordinates(data_aedes))
names(centroids.df) = c("Longitude", "Latitude")  #more sensible column localidades

# base para plotagem
mapa_aedesInfec_data <- data.frame(centroids.df,
                                   variavel = data_aedes@data$dengue_Por1000Hab, 
                                   nomes_centroides = ifelse(data_aedes$dengue_Por1000Hab > 7, 
                                                             as.character(data_aedes$EBAIRRNOME),''),
                                   localidade = data_aedes@data$EBAIRRNOME
)

mapDengue = ggplot(data = mapa_aedesInfec_data, aes(map_id = localidade)) + 
  geom_map(aes(fill = data_aedes$dengue_Por1000Hab),colour = grey(0.85),  map = data_aedes_fortity) +
  expand_limits(x = data_aedes_fortity$long, y = data_aedes_fortity$lat) +
  scale_fill_viridis(name = "Pessoas Infectadas 1000hab", option = "B", direction = -1) +
  # scale_fill_gradient(name = legendtitle, low="lightgreen", high= "darkblue")+
  geom_label_repel(aes(label = nomes_centroides, x = Longitude, y = Latitude), size = 3.3, color = "black") +
  labs(title = "Casos de Dengue em Recife por 1000 hab (2016)")+
  coord_fixed(1) +
  theme_nothing(legend = T)+
theme(legend.key.size = unit(0.7, "cm"),
      legend.text = element_text(size = 14, hjust = 3, vjust = 3),
      legend.title = element_text(size = 15, face = "plain"),
      title = element_text(size = 15, face = "plain"))
mapDengue
ggsave(filename = "dengueBairroMapa2.png", path = "Resultados", width = 8, height = 10, units = "in")

#==== casos por logradouro ===#

# contagem
rua_dengue <- data.frame(table(casos_dengue2016$nome_logradouro_residencia))
rua_coleta <- data.frame(table(roteiro_coleta$endereco))






