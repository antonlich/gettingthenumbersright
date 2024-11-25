# loading the packages required
packagename<-c("readxl","sf","ggplot2","mapview","RColorBrewer")
if (length(setdiff(packagename, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packagename, rownames(installed.packages())),repos = "https://cran.uni-muenster.de/", Ncpus = 16)  
}

lapply(packagename, require, character.only = T)
rm(packagename)


# increasing the java heap space
rm(list = ls())
options(java.parameters = "-Xmx24g")


# downloading and reading the results of the elections for the european parliament in 2024
temp = tempfile(fileext = ".xlsx")
dataURL <- "https://wahlergebnisse.brandenburg.de/12/100/20240609/europawahl_land/DL/DL_BB_2_EU2024.xlsx"
download.file(dataURL, destfile=temp, mode='wb')

elec_data <- readxl::read_excel(temp, sheet=3)
head(elec_data)


# selection of the election results on the municipality level
elec_data<-elec_data[elec_data$Gebietsart=="Gemeinde", ]


# read in geodata of the municipalities
# data downloaded from https://geobroker.geobasis-bb.de/ where it can be accessed free of charge and without registration
pathtogeodata<-"D:/brandenburg/daten/pos_1/GRENZE_152811-5683624_gemeinden.shp"
mun_geo<-read_sf(pathtogeodata)


# add the shares of votes for the afd to the municipalities geo data set
elec_data$ars<-elec_data$Nummer
elec_data$afd_share<-elec_data$`AfD Prozent`
mun_geo<-merge(mun_geo,elec_data[,c("ars","afd_share")], by="ars")


# plotting
mun_geo_crs<-st_set_crs(mun_geo,'+init=EPSG:25833')

mapview(mun_geo_crs, zcol="afd_share", col.region=colorRampPalette(brewer.pal(9, "Blues")), layer.name = "Share of AfD votes in percent")


# distances from berlin centroid
# read in geodata of the municipality of berlin
# data downloaded from https://geobroker.geobasis-bb.de/ where it can be accessed free of charge and without registration
pathtobergeodata<-"D:/brandenburg/daten/pos_1/GRENZE_152811-5683624_berlingrenze.shp"
mun_ber_geo<-read_sf(pathtobergeodata)
mun_ber_geo_crs<-st_set_crs(mun_ber_geo[1,],'+init=EPSG:25833')

# calculating the centroids of the municipalities
mun_geo_crs$cent<-st_centroid(mun_geo_crs)["geometry"]

# calculating the distance from the centroids of the municipalitiesto the centroid of berlin
mun_geo_crs$dist_ber<-as.numeric(st_distance(mun_geo_crs$cent,st_centroid(mun_ber_geo_crs)["geometry"])[,1])

# calculating the correlation between shares of the afd votes and the distance to the centroid of berlin for each municipality 
cor(mun_geo_crs$afd_share,mun_geo_crs$dist_ber)

# plotting the shares of the afd votes and the distance to the centroid of berlin for each municipality
ggplot(mun_geo_crs, aes(x=dist_ber, y=afd_share)) + geom_point(color="blue") + geom_smooth(method = "lm", color="red") +
  labs(title="Share of AfD votes by distance to Berlin",
       x="Distance to Berlin in metres\n(centroid to centroid)", y = "Share of AfD votes in percent") + theme_classic() +
  xlim(0,max(mun_geo_crs$dist_ber)+10) + ylim(0,100)

