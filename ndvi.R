library(raster)
library(sf)
library(sp)
library(readr)
library(rgdal)
library(mapview)
require(MODISTools)

setwd("C:/Users/zy125/Box Sync/Postdoc/taiyuan")


address_2019 <- read_csv("C:/Users/zy125/Box Sync/Postdoc/taiyuan/address_2019.csv")

address<-st_as_sf(address_2019,coords=c("lng","lat"))

address<-st_set_crs(address, "espg:3857")


mapview(address)



buffer_1000 = st_buffer(address, 1000)



mapview(buffer_1000)


ndvi<-stack("NDVI_2019.tif")


ndvi_1000<-extract(ndvi, buffer_1000, fun="mean", na.rm=TRUE)


plot(ndvi)




##########################################

library(MODIS)

ty <- read_sf(dsn = "taiyuan_boundary_4326.shp")
targetCRS <- CRS("+init=epsg:4326")

files = list.files(pattern = "MOD13Q1.*\\.hdf$", recursive = TRUE)


for(f in files){
            
           mod <- getSds(f,method="gdal")
           ndvi <- raster(mod$SDS4gdal[1])*0.0001
           evi <- raster(mod$SDS4gdal[2])*0.0001
           reliability <- raster(mod$SDS4gdal[12])
           m <- reliability
           m[(reliability < 0 | reliability > 1)] <- NA # continue working with QA 0 (good data), and 1 (marginal data)
           ndvi_m <- mask(ndvi, m, maskvalue=NA, updatevalue=NA) # apply the mask to the NDVI raster
          
           ndvi_ReprojWGS84 <- projectRaster(ndvi_m, method = "ngb", crs = targetCRS)
           ndvi_ReprojWGS84_ty<-crop(ndvi_ReprojWGS84, ty)
           writeRaster(ndvi_ReprojWGS84_ty, paste(substr(f, 1, 16), ".tif"))
           
           }


s <- stack(ndvi,evi,reliability)

extract(s,sp)
plot(ndvi_m,1)
print(ndvi_ReprojWGS84)
mapview(ndvi_ReprojWGS84_ty)






ff <- list.files( pattern = "\\.tif$", full=TRUE)



s <- stack(ff)


means = stackApply(s,indices=23,  fun=mean)

mean <- calc(s, fun = mean, na.rm = T)


sites<-read_sf("stations_ty_32649.shp", encoding = "GB2312")

ex <- extract(mean,sites, na.rm=TRUE, df=TRUE, weights = TRUE)



