setwd("C:/HY/Projects/Elections")
load("Leaflet.RData") # only contains the file assigned to cdn_1st
libraries_needed<-c("leaflet", "rgdal", "magrittr", "maptools", "ggplot2", "broom", "data.table", "stringr")
lapply(libraries_needed,require,character.only=TRUE)

# cdn_1st<-geojsonio::geojson_read("")
# cdn_1st<-rgdal::readOGR("geoBoundaries-CAN-ADM1-all/geoBoundaries-CAN-ADM1.geojson")
class(cdn_1st)
create_dt_version<-function(ogr_obj)  {
  res<-broom::tidy(ogr_obj, region = "shapeName") %>% as.data.table
  setkey(res, "id")
  res %<>% merge(regions, by.x="id", by.y="Province")
  res
}
# adapted from https://rpubs.com/huanfaChen/ggplotShapefile
# cdn_1st_dt<-broom::tidy(cdn_1st, region = "shapeName") %>% as.data.table
# cdn_1st_dt<-broom::tidy(cdn_1st) %>% as.data.table # meaningless numeric IDs

regions<-fread("CDN_regional_definitions.csv")
regions[,Region:=as.factor(Region)]; setkey(regions, "Province")
nwt_name<-Filter(function(x) grepl("^Northwest", x, ignore.case=T),
                 regions$Province)
nu_name<-Filter(function(x) grepl("^Nun", x, ignore.case=T),
                regions$Province)
cdn_1st_dt<-create_dt_version(cdn_1st)

# provinces_only<-leaflet(cdn_1st) %>%
#   setView(lng = -100, lat = 57.5, zoom = 3) %>%
#   addTiles %>%
#   addPolygons
# plot(cdn_1st)

generate_map<-function(dt)  {
  res<-ggplot() + geom_polygon(data = dt,
                               aes(x=long, y=lat, group=group, fill=Region),
                               colour = "black")
  res
}
present_map <- generate_map(cdn_1st_dt)
present_map


## historical boundaries, i.e. before Nunavut created, Newfoundland entered confederation
# nwt_points<-cdn_1st_dt[id %in% c(nwt_name, nu_name),]
# nwt_points[abs(long - -102) < 0.001,] # part of border is approximately 102* W
# cdn_1st_dt$order %>% unique %>% length == nrow(cdn_1st_dt) # order all unique

cdn_1st_dt[,lat_long := str_c(as.character(lat), as.character(long), sep=",")]
setkey(cdn_1st_dt, "lat_long")
nwt_points<-cdn_1st_dt[id==nwt_name,]; nu_points<-cdn_1st_dt[id==nu_name,]
cdn_1st_dt[,lat_long := NULL]

nwt_minus_nu_border<-nwt_points[!nu_points, on=.(lat_long)] # still has parts of "pieces" from full NWT set
nu_minus_nwt_border<-nu_points[!nwt_points, on=.(lat_long)]
rm(list=ls(pattern="points$"))

pre_nu_points<-rbindlist(list(nwt_minus_nu_border, nu_minus_nwt_border))[
  ,lat_long:=NULL][,`:=`(id=nwt_name, group=str_c(nwt_name, piece, sep="."))]
rm(list=ls(pattern="border$"))
# pre_nu_points<-merge.data.table(nwt_minus_nu_border, nu_minus_nwt_border,
#                                 all=T, by="lat_long")
# nwt_minus_nu_border[nu_minus_nwt_border, on=.(lat_long)]

# any(unique(nwt_points$piece) %in% unique(nu_points$piece)) # doesn't work

rm(cdn_1st_dt)
cdn_1st_dt<-create_dt_version(cdn_1st)
no_nunavut<-cdn_1st_dt[!(id %in% c(nwt_name, nu_name)),] # ; no_nunavut[id==nu_name, id:=nwt_name]
no_nunavut<-rbindlist(list(no_nunavut, pre_nu_points))
setkey(no_nunavut, NULL)
nfld_confederation_map<-generate_map(no_nunavut)
# nfld_confederation_map

self_join_dt<-cdn_1st_dt[id %in% c(nwt_name, nu_name),][
  ,lat_long := str_c(as.character(lat), as.character(long), sep=",")]
setkey(self_join_dt, "lat_long")
self_join_res<-merge(self_join_dt, self_join_dt, by="lat_long")