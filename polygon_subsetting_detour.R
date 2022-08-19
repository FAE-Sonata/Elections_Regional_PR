nwt_name<-Filter(function(x) grepl("^Northwest", x, ignore.case=T),
                 regions$Province)
nu_name<-Filter(function(x) grepl("^Nun", x, ignore.case=T),
                regions$Province)

province_polygons<-cdn_1st@polygons
idx_nwt<-which(cdn_1st$shapeName == nwt_name)[1]
idx_nu<-which(cdn_1st$shapeName == nu_name)[1]
nwt_polygons<-province_polygons[[idx_nwt]]
nwt_polygons@plotOrder
nwt_polygons@Polygons[[358]]