packagesPrev<- installed.packages()[,"Package"] # Check and get a list of installed packages in this machine and R version
packagesNeed<-list("magrittr", "dplyr", "plyr", "pbapply", "rstac", "gdalcubes", "sf", "terra", "raster", "stars")
lapply(packagesNeed, function(x) {   if ( ! x %in% packagesPrev ) { install.packages(x, force=T)}    }) # Check and install required packages that are not previously installed

# Load libraries
lapply(packagesNeed, library, character.only = TRUE)  # Load libraries - packages

# Pruebas de coleccion QUEBEC
## Cargar el RSTACQuery
baseurl_QUEBEC<- "http://io.biodiversite-quebec.ca/stac/"
STACQuery_QUEBEC<- rstac::stac(baseurl_QUEBEC)

## Revisar lista de colecciones
STACCollectionList_QUEBEC<- rstac::collections(STACQuery_QUEBEC) %>% rstac::get_request()
STACCollectionList_ids_QUEBEC<- sapply(STACCollectionList_QUEBEC$collections, function(x) x$id)

## Cargar coleccion
STACItemCollection_QUEBEC <- STACQuery_QUEBEC %>% rstac::stac_search(collections = "esacci-lc" ) %>% rstac::get_request()

## Cargar assests
assestCollection_QUEBEC<- unlist(rstac::items_assets(STACItemCollection_QUEBEC))

## Cargar image coleccion
image_collection_QUEBEC <- stac_image_collection(STACItemCollection_QUEBEC$features, asset_names = assestCollection_QUEBEC, skip_image_metadata= T)




# Pruebas de coleccion IAvH
## Cargar el RSTACQuery
baseurl_IAvH<- "http://172.191.168.255:8082/"
STACQuery_IAvH<- rstac::stac(baseurl_IAvH)

## Revisar lista de colecciones
STACCollectionList_IAvH<- rstac::collections(STACQuery_IAvH) %>% rstac::get_request()
STACCollectionList_ids_IAvH<- sapply(STACCollectionList_IAvH$collections, function(x) x$id)

STACCollectionList_ids_IAvH

collection_selected<- STACCollectionList_ids_IAvH[length(STACCollectionList_ids_IAvH)]
id_collection<- which(collection_selected == STACCollectionList_ids_IAvH)
  
metadata_collection<- STACCollectionList_IAvH[["collections"]][[id_collection]]
metadata_colection_data<- data.frame(col= metadata_collection$col, value= metadata_collection$value, classes= metadata_collection$classes)

## Cargar coleccion
STACItemCollection_IAvH <- STACQuery_IAvH %>% rstac::stac_search(collections =  "Colombia_PP_test" ) %>% rstac::get_request()

## Cargar assests
assestCollection_IAvH<- unlist(rstac::items_assets(STACItemCollection_IAvH))

## Cargar image coleccion
image_collection_IAvH <- stac_image_collection(STACItemCollection_IAvH$features, asset_names = assestCollection_IAvH, skip_image_metadata= T)

t0_collection_IAvH <- gdalcubes::extent(image_collection_IAvH)$t0
t1_collection_IAvH <- gdalcubes::extent(image_collection_IAvH)$t1

## Poligono de prueba
input<- list(wkt_area= "input/wkt_polygon_test.txt",
             epsg= 3395,resolution=1000
             )

ext_WKT_area<- tools::file_ext(input$wkt_area)
dir_wkt<- if(ext_WKT_area %in% "txt"){ readLines(input$wkt_area) }else{ input$wkt_area }
crs_polygon<- terra::crs( paste0("+init=epsg:", input$epsg) ) %>% as.character()
vector_polygon<- terra::vect(dir_wkt, crs=  crs_polygon ) %>% st_as_sf() %>% st_transform(4326)

crs_polygon<- terra::crs( paste0("+init=epsg:", 4326) ) %>% as.character()

input$resolution<-1000

# Ajustar resolucion
resolution_crs<- raster::raster(raster::extent(seq(4)),crs= paste0("+init=epsg:", 3395), res= input$resolution) %>% 
  raster::projectRaster( crs = crs_polygon) %>% raster::res()


box_polygon<-  sf::st_bbox(vector_polygon) %>% sf::st_as_sfc() %>% sf::st_buffer(sqrt(prod(resolution_crs))) %>% sf::st_bbox()
rasterbase<- raster::raster( raster::extent(box_polygon),crs= crs_polygon, res= resolution_crs) %>% terra::rast()
box_rasterbase<- terra::ext(rasterbase) %>% sf::st_bbox()
dim_rasterbase<- dim(rasterbase)
study_area<- terra::rasterize(vector_polygon, rasterbase)


cube_collection<- gdalcubes::cube_view(srs = crs_polygon,  extent = list(t0 = t0_collection_IAvH, t1 = t1_collection_IAvH,
                                                                         left = box_rasterbase[1], right = box_rasterbase[3],
                                                                         top = box_rasterbase[4], bottom = box_rasterbase[2]),
                                       nx = dim_rasterbase[2], ny = dim_rasterbase[1], dt = "P1Y",aggregation = "near", resampling = "first",
                                       keep.asp= T)


cube <- gdalcubes::raster_cube(image_collection_IAvH, cube_collection)

test_IAvH<- stars::st_as_stars(cube)

raster_test<- terra::rast(test_IAvH)

test_freq<- terra::freq(raster_test)

head(test_freq)

plot(raster_test)