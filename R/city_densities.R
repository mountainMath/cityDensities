get_GHS_for<-function(geo=NULL,type=c("250","1k"),year=c("1975","1990","2000","2015")){
  buffer <- ifelse(type=="1k",500,125)
  raster_path = paste0(getOption("custom_data_path"),"GHS/GHS_POP_GPW4",year,"_GLOBE_R2015A_54009_",type,"_v1_0/GHS_POP_GPW4",year,"_GLOBE_R2015A_54009_",type,"_v1_0.tif")
  if (!file.exists(raster_path)) {
    temp=tempfile()
    download.file(paste0("http://cidportal.jrc.ec.europa.eu/ftp/jrc-opendata/GHSL/GHS_POP_GPW4_GLOBE_R2015A/GHS_POP_GPW4",year,"_GLOBE_R2015A_54009_",type,"/V1-0/GHS_POP_GPW4",year,"_GLOBE_R2015A_54009_",type,"_v1_0.zip"),temp)
    exdir=file.path(getOption("custom_data_path"),"GHS")
    if (!dir.exists(exdir)) dir.create(exdir)
    zip::unzip(temp,exdir = exdir)
    if (!file.exists(raster_path))
      stop("Downloading of raster file failed, probably needs some tweaking of the code.")
  }
  r <- raster::raster(raster_path)
  if (!is.null(geo)) {
    vv <- as(geo %>% sf::st_transform(as.character(projection(r))) %>% sf::st_buffer(buffer),"Spatial")
    rr <- raster::crop(r,extent(vv))
    rr <- raster::mask(rr,vv)
  } else {
    rr=r
  }
  #wgs_poj4 <- "+proj=longlat +datum=WGS84 +no_defs"
  #rr %>% projectRaster(crs=wgs_poj4)
  rr
}

#' @export
get_city_locations <- function(){
  location<-maps::world.cities %>%
    tibble::as_tibble() %>%
    dplyr::mutate(name=dplyr::recode(name,"Xianggangdao"="Hong Kong","Soul"="Seoul","Bombay"="Mumbai")) %>%
    sf::st_as_sf(coords = c("long", "lat"), crs = 4326, agr = "constant")
}


labels_and_colors <-function(breaks) {
  labels <- paste0(breaks[-length(breaks)], " to ",breaks[-1])
  l <- rlang::set_names(viridis::inferno(length(labels)),labels)
  names(l)[1]=paste0("Below ",breaks[2])
  names(l)[length(l)]=paste0("Above ",breaks[length(breaks)-1])
  l
}

legend_plot_for_breaks <- function(title="People/ha",
                                   bks=c(1,2.50,5.00,7.50,10.00,17.50,25.00,50.00, 75.00,100.00,200),
                                   remove_lowest=TRUE,
                                   lowest_color=NULL){
  breaks <- c(-Inf,bks,Inf)
  labels <- labels_and_colors(breaks)
  if (remove_lowest) labels <- labels[-1]
  if (!is.null(lowest_color)) labels[1]=lowest_color
  plot_data <- tibble::tibble(cats=factor(names(labels),levels=names(labels)),count=1)
  legend_plot <- ggplot2::ggplot(plot_data,ggplot2::aes(fill=cats,x=cats)) +
    ggplot2::geom_bar() +
    ggplot2::scale_fill_manual(values = labels) +
    ggplot2::labs(fill=title)
  legend_plot
  #tmp <- ggplot_gtable(ggplot_build(legend_plot))
  #leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  #legend <- tmp$grobs[[leg]]
  #legend
}




grid_arrange_shared_legend <- function(plots, legend_plot,
                                       ncol = 3,
                                       nrow = ceiling(length(plots)/ncol),
                                       position = c("bottom", "right"),caption=NA,legend_rows=2) {

  position <- match.arg(position)
  if (position=="bottom") {
    g <- ggplotGrob(legend_plot + theme(legend.position = position) +
                      guides(fill=guide_legend(nrow=legend_rows)))$grobs
  } else {
    g <- ggplotGrob(legend_plot + theme(legend.position = position))$grobs
  }
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  lheight <- sum(legend$height)
  lwidth <- sum(legend$width)
  gl <- lapply(plots, function(x) x + theme(legend.position = "none"))
  gl <- c(gl, nrow = nrow, ncol = ncol)

  combined <- switch(position,
                     "bottom" = arrangeGrob(do.call(arrangeGrob, gl),
                                            legend,
                                            ncol = 1,
                                            heights = unit.c(unit(1, "npc") - lheight, lheight)),
                     "right" = arrangeGrob(do.call(arrangeGrob, gl),
                                           legend,
                                           ncol = 2,
                                           widths = unit.c(unit(1, "npc") - lwidth, lwidth)))
  combined
}



map_plot_for_city <- function(location,title,radius=25000,smoothing=500,
                              bks=c(1,2.50,5.00,7.50,10.00,17.50,25.00,50.00, 75.00,100.00,200),
                              year="2015",
                              remove_lowest=TRUE,lowest_color=NULL,show_density_rings=FALSE) {
  c <- sf::st_coordinates(location) %>% tibble::as_tibble()
  proj4string <- paste0("+proj=lcc +lat_1=",c$Y-1," +lat_2=",c$Y+1," +lat_0=",c$Y,
                        " +lon_0=",c$X," +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")
  ghs <- get_GHS_for(NULL,type="250",year=year)
  center1 <- location %>%
    sf::st_transform(sf::st_crs(ghs)) %>%
    sf::st_buffer(dist = 3*radius)

  ras1 <- raster::crop(ghs, sf::st_bbox(center1)[c(1,3,2,4)]) %>%
    raster::projectRaster(crs=proj4string)
  center <- location %>%
    sf::st_transform(sf::st_crs(ras1)) %>%
    sf::st_buffer(dist = radius)
  ras <- raster::crop(ras1, sf::st_bbox(center %>% sf::st_buffer(radius*0.2))[c(1,3,2,4)]) * 16/100

  mat <- raster::focalWeight(x = ras, d = 500, type = "Gauss")
  rassmooth <- raster::focal(x = ras, w = mat, fun = sum, pad = TRUE, padValue = 30)
  shift=c(200,-200)


  breaks <- c(-Inf,bks,Inf)
  labels <- labels_and_colors(breaks)
  upper_labels <- rlang::set_names(names(labels),breaks[-1])
  lower_labels <- rlang::set_names(names(labels),breaks[-length(breaks)])
  if (!is.null(lowest_color)) labels[1+remove_lowest]=lowest_color

  contours1 <- tanaka::tanaka_contour(rassmooth, breaks = bks)  %>%
    dplyr::mutate(label=dplyr::coalesce(as.character(upper_labels[as.character(max)]),
                          as.character(lower_labels[as.character(min)]))) %>%
    dplyr::mutate(f=labels[label],c=NA)


  contours2 <- tanaka::tanaka_contour(rassmooth %>% raster::shift(x=shift[1],y=shift[2]), breaks = bks) %>%
    dplyr::mutate(f="#000000aa",
                  id=id-0.5,c="black")
  contours3 <- tanaka::tanaka_contour(rassmooth %>% raster::shift(x=-shift[1]/2,y=-shift[2]/2), breaks = bks) %>%
    dplyr::mutate(f="#ffffffaa",
                  id=id-0.6,c="white")
  contours <- dplyr::bind_rows(
    contours1,
    contours2,
    contours3
  ) %>%
    sf::st_sf() %>%
    dplyr::mutate(id=factor(id,levels=.data$id %>% sort)) %>%
    sf::st_set_crs(proj4string)

  if (remove_lowest) contours <- contours %>% dplyr::filter(max>bks[1])

  mask <- center %>%
    sf::st_buffer(2*radius) %>%
    sf::st_difference(center) %>%
    sf::st_transform(crs=proj4string)

  small_mask <- center %>%
    sf::st_buffer(radius/20) %>%
    sf::st_transform(crs=proj4string)

  bbox <- sf::st_bbox(center %>% st_transform(proj4string))
  bbox2 <- sf::st_bbox(center %>% st_transform(4326))

  tile_cache <- paste0(gsub(" ","_",gsub(",.+$","",paste0(round(c,3),collapse = "_"))), "_",radius,"_density_vector_tiles")
  vector_tiles <- cancensusHelpers::simpleCache(cancensusHelpers::get_vector_tiles(bbox2), tile_cache)

  if (length(vector_tiles$water$features)==0) { # workaround if there is no water nearby
    water=rmapzen::as_sf(vector_tiles$roads) %>%
      sf::st_transform(proj4string) %>%
      dplyr::filter(kind=="xxx")
  } else {
    water=rmapzen::as_sf(vector_tiles$water) %>%
      sf::st_transform(proj4string) %>%
      lwgeom::st_make_valid()
  }

  g<-ggplot2::ggplot(contours %>% sf::st_intersection(small_mask)) +
    ggplot2::geom_sf(data=water  %>% sf::st_intersection(small_mask),fill="lightblue",color=NA) +
    ggplot2::geom_sf(ggplot2::aes(fill=f,color=c,group=id),show.legend = "none") +
    ggplot2::geom_sf(data=mask,fill="white",color=NA) +
    ggplot2::scale_colour_identity() +
    ggplot2::scale_fill_identity() +
    ggplot2::theme_void() +
    ggplot2::labs(title = title) +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))

  if (show_density_rings) {
    density_rings <- lapply(seq(1,floor(radius/5000)),function(r){
      location %>%
        sf::st_transform(crs=sf::st_crs(contours)) %>%
        sf::st_buffer(r*5000) %>%
        dplyr::mutate(size=1) %>% #ifelse(r %% 5==1,1,0.25)) %>%
        dplyr::select(size)
    }) %>%
      bind_rows %>%
      sf::st_sf() %>%
      sf::st_set_crs(proj4string)

    g <- g + ggplot2::geom_sf(data=density_rings, inherit.aes = FALSE,ggplot2::aes(size=size),fill=NA,color="#00000055") +
      ggplot2::scale_size_identity()
  }

  g +     ggplot2::coord_sf(datum=NA,xlim=c(bbox$xmin,bbox$xmax),ylim=c(bbox$ymin,bbox$ymax))
}


# example to track down funny plot outline,
# it appears that the mask does not quite overwrite the underlying plot
# at the edges of the plot area.
example_of_plot_boundary_issue <- function(){
  l <- st_point(c(0,0))
  center <- l %>%
    st_buffer(dist = 1)
  bbox <- st_bbox(center)
  mask <- center %>%
    st_buffer(2) %>%
    st_difference(center)

  #ggplot(center %>% st_buffer(2) %>% st_intersection(center %>% st_buffer(0.1))) +
  ggplot2::ggplot(center %>% sf::st_buffer(2)) +
    ggplot2::geom_sf(fill="lightblue",color=NA,show.legend = "none") +
    ggplot2::geom_sf(data=mask,fill="white",color="white") +
    ggplot2::theme_void() +
    ggplot2::coord_sf(datum=NA,xlim=c(bbox$xmin,bbox$xmax),ylim=c(bbox$ymin,bbox$ymax)) +
    ggplot2::labs(title = "Test") +
    ggplot2::theme(plot.title =  ggplot2::element_text(hjust = 0.5))
}


plot_facet <- function(cities,bks=c(1,2.50,5.00,7.50,10.00,17.50,25.00,50.00, 75.00,100.00,200),
                       radius_km=25,ncol=3,years="2015",remove_lowest=TRUE,lowest_color=NULL) {
  caption <- 'Data : European Commission, Joint Research Centre (JRC); Columbia University, CIESIN (2015): GHS population grid, derived from GPW4.'

  if ("sf" %in% class(cities)) {
    location=cities
    city_names <- cities$name
  } else {
    location<-city_locations %>%
      dplyr::filter(name %in% cities) %>%
      dplyr::group_by(name) %>%
      dplyr::top_n(1,pop)
    city_names <- cities
  }

  d=setdiff(city_names,location$name)
  if (length(d)>0) stop(paste0("Could not find ",paste0(d,collapse = ", "),"."))


  plots <- purrr::map(city_names,function(c){
    print(c)
    l <- location %>% filter(name==c)
    years %>% purrr::map(function(y)map_plot_for_city(location=l,
                                               title=paste0(c,", ",y),
                                               radius=radius_km*1000,
                                               bks=bks,year=y,
                                               remove_lowest = remove_lowest,
                                               lowest_color=lowest_color))
  }) %>%
    unlist(recursive = FALSE)

  legend_plot <- legend_plot_for_breaks(bks,title="People/ha",lowest_color=lowest_color,remove_lowest=remove_lowest)

  g<-grid_arrange_shared_legend(plots,legend_plot,position="bottom",ncol=ncol,legend_rows = 1)

  grid.arrange(g, bottom=textGrob(caption, gp=gpar(fontsize=6)),
               top=textGrob(paste0("Population density, ",radius_km,"km radius"), gp=gpar(fontsize=15,font=8)))
}

#' populatin weighted density
#' @export
pop_weighted_density_for <- function(location,max_radius_km=40,year="2015",type="250"){
  c <- sf::st_coordinates(location) %>% as_tibble()
  ghs <- get_GHS_for(NULL,type=type,year=year)
  radius=max_radius_km*1000
  center1 <- location %>%
    sf::st_transform(sf::st_crs(ghs)) %>%
    sf::st_buffer(dist = 2*radius)
  ras1 <- raster::crop(ghs, sf::st_bbox(center1)[c(1,3,2,4)])
  center <- location %>%
    sf::st_transform(sf::st_crs(ras1)) %>%
    sf::st_buffer(dist = radius)
  ras <- stars::st_as_stars(ras1)[center]
  n=names(ras)
  pop <- ras[[n]] %>% sum(na.rm=TRUE)
  a <- ifelse(type=="250",100/16,100)
  density <- (ras[[n]] * ras[[n]]) %>% sum(na.rm=TRUE)  / pop / a
}

density_profile_for_city <- function(location,max_radius_km=40,year="2015") {
  c <- sf::st_coordinates(location) %>% as_tibble()
  proj4string <- paste0("+proj=lcc +lat_1=",c$Y-1," +lat_2=",c$Y+1," +lat_0=",c$Y,
                        " +lon_0=",c$X," +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")
  ghs <- get_GHS_for(NULL,type="250",year=year)
  radius=max_radius_km*1000
  center1 <- location %>%
    sf::st_transform(sf::st_crs(ghs)) %>%
    sf::st_buffer(dist = 3*radius)
  ras1 <- raster::crop(ghs, sf::st_bbox(center1)[c(1,3,2,4)]) %>%
    raster::projectRaster(crs=proj4string)
  center <- location %>%
    sf::st_transform(sf::st_crs(ras1)) %>%
    sf::st_buffer(dist = radius)
  ras <- raster::crop(ras1, sf::st_bbox(center %>% sf::st_buffer(radius*0.2))[c(1,3,2,4)])
  n=names(ras)
  #rassmooth <- focal(x = ras, w = mat, fun = sum, pad = TRUE, padValue = 30)
  d<-seq(1,max_radius_km) %>% lapply(function(r){
    annulus <- sf::st_difference(location %>% sf::st_transform(proj4string) %>% sf::st_buffer(r*1000),
                             location %>% sf::st_transform(proj4string) %>% sf::st_buffer((r-1)*1000))
    rr <- stars::st_as_stars(ras)[annulus]
    rrr <- rr > 1
    pop <- rr[[n]] %>% sum(na.rm=TRUE)
    area <- (rr>1)[[n]] %>% sum(na.rm=TRUE) * 100/16
    total_area <- (rr>=0)[[n]] %>% sum(na.rm=TRUE) * 100/16
    tibble::tibble(OuterRadius=r,InnerRadius=r-1,Population=pop,PopulatedArea=area,totalArea=total_area)
  }) %>%
    bind_rows %>%
    dplyr::mutate(name=location$name) %>%
    dplyr::arrange(OuterRadius) %>%
    dplyr::mutate(Density=Population/PopulatedArea) %>%
    dplyr::mutate(x=(OuterRadius+InnerRadius)/2) %>%
    dplyr::mutate(AnalyticalArea=pi*(OuterRadius**2-InnerRadius**2)*100)
  d %>% dplyr::filter(!is.na(Density),!is.infinite(Density))
}



density_model_for_profile <- function(d,allow_remove=FALSE){
  model <-   nls(formula=Density ~ exp(a+b*x),start=list(a=0,b=0),data=d)
  if (allow_remove) {
    removed <- NULL
    for (r in d$x) {
      m <- nls(formula=Density ~ exp(a+b*x),start=list(a=0,b=0),data=d %>% dplyr::filter(x != r))
      if (summary(m)$sigma<summary(model)$sigma) {
        model=m
        removed=r
      }
    }
    attr(model,"removed")=removed
  }
  model
}

#' Attribution for data
#' @export
ghs_attribution = 'Data : European Commission, Joint Research Centre (JRC); Columbia University, CIESIN (2015): GHS population grid, derived from GPW4.'

#' Attribution for data
#' @export
ghs_attribution_linebreak = 'Data : European Commission, Joint Research Centre (JRC); Columbia University,\nCIESIN (2015): GHS population grid, derived from GPW4.'

density_plot_for_city <- function(location,max_radius_km=40,year="2015"){
  caption <- ghs_attribution
  d <- density_profile_for_city(location,max_radius_km=max_radius_km,year=year)
  model <- density_model_for_profile(d)
  p<-model$m$getAllPars()
  d <- d %>% dplyr::mutate(f=ifelse(!is.null(attr(model,"removed")) && x==attr(model,"removed"),"grey","steelblue"))
  ggplot2::ggplot(d,ggplot2::aes(x=x,y=Density)) +
    ggplot2::geom_bar(stat="identity",ggplot2::aes(fill=f)) +
    ggplot2::scale_fill_identity() +
    ggplot2::theme_light() +
    ggplot2::geom_line(data=tibble::tibble(Density=stats::predict(model,list(x=d$x)),x=d$x),color="brown") +
    ggplot2::geom_text(data=tibble::tibble(x=mean(d$x)*1.5,Density=max(d$Density)*0.9),
              label=paste0("Density = ",round(exp(p[1]))," * exp(",round(p[2],2)," * distance)"),color="brown") +
    ggplot2::labs(caption=caption,title=paste0(location$name,", ",year),x="Distance from centre",y="Mean density (people/ha)")
}


density_plot_series <- function(location,years=c("1975","1990","2000","2015"),radius_km=30,max_density=NULL){

  caption <- 'Data : European Commission, Joint Research Centre (JRC); Columbia University, CIESIN (2015): GHS population grid, derived from GPW4.'

  d <- lapply(years,function(year) {
    d<-density_profile_for_city(location,max_radius_km=radius_km,year=year) %>%
      dplyr::mutate(Year=year)
    model <- density_model_for_profile(d)
    p<-model$m$getAllPars()
    d <- d %>%
      dplyr::mutate(ModeledDensity=predict(model,list(x=d$x))) %>%
      dplyr::mutate(f=ifelse(!is.null(attr(model,"removed")) && x==attr(model,"removed"),"grey","steelblue")) %>%
      dplyr::mutate(D0=exp(p[1]),rho=p[2])
    d
  }) %>%
    bind_rows

  label_data <- d %>%
    group_by(Year) %>%
    summarise(x=mean(x)*1.4,
              Density=max(Density)*0.9,
              D0=first(D0),
              rho=first(rho)) %>%
    ungroup %>%
    mutate(Density=max(Density))

  ggplot2::ggplot(d,ggplot2::aes(x=x,y=Density)) +
    ggplot2::geom_bar(stat="identity",ggplot2::aes(fill=f)) +
    ggplot2::scale_fill_identity() +
    ggplot2::theme_light() +
    ggplot2::facet_wrap("Year",nrow=1) +
    ggplot2::geom_line(ggplot2::aes(y=ModeledDensity),color="brown") +
    ggplot2::geom_text(data=label_data,
              ggplot2::aes(label=latex2exp::TeX(paste0("$Density = ",round(D0)," e^{",round(rho,2)," \\cdot distance}$"), output = "character")),
              color="brown",size=3, parse=TRUE) +
    ggplot2::labs(caption=caption,title=paste0(location$name," density profile"),x="Distance from centre",y="Population density (people/ha)")
}



#' @export
plot_density_facet <- function(cities,bks=c(4,10,25,50,100,200,500,1000),
                               radius_km=40,years=c("1975","1990","2000","2015"),remove_lowest=TRUE,lowest_color=NULL) {
  caption <- 'Data : European Commission, Joint Research Centre (JRC); Columbia University, CIESIN (2015): GHS population grid, derived from GPW4.'
  ncol=4
  legend_rows=1
  if ("sf" %in% class(cities)) {
    location=cities
    city_names <- cities$name
  } else {
    location<-city_locations %>%
      dplyr::filter(name %in% cities) %>%
      dplyr::group_by(name) %>%
      dplyr::top_n(1,pop)
    city_names <- cities
  }

  d=setdiff(city_names,location$name)
  if (length(d)>0) stop(paste0("Could not find ",paste0(d,collapse = ", "),"."))

  plots <- purrr::map(city_names,function(c){
    print(c)
    l <- location %>% filter(name==c)
    years %>% purrr::map(function(y)
      map_plot_for_city(location=l,
                        title=y,
                        radius=radius_km*1000,
                        bks=bks,year=y,
                        remove_lowest = remove_lowest,
                        lowest_color=lowest_color,
                        show_density_rings=TRUE
      ))
  }) %>%
    unlist(recursive = FALSE)


  density_plots <- density_plot_series(location,years=c("1975","1990","2000","2015"),radius_km=radius_km,max_density=NULL) +
    ggplot2::labs(title=NULL)

  legend_plot <- legend_plot_for_breaks(bks,title="People/ha",lowest_color=lowest_color,remove_lowest=remove_lowest)


  g <- ggplot2::ggplotGrob(legend_plot + ggplot2::theme(legend.position = "bottom") +
                             ggplot2::guides(fill=ggplot2::guide_legend(nrow=legend_rows)))$grobs
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  lheight <- sum(legend$height)
  lwidth <- sum(legend$width)
  gl <- lapply(plots, function(x) x + ggplot2::theme(legend.position = "none"))
  gl <- c(gl, nrow = 1, ncol = 4)
  dl <- c(density_plots,nrow=1,ncol=1)

  g <-gridExtra::arrangeGrob(do.call(gridExtra::arrangeGrob, gl),
                  legend,
                  ncol = 1,
                  heights = grid::unit.c(grid::unit(1, "npc") - lheight, lheight))


  gridExtra::grid.arrange(g,density_plots, nrow=2,#bottom=textGrob(caption, gp=gpar(fontsize=6)),
               top=grid::textGrob(paste0(location$name," population density, ",radius_km,"km radius"),
                                  gp=grid::gpar(fontsize=15,font=8)))
}
