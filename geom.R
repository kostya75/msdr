library(ggplot2)
library(dplyr)
library(grid)
library(scales)

geom_timeline(xmin, xmax,color,size alpha)

geom_stat<-
  data_df2%>%
  filter(between(YEAR, 2000, 2015),COUNTRY %in% c("USA","CHINA","GREECE"))%>%
  select(datetime,YEAR,EQ_PRIMARY,DEATHS,COUNTRY)%>%
  #mutate(DEATHS=convert_na_n(DEATHS,0),c2=ifelse(COUNTRY=="USA",30,60))
  mutate(DEATHS=convert_na_n(DEATHS,0),c2=as.numeric(as.factor(COUNTRY)))



ggplot(geom_stat)+geom_segment(aes(x=min(geom_stat$datetime),xend=max(geom_stat$datetime),y=30,yend=30))+
  geom_point(aes(y=30,x=datetime,size=EQ_PRIMARY,color=DEATHS),alpha=1/5)+scale_size_area(max_size=10)


ggplot(geom_stat)+
  #geom_hline(aes(yintercept=30))+geom_hline(aes(yintercept=60))+
  geom_point(aes(y=c2,x=datetime,size=EQ_PRIMARY,color=DEATHS),alpha=1/5)+scale_size_area(max_size=2)+ylim(c(0.5,max(geom_stat$c2+1)))


# Step 1: Create Stat =========================================================================================
StatTimeline <- ggplot2::ggproto("StatTimeline", Stat,
                                  compute_group = function(data, scales) {
                                    
                                    # 
                                    # df<-data%>%
                                    #   filter(between(YEAR, xmin, xmax),COUNTRY %in% countries)%>%
                                    #   select(datetime,YEAR,EQ_PRIMARY,DEATHS,COUNTRY)%>%
                                    #   mutate(DEATHS=convert_na_n(DEATHS,0),c2=as.numeric(as.factor(COUNTRY)),x=datetime)
                                    # 
                                  
                                    #print(data$xmin)
                                  },
                                  required_aes = c("xmin", "xmax","countries")
                                  
                                  
)
stat_timeline <- function(mapping = NULL, data = NULL, geom = "timeline",
                           position = "identity", show.legend = NA,
                           inherit.aes = TRUE, ...) {
  ggplot2::layer(
    stat = StatTimeline,
    data = data,
    mapping = mapping,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes
  )
}


# Step 2: Create Geom ========================================================================================

draw_panel_timeline<-function(data, panel_scales, coord) {
  
  coords <- coord$transform(data, panel_scales)%>%
    mutate(EQ_PRIMARY = rescale(EQ_PRIMARY, from = panel_scales$y.range),
           c2 = rescale(c2,to=c(0,1), from = panel_scales$y.range))
  
  p_grob<-grid::pointsGrob(
    x=coords$datetime,
    y=coords$c2,
    pch=1
    
  )
 
}

GeomTimeline <- ggplot2::ggproto("GeomTimeline", Geom,
                                  required_aes = c("xmin", "xmax","countries"),
                                  #default_aes = aes(scale_radii=1),
                                  draw_key = draw_key_point,
                                  draw_panel = draw_panel_timeline
)

geom_timeline <- function(mapping = NULL, data = NULL, stat = "timeline",
                           position = "identity", show.legend = NA,
                           na.rm = FALSE, inherit.aes = TRUE, ...) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomTimeline,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}


ggplot(data_df2,aes(xmin=2000,xmax=2015,countries="USA"))+geom_timeline()

