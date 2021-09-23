#------------------------------------------------------------------------
library(raster)
library(sf)
library(ggplot2)
library(ggspatial)
#------------------------------------------------------------------------
Peru        <- getData('GADM', country='Peru', level=2) %>% st_as_sf()
Peru_d      <- getData('GADM', country='Peru', level=3) %>% st_as_sf()
MDD         <- subset(Peru, NAME_1  == "Madre de Dios")
MDD_l        <- subset(Peru_d, NAME_2  == "Tambopata")
Tambo       <- subset(Peru, NAME_2  == "Tambopata")
Tambo_dis   <- subset(Peru_d, NAME_3  == "Tambopata")
Reser_tam   <- st_read ("SHP/Reserva_tambopata.shp")
Carre       <- st_read ("SHP/Via_interocianica.shp")
Reser_tamop <- st_transform(Reser_tam ,crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))
Carre_tamop <- st_transform(Carre ,crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))
Tambo_dis_l <- cbind(MDD_l, st_coordinates(st_centroid(MDD_l$geometry)))
#------------------------------------------------------------------------
library(grid)
library(png)
img <- readPNG("PNG\\Lago.png", FALSE)
g <- rasterGrob(img, x = unit(0.1, "npc"),y = unit(0.1, "npc"), width = unit(0.18, "npc"))
img1 <- readPNG("PNG\\reserva.png", FALSE)
g1 <- rasterGrob(img1, x = unit(0.35, "npc"),y = unit(0.1, "npc"), width = unit(0.3, "npc"))
img2 <- readPNG("PNG\\tortugas.png", FALSE)
g2 <- rasterGrob(img2, x = unit(0.1, "npc"),y = unit(0.65, "npc"), width = unit(0.18, "npc"))
img3 <- readPNG("PNG\\plantas.png", FALSE)
g3 <- rasterGrob(img3, x = unit(0.1, "npc"),y = unit(0.3, "npc"), width = unit(0.18, "npc"))
img4 <- readPNG("PNG\\Reserva-Nacional-Tambopata.png", FALSE)
g4 <- rasterGrob(img4, x = unit(0.1, "npc"),y = unit(0.45, "npc"), width = unit(0.18, "npc"))
img5 <- readPNG("PNG\\tigre.png", FALSE)
g5 <- rasterGrob(img5, x = unit(0.35, "npc"),y = unit(0.35, "npc"), width = unit(0.3, "npc"))
#------------------------------------------------------------------------
MADRE_DIOS =ggplot()+
  geom_sf(data = MDD , fill="seagreen1", color="seagreen4", size=0.7)+
  geom_sf(data = Tambo, color="palegreen2", fill="palegreen3")+
  geom_sf(data = Reser_tamop, fill="seagreen", color="palegreen4")+
  annotate(geom = "text", x = -69.5, y = -10.5, label = "MADRE DE \nDIOS", family="candara", 
           color = "black", size = 3,fontface = "bold")+
  theme_void()
MDD.grob        <- ggplotGrob(MADRE_DIOS)  

MAP=ggplot()+
  geom_sf(data = Tambo , fill="paleturquoise3", color="paleturquoise4")+
  geom_sf(data = Tambo_dis, fill=NA, color="gray60")+
  geom_sf(data = Reser_tamop, fill="palegreen2", color="palegreen4")+
  geom_sf(data = Carre_tamop, color ="orangered4")+
  geom_sf(data = MDD, fill=NA, color="gray70")+
  theme_bw()+
  geom_label(data =  Tambo_dis_l  , aes(x= X, y=Y, label = NAME_3), size = 2, color="black", fontface = "bold",fontfamily = "serif") +
  annotation_north_arrow(location="tr",which_north="true",style=north_arrow_fancy_orienteering ())+
  ggspatial::annotation_scale(style ="bar" ,location = "br",bar_cols = c("grey60", "white"), text_family = "ArcherPro Book")+
  theme(panel.grid.major = element_line(color = gray(.8),linetype = "dashed", size = 0.5),
        axis.text = element_text(colour = "black"),
        axis.text.y  = element_text(angle = 90),
        panel.background = element_rect(fill = "seashell2", colour = "black"),
        panel.border = element_rect(size = 2))+
  annotation_custom(MDD.grob , xmin = -69.5, xmax = -68.9, ymin =-11.5, ymax=-10.9)+
  annotate(geom = "text", x = -70.3, y = -11.2, label = "Mapa de Ubicacion \nde la Reserva \nTAMBOPATA", 
           family="candara", color = "paleturquoise4", size = 6,fontface = "bold")+
  coord_sf(xlim = c(-72.3 ,-68.5), ylim = c(-13.5, -10.8),expand = FALSE)+
  labs(x = NULL, y = NULL)+
  annotation_custom(g)+
  annotation_custom(g1)+
  annotation_custom(g2)+
  annotation_custom(g3)+
  annotation_custom(g4)+
  geom_segment(aes(x=-70.4, xend=-69.3, y=-12.5, yend=-13),  linetype = "dashed", color = "skyblue4", size = 0.6) +
  geom_segment(aes(x=-70.4, xend=-69.3, y=-13.2, yend=-13),  linetype = "dashed", color = "skyblue4", size = 0.6) +
  geom_segment(aes(x=-71.8, xend=-69.3, y=-11.7, yend=-13),  linetype = "dashed", color = "skyblue4", size = 0.6)+
  annotation_custom(g5)
#------------------------------------------------------------------------
ggsave(plot = MAP,"MAPAS/Tambopata.png",
       units = "cm", width = 29,height = 21, dpi = 900)# guardar grafico

  