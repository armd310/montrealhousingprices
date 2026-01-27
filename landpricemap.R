library(patchwork)
library(scico)
library(sf)
library(tidyverse)

# load map data
map<-st_read("limites-administratives-agglomeration-nad83.geojson") %>%
  st_make_valid()

# load prices from csv and keep only unifamiliale property
prices_raw<-read.csv("montreal-centris-2025.csv")
prices_uni <- prices_raw %>%
  filter(property_type == "unifamiliale") %>%
  select(borough, median_price)

# left join when nom = borough
data<-map %>%
  left_join(prices_uni, by = c("NOM" = "borough"))

# make quantile breaks
bks<-quantile(data$median_price, na.rm=TRUE, probs=seq(0,1,0.1))

# add classes based on land prices
data_cl<-data %>%
  mutate(cl=case_when(
    median_price<bks[2]~"A",
    median_price<bks[3]~"B",
    median_price<bks[4]~"C",
    median_price<bks[5]~"D",
    median_price<bks[6]~"E",
    median_price<bks[7]~"F",
    median_price<bks[8]~"G",
    median_price<bks[9]~"H",
    median_price<bks[10]~"I",
    median_price<=bks[11]~"J",
    TRUE~NA
  ))

# make a colour palette, set NA colours, set border colour
pl<-rev(scico(10, palette = "bam"))
na_cl<-'grey50'
br_cl<-'#437C90'

# make a theme
custom_theme<-theme_void()+
  theme(plot.background = element_rect(fill="#202020",color=NA))

#make the map
mp<-ggplot(data_cl) +
  geom_sf(aes(fill = cl), color='dimgray', linewidth=0.2) +
  scale_fill_manual(values=pl,na.value=na_cl)+
  scale_color_manual(values=pl,na.value=na_cl)+
  guides(fill='none',color="none")+
  custom_theme

# Make legend
bks[1]=0
tib<-tibble(
    end=bks
  ) %>%
  mutate(
    start=lag(bks,1),
    cl=c(0,"A","B","C","D","E","F","G","H","I","J")
  ) %>%
  slice(2:11)

# plot the color gradient
lg<-ggplot()+
  geom_rect(
    data=tib,
    aes(xmin=start, xmax=end, ymin=0, ymax=1, fill=cl)
  )+
  geom_text(
    data=tib%>%slice(c(seq(1,10,2))),
    mapping=aes(x=start,y=1.1,label=start),
    angle=45,hjust=0,vjust=0,
    color='#FFF4E5'
  )+
  annotate(
    geom="rect",
    xmin=0,xmax=max(bks),ymin=-1.25,ymax=-0.25,
    fill=NA,color="dimgrey"
  )+
  geom_jitter(
    data=data_cl%>%st_drop_geometry(),
    mapping=aes(x=median_price,y=-0.75,fill=cl,color=cl),
    pch=21,
    width=0,height=0.5
  )+
  scale_y_continuous(limits=c(-1.5,1.5))+
  scale_fill_manual(values=pl,na.value=na_cl)+
  scale_color_manual(values=pl,na.value=na_cl)+
  guides(fill='none',color='none')+
  custom_theme
lg

# Assemble the two plots
layout<- c(
  area(t=0, l=0, b=10, r=10),
  area(t=11, l=0, b=18, r=10)
)

mp + lg +
  plot_layout(design = layout)+
  plot_annotation(
    title = "The price of housing",
    subtitle = 'The map below shows the median price of unifamily housing in 2025'
  )&
  theme(
    plot.background = element_rect(fill="#202020",color=NA),
    text = element_text('mono',hjust=0.5,color="#FFF4E5"),
    plot.title = element_text(hjust=0.5,face='bold'),
    plot.subtitle = element_text(hjust=0.5)
  )


