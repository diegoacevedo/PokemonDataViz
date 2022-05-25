library(httr)
library(jsonlite)
library(tidyverse)
library(gridExtra)
library(RcppSimdJson)
library(patchwork)
library(grid)

## First data extraction and transformation from JSON to Dataframe

url1 <- "https://pokemon-go1.p.rapidapi.com/pokemon_max_cp.json"


response1=httr::GET(
  url1,
  config = add_headers(
    'X-RapidAPI-Host' = 'pokemon-go1.p.rapidapi.com',
    'X-RapidAPI-Key' = '28926dac64msh6173833084cd432p12f04bjsna8ec1d4882cd'),
  config(connecttimeout=60)
)

text1=content(response1,"text")

df_pokemon1=as.data.frame(fromJSON(text1))

df_pokemon1_normal=df_pokemon1 %>% filter(form=='Normal')


## For second data extraction, i used python because i wasn't able to manipulate "list" datatype columns.
## However, i'm uploading the resulting CSV after i used python so you won't struggle as i did :)
## i'm also leaving the python code just in case you wish to give a look

df_pokemon3=read_csv("C:/Users/UTP/OneDrive - Universidad Tecnologica del Peru/Documents/dacevedo/scripts python/data.csv")

df_pokemon3_normal=df_pokemon3 %>% 
  filter(form=='Normal')

## i joined bot dataframes

df_pokemon4=inner_join(x=df_pokemon1_normal,y=df_pokemon3_normal,by="pokemon_id")

## i sliced "df_pokemon4" in 3 subsets, for plotting purposes, each slice contains 6 Pokemon classes

## i first went with Normal, Fire, Water, Grass, Flying and Fighting:

df_pokemon4_1 = df_pokemon4 %>% 
  select(c(1,2,3,4,7,8))%>% 
  filter(type1 %in% c('Normal','Fire','Water','Grass','Flying','Fighting'))

## colors for box/jitter plots

paleta1=c('burlywood4','firebrick1','mediumorchid1','green3','gray40','dodgerblue')

## Pokemon's .png files, check the zip i uploaded

kartana=png::readPNG('kartana.png',T)
hooh=png::readPNG('ho-oh.png',T)
kyogre=png::readPNG('kyogre.png',T)
marshadow=png::readPNG('marshadow.png',T)
noivern=png::readPNG('noivern.png',T)
slaking=png::readPNG('slaking.png',T)

## font preset
windowsFonts(A=windowsFont('Segoe UI'))

## first visualization. Notice i manually inputed some values. Feel free to automate that hardcoding :)
plot1=ggplot(df_pokemon4_1,aes(x=type1,y=max_cp))+
  geom_boxplot(aes(color=type1),fill=NA,width=0.3,show.legend = F)+
  geom_jitter(aes(color=type1),alpha=0.5,show.legend = F,position = position_jitter(width = 0.2,seed = 10))+
  ylim(0,5500)+
  scale_color_manual(values = paleta1)+
  theme(
    plot.title=element_text(hjust = 0.5,face = 'bold',size=20),
    plot.subtitle = element_text(hjust = 0.5,size=15),
    plot.caption = element_text(hjust = 1,size = 7),
    text=element_text(family = 'A'),
    panel.background = element_blank(),
    axis.line.y =  element_line(linetype = 'solid'),
    axis.line.x =  element_line(linetype = 'solid'),
    plot.background = element_rect(fill='palegoldenrod',),
    panel.grid = element_blank())+
  labs(
    title = 'Pokemons and their maximum Combat Power (MCP)',
    subtitle = 'Distribution of the MCP by type within the game Pokemon GO®.',
    x='Type',
    y='MCP',
    caption = 'Based on Pokemon Go API, from "https://rapidapi.com/Chewett/api/pokemon-go1/"'
  )+
  annotation_raster(kartana,xmin=3.6,xmax =4.5,ymin=3800,ymax=5000,interpolate = F)+
  annotation_raster(hooh,xmin=1.6,xmax =2.5,ymin=3900,ymax=5100,interpolate = F)+
  annotation_raster(kyogre,xmin=5.6,xmax =6.5,ymin=4150,ymax=5350,interpolate = F)+
  annotation_raster(marshadow,xmin=0.7,xmax =1.4,ymin=3800,ymax=4800,interpolate = F)+
  annotation_raster(slaking,xmin=4.6,xmax =5.5,ymin=4500,ymax=5700,interpolate = F)+
  annotation_raster(noivern,xmin=2.6,xmax =3.5,ymin=2800,ymax=4000,interpolate = F)+
  
  annotate(geom='text',label='Marshadow = 3730',x=0.5,y=3730,hjust=0,color='burlywood4',family='A',size=3.5,fontface='bold')+
  annotate(geom='text',label='Ho-oh = 3863',x=1.75,y=3863,hjust=0,color='firebrick1',family='A',size=3.5,fontface='bold')+
  annotate(geom='text',label='Noivern = 2764',x=2.75,y=2764,hjust=0,color='mediumorchid1',family='A',size=3.5,fontface='bold')+
  annotate(geom='text',label='Kartana = 3677',x=3.75,y=3767,hjust=0,color='green3',family='A',size=3.5,fontface='bold')+
  annotate(geom='text',label='Slaking = 4431',x=4.75,y=4431,hjust=0,color='gray40',family='A',size=3.5,fontface='bold')+
  annotate(geom='text',label='Kyogre = 4115',x=5.75,y=4115,hjust=0,color='dodgerblue',family='A',size=3.5,fontface='bold')

## for second visualization, i went with Posion, Electric, Ground, Rock, Psychic and Ice:

df_pokemon4_2 = df_pokemon4 %>% 
  select(c(1,2,3,4,7,8))%>% 
  filter(type1 %in% c('Poison','Electric','Ground','Rock','Psychic','Ice'))

## colors for box/jitter plots
paleta2=c('gold3','tan4','deepskyblue','purple','magenta','gray28')

## Pokemon's .png files
eternatus=png::readPNG('eternatus.png',T)
glastrier=png::readPNG('glastrier.png',T)
groudon=png::readPNG('groudon.png',T)
mewtwo=png::readPNG('mewtwo.png',T)
nihilego=png::readPNG('nihilego.png',T)
xurkitree=png::readPNG('Xurkitree.png',T)

## font preset
windowsFonts(A=windowsFont('Segoe UI'))

## Second visualization
plot2=ggplot(df_pokemon4_2,aes(x=type1,y=max_cp))+
  geom_boxplot(aes(color=type1),fill=NA,width=0.3,show.legend = F)+
  geom_jitter(aes(color=type1),alpha=0.5,show.legend = F,position = position_jitter(width = 0.2,seed = 10))+
  ylim(0,5500)+
  scale_color_manual(values = paleta2)+
  theme(
    plot.title=element_text(hjust = 0.5,face = 'bold',size=20),
    plot.subtitle = element_text(hjust = 0.5,size=15),
    plot.caption = element_text(hjust = 1,size = 7),
    text=element_text(family = 'A'),
    panel.background = element_blank(),
    axis.line.y =  element_line(linetype = 'solid'),
    axis.line.x =  element_line(linetype = 'solid'),
    plot.background = element_rect(fill='palegoldenrod',),
    panel.grid = element_blank())+
  labs(
    title = 'Pokemons and their maximum Combat Power (MCP)',
    subtitle = 'Distribution of the MCP by type within the game Pokemon GO®.',
    x='Type',
    y='MCP',
    caption = 'Based on Pokemon Go API, from "https://rapidapi.com/Chewett/api/pokemon-go1/"'
  )+
  annotation_raster(xurkitree,xmin=0.7,xmax =1.4,ymin=4000,ymax=5200,interpolate = F)+
  annotation_raster(groudon,xmin=1.6,xmax =2.5,ymin=4150,ymax=5350,interpolate = F)+
  annotation_raster(glastrier,xmin=2.6,xmax =3.5,ymin=3950,ymax=5150,interpolate = F)+
  annotation_raster(eternatus,xmin=3.6,xmax =4.5,ymin=4450,ymax=5650,interpolate = F)+
  annotation_raster(mewtwo,xmin=4.6,xmax =5.5,ymin=4200,ymax=5500,interpolate = F)+
  annotation_raster(nihilego,xmin=5.6,xmax =6.5,ymin=4000,ymax=5200,interpolate = F)+
  
  annotate(geom='text',label='Xurkitree = 3937',x=0.5,y=3937,hjust=0,color='gold3',family='A',size=3.5,fontface='bold')+
  annotate(geom='text',label='Groudon = 4115',x=1.75,y=4115,hjust=0,color='tan4',family='A',size=3.5,fontface='bold')+
  annotate(geom='text',label='Glastrier = 3895',x=2.75,y=3895,hjust=0,color='deepskyblue',family='A',size=3.5,fontface='bold')+
  annotate(geom='text',label='Eternatus = 4429',x=3.75,y=4429,hjust=0,color='purple',family='A',size=3.5,fontface='bold')+
  annotate(geom='text',label='Mewtwo = 4178',x=4.75,y=4178,hjust=0,color='magenta',family='A',size=3.5,fontface='bold')+
  annotate(geom='text',label='Nihilego = 3949',x=5.75,y=3949,hjust=0,color='gray28',family='A',size=3.5,fontface='bold')

## for the last visual, i went with bug, Ghost, Steel, Dragon, Dark and Fairy
df_pokemon4_3 = df_pokemon4 %>% 
  select(c(1,2,3,4,7,8))%>% 
  filter(type1 %in% c('Bug','Ghost','Steel','Dragon','Dark','Fairy'))

## colors for box/jitter plots
paleta3=c('olivedrab3','gray8','darkred','hotpink','plum4','steelblue4')

## Pokemon's .png files
volcarona=png::readPNG('volcarona.png',T)
zarude=png::readPNG('zarude.png',T)
reshiram=png::readPNG('reshiram.png',T)
xerneas=png::readPNG('xerneas.png',T)
spectrier=png::readPNG('spectrier.png',T)
dialga=png::readPNG('dialga.png',T)

## font preset
windowsFonts(A=windowsFont('Segoe UI'))

## Third visualization
plot3=ggplot(df_pokemon4_3,aes(x=type1,y=max_cp))+
  geom_boxplot(aes(color=type1),fill=NA,width=0.3,show.legend = F)+
  geom_jitter(aes(color=type1),alpha=0.5,show.legend = F,position = position_jitter(width = 0.2,seed = 10))+
  ylim(0,5500)+
  scale_color_manual(values = paleta3)+
  theme(
    plot.title=element_text(hjust = 0.5,face = 'bold',size=20),
    plot.subtitle = element_text(hjust = 0.5,size=15),
    plot.caption = element_text(hjust = 1,size = 7),
    text=element_text(family = 'A'),
    panel.background = element_blank(),
    axis.line.y =  element_line(linetype = 'solid'),
    axis.line.x =  element_line(linetype = 'solid'),
    plot.background = element_rect(fill='palegoldenrod',),
    panel.grid = element_blank())+
  labs(
    title = 'Pokemons and their maximum Combat Power (MCP)',
    subtitle = 'Distribution of the MCP by type within the game Pokemon GO®.',
    x='Type',
    y='MCP',
    caption = 'Based on Pokemon Go API, from "https://rapidapi.com/Chewett/api/pokemon-go1/"'
  )+
  annotation_raster(volcarona,xmin=0.7,xmax =1.4,ymin=3700,ymax=4900,interpolate = F)+
  annotation_raster(zarude,xmin=1.6,xmax =2.5,ymin=3900,ymax=5100,interpolate = F)+
  annotation_raster(reshiram,xmin=2.6,xmax =3.5,ymin=4100,ymax=5300,interpolate = F)+
  annotation_raster(xerneas,xmin=3.6,xmax =4.5,ymin=3850,ymax=5050,interpolate = F)+
  annotation_raster(spectrier,xmin=4.6,xmax =5.5,ymin=3450,ymax=4650,interpolate = F)+
  annotation_raster(dialga,xmin=5.6,xmax =6.5,ymin=4100,ymax=5300,interpolate = F)+
  
  annotate(geom='text',label='Volcarona = 3632',x=0.5,y=3632,hjust=0,color='olivedrab3',family='A',size=3.5,fontface='bold')+
  annotate(geom='text',label='Zarude = 3833',x=1.75,y=3833,hjust=0,color='gray8',family='A',size=3.5,fontface='bold')+
  annotate(geom='text',label='Reshiram = 4038',x=2.75,y=4038,hjust=0,color='darkred',family='A',size=3.5,fontface='bold')+
  annotate(geom='text',label='Xerneas = 3781',x=3.75,y=3781,hjust=0,color='hotpink',family='A',size=3.5,fontface='bold')+
  annotate(geom='text',label='Spectrier = 3385',x=4.75,y=3385,hjust=0,color='plum4',family='A',size=3.5,fontface='bold')+
  annotate(geom='text',label='Dialga = 4038',x=5.75,y=4038,hjust=0,color='steelblue4',family='A',size=3.5,fontface='bold')


## call your visuals and that's all!
plot1
plot2
plot3