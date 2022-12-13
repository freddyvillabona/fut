



library(ggplot2)
library(dplyr)
library(hrbrthemes)
library(viridis)
library(readxl)
library(patchwork) 
library(ggflags)
library(stringr)


cup <- read.csv("cup.csv", sep=";", stringsAsFactors = FALSE)
cup <- cup[with(cup, order(-cup$WIN.WORLD.CUP)), ] # Orden inverso

cup$TEAM <- gsub("\n","",cup$TEAM)
cup$TEAM <- gsub(" ","",cup$TEAM)

cup$TEAM <- str_replace_all(cup$TEAM," ", "")  


cup$TEAM



cup <- filter(cup, TEAM %in% c("Argentina","Morocco", "Croatia", "France") )

#cup <- head(cup,10)






names(cup)[12] = "Win"


new <- c(35, 37, 16, 13) 
    

cup2 <- cbind(cup, new)


ggplot(data=cup2, aes(x= reorder(TEAM, -Win) , y=Win, group=1)) +
  ggtitle("Ganar el Mundial de Fútbol 2022 (Top 4)") +
  xlab("País") +
  ylab("Porcentaje") +
  
  geom_line(col="gray",size=1.1) +
  geom_point(size=4, col="gray") +
  
  geom_line(data=cup2, aes(x= reorder(TEAM, -new) , y=new, group=1),  col="blue", size=1.5) +
  geom_point(data=cup2, aes(x= reorder(TEAM, -new) , y=new, group=1), size=6, col="blue") +
  
  # geom_bar(stat="identity", fill=cup$paleta)+
  geom_text(data=cup2,aes(label=paste(Win, "%") ), vjust=2.3, color="gray", size=3.5)+
  
  geom_text(data=cup2, 
            aes(x= reorder(TEAM, -new) , y=new, size=20),
            label=paste(new, "%"), vjust=-1.6, color="blue", size=3.5)+
  
  geom_flag(data=cup2, aes(x = reorder(TEAM, -Win) , y=-7, country = code), size=25 ) +
  theme_ipsum(grid = F, base_family = "sans") +
  labs(subtitle = "Datos: Soccer Power Index (SPI)") +
  
  geom_segment(aes(x="Croatia",
                   xend = "Morocco", 
                   y = 50,
                   yend = 50),
               size = 5, 
               col = "blue") +
  
  geom_segment(aes(x="Croatia",
                   xend = "Morocco", 
                   y = 47,
                   yend = 47),
               size = 5,
               col = "gray") +
  
  annotate("text", x = "Argentina", y = 50, 
           label = "Predicción semifinales (Mar 13 dic 17:30 GTM+1)") +
  annotate("text", x = "Argentina", y = 47, 
           label = "Predicción antes de comenzar el mundial              ") +
  theme(
        axis.text.x=element_text(margin = margin(t = 25)))




scale_alpha_manual(name = NULL,
                   values = c(1, 1),
                   breaks = c("Observed", "Fitted") )



scale_color_identity(name = "", labels = c("Actual (Jue 1 dic 21:07)", "Antes de comenzar el mundial"),
                     guide = guide_legend())














