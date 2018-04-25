library(dplyr)
library(stringr)
library(wordcloud)
library(ggpubr)


tabele<-list.files(getwd(), ".*[^R]$",recursive = T)

Tab<-data.frame()
# łaczymy wszytskie tabele
for(i in 1:length(tabele)){
  Tab<-rbind(Tab,read.table(tabele[i]))
}


#                     HISTOGRAM

t<-Tab %>% group_by(pakiet,funkcja)%>%
 # filter(!(pakiet %in% c("base","stats","methods")))%>%
  count%>%
  arrange(desc(n)) %>% 
  as.data.frame() %>% 
  slice(1:45) 

ggbarplot(na.omit(t),x = "funkcja", y = "n",
          fill = "pakiet",              
          color = "black",           
          palette = "Set3",          
          sort.val = "desc",       
          sort.by.groups = FALSE,    
          x.text.angle = 90,        
          ylim=c(0,1000)
)+
  geom_text(aes(label = n), vjust = -.81)+
  ggtitle("Zestawienie najczęściej używanych funkcji")+
  theme(plot.title = element_text(size=17, face="bold",hjust = 0.5, color="black"))+
  theme(axis.title.x = element_text(size = 14, angle = 0, face = "italic",color="grey40"), 
        axis.title.y = element_text(size = 14, angle = 90, face = "italic", color="grey40"))+
  xlab("Funkcja") + ylab("Liczba wystąpień")




