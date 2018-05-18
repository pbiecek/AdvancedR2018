library(data.table)
library(dplyr)
library(stringr)
library(ggpubr)
library(RColorBrewer) 


tabele<-list.files(getwd(), ".*[^R]$", recursive = T)
Tab<-data.frame()
# łaczymy wszytskie tabele
for(i in 1:length(tabele)){
  tabik<-read.table(tabele[i])
  Tab<-rbind(Tab, tabik)
}

#       Wykres kołowy

mypal<-c(brewer.pal(11, "Spectral"), brewer.pal(8,"Dark2"), brewer.pal(8,"Set3"), brewer.pal(12,"Paired"), brewer.pal(8,"Set2"),
         brewer.pal(8,"Set1"),  brewer.pal(9,"Pastel1"), brewer.pal(8,"Pastel2"))



# kolowy po autorach wg ilosc funkcji uzytych z danego pakietu
Tab1<-Tab%>%group_by(pakiet,funkcja, autor)%>%filter(autor!="pakiet")%>%count
Tab2<-Tab1 %>% group_by(pakiet, autor)%>% summarize(n=sum(n))


ggplot(data = na.omit(Tab2), aes(x = "", y = n, fill = pakiet )) + 
  geom_bar(stat = "identity", position = position_fill()) +
  coord_polar(theta = "y") +
  facet_wrap(~ autor)  +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank()) + 
  theme(legend.position='right') + 
  guides(fill=guide_legend(ncol=3, byrow=TRUE))+
  ggtitle("Proporcje wykorzytanych pakietów")+
  theme(plot.title = element_text(size=25, face="bold",hjust = 0.5, color="black"))+
  scale_fill_manual(values=mypal)


# kolowy po autorach poza basem
Tab3<-Tab2%>%filter(pakiet!="base")


ggplot(data = na.omit(Tab3), aes(x = "", y = n, fill = pakiet )) + 
  geom_bar(stat = "identity", position = position_fill()) +
  coord_polar(theta = "y") +
  facet_wrap(~ autor)  +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank()) + 
  theme(legend.position='right') + 
  guides(fill=guide_legend(ncol=3, byrow=TRUE))+
  ggtitle("Proporcje wykorzytanych pakietów bez pakietu base")+
  theme(plot.title = element_text(size=25, face="bold",hjust = 0.5, color="black"))+
  scale_fill_manual(values=mypal)



#  Redukcja ilosci pakietow przypadajacych na jednego autora: 3 najczesciej wykorzystywane pakiety + reszta

temp<- Tab2%>%mutate(reszta = sum(n[-(1:3)]))
zast<-temp %>% select(autor,reszta)%>% unique
zast["pakiet"]<-"reszta"
zast<-zast[,c(1,3,2)]
names(zast2)[2]<-"pakiet"
names(zast2)[3]<-"n"
Tab2_<-setDT(Tab2)[order(autor,-n), .SD[1:3], by=autor]
Reszta<-rbind(as.data.frame(Tab2), zast2)%>% group_by(autor)%>%arrange(autor,desc(n)) 
Tab3<-setDT(Reszta)[order(autor,-n), .SD[1:4], by=autor]

