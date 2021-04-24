library(dplyr)
library(lubridate) 
library(tidyverse)
library(ggplot2)
library(plotly)
library(corrplot)
library(lattice)

#Zadanie 1

#wczytanie danych z pliku SeoulBikeData i przypisanie do zmiennej BikeData
BikeData <- read.csv("Florczak_dane_surowe.csv", stringsAsFactors = FALSE)

#zmiana nazw kolumn 
colnames(BikeData) <- c("Date", "Rented_Bike_Count", "Hour", "Temperature_°C",
                        "Humidity_%", "Wind_speed_m/s", "Visibility_10m",
                        "Dew_point_temperature_°C", "Solar_Radiation_MJ/m2",
                        "Rainfall_mm", "Snowfall_cm", "Seasons", "Holiday",
                        "Functioning_Day" )

#wyswietlenie informacji o zawartych danych (ilosc wierszy/kolumn, typ danych)
str(BikeData)

#zamiana stringa znajdujcego sie w polu "Date" na date w celu pozniejszego posortowania
BikeData[ , 1 ] <- dmy(BikeData[, 1])

#ponowne sprawdzenie typow danych
str(BikeData)

#wyswietlenie pierwszych 6 wierszy zestawu danych SeoulBikeData
head(BikeData)

#posortowanie danych rosnaco wedlug daty, nastepnie wedlug ilosci wypozyczonych rowerow
arrange(BikeData, BikeData$Date, BikeData$Rented_Bike_Count)

#sprawdzenie ilosci wystepowania braku danych
table(is.na(BikeData))

#sprawdzenie wartosci w kolumnie "Functioning_Day"
table(BikeData$Functioning_Day)

#usuniecie wierszy w ktorych wypozyczalnia nie byla otwarta
BikeData <- filter(BikeData, BikeData$Functioning_Day == "Yes")

#podsumowanie danych 
summary(BikeData)

#policzenie sumy wypozyczonych rowerow
Rented_Bike_Sum <- sum(BikeData$Rented_Bike_Count)
Rented_Bike_Sum

#dodanie kolumny z informacja o miesiacu
BikeData <- mutate(BikeData, Month=month.abb[month(BikeData$Date)])
BikeData

#zapisanie danych do nowego pliku
write.csv(BikeData, file = "Florczak_dane_przeksztalcone.csv", row.names = FALSE)


#Zadanie 2

#podzielenie danych ze wzgledu na pory roku


#zima

#wybranie danych zebranych w danej porze roku
BikeDataWinter <- filter(BikeData, BikeData$Seasons == "Winter" )

#wypisanie kilku pierwszych wierszy
head(BikeDataWinter)

#zliczenie wierszy dla danej pory roku
count(BikeDataWinter)

#sprawdzenie czy wystepowaly dni kiedy wypozyczalnia byla otwarta ale nie wypozyczyla zadnego roweru
filter(BikeDataWinter, BikeDataWinter$Rented_Bike_Count == 0 & BikeDataWinter$Functioning_Day == "Yes" )

#sprawdzenie kiedy ilosc wypozyczen byla najmniejsza
subset(BikeDataWinter,BikeDataWinter$Rented_Bike_Count==min(BikeDataWinter$Rented_Bike_Count))

#sprawdzenie kiedy ilosc wypozyczen byla najwieksza
subset(BikeDataWinter,BikeDataWinter$Rented_Bike_Count==max(BikeDataWinter$Rented_Bike_Count))

#zliczenie wszystkich wypozyczen rowerow w danej porze roku
Rented_Bike_Sum_Winter <- sum(BikeDataWinter$Rented_Bike_Count)
Rented_Bike_Sum_Winter

#obliczenie sredniej wypozyczen na dany dzien i dolaczenie wyniku do danych
BikeDataWinter <- mutate(BikeDataWinter, Rented_Bike_Mean=mean(BikeDataWinter$Rented_Bike_Count))

#zamiana kolejnosci kolumn danych
BikeDataWinter <- subset(BikeDataWinter, select = c(1,2,15,3,4,5,6,7,8,9,10,11,12,13,14))

#posortowanie danych
arrange(BikeDataWinter, BikeDataWinter$Date, BikeDataWinter$Hour)


#wiosna
BikeDataSpring <- filter(BikeData, BikeData$Seasons == "Spring" )
head(BikeDataSpring)
count(BikeDataSpring)
filter(BikeDataSpring, BikeDataSpring$Rented_Bike_Count == 0 & BikeDataSpring$Functioning_Day == "Yes")
subset(BikeDataSpring,BikeDataSpring$Rented_Bike_Count==min(BikeDataSpring$Rented_Bike_Count))
subset(BikeDataSpring,BikeDataSpring$Rented_Bike_Count==max(BikeDataSpring$Rented_Bike_Count))
Rented_Bike_Sum_Spring <- sum(BikeDataSpring$Rented_Bike_Count)
Rented_Bike_Sum_Spring
BikeDataSpring <- mutate(BikeDataSpring, Rented_Bike_Mean=mean(BikeDataSpring$Rented_Bike_Count))
BikeDataSpring <- subset(BikeDataSpring, select = c(1,2,15,3,4,5,6,7,8,9,10,11,12,13,14))
arrange(BikeDataSpring, desc(BikeDataSpring$Rented_Bike_Count), BikeDataSpring$Hour)


#lato
BikeDataSummer <- filter(BikeData, BikeData$Seasons == "Summer" )
head(BikeDataSummer)
count(BikeDataSummer)
filter(BikeDataSummer, BikeDataSummer$Rented_Bike_Count == 0 & BikeDataSummer$Functioning_Day == "Yes" )
subset(BikeDataSummer,BikeDataSummer$Rented_Bike_Count==min(BikeDataSummer$Rented_Bike_Count))
subset(BikeDataSummer,BikeDataSummer$Rented_Bike_Count==max(BikeDataSummer$Rented_Bike_Count))
Rented_Bike_Sum_Summer <- sum(BikeDataSummer$Rented_Bike_Count)
Rented_Bike_Sum_Summer
BikeDataSummer <- mutate(BikeDataSummer, Rented_Bike_Mean=mean(BikeDataSummer$Rented_Bike_Count))
BikeDataSummer <- subset(BikeDataSummer, select = c(1,2,15,3,4,5,6,7,8,9,10,11,12,13,14))
arrange(BikeDataSpring, BikeDataSpring$`Temperature_°C`, BikeDataSpring$`Wind_speed_m/s`)


#jesien
BikeDataAutumn <- filter(BikeData, BikeData$Seasons == "Autumn" )
head(BikeDataAutumn)
count(BikeDataAutumn)
filter(BikeDataAutumn, BikeDataAutumn$Rented_Bike_Count == 0 & BikeDataAutumn$Functioning_Day == "Yes" )
subset(BikeDataAutumn,BikeDataAutumn$Rented_Bike_Count==min(BikeDataAutumn$Rented_Bike_Count))
subset(BikeDataAutumn,BikeDataAutumn$Rented_Bike_Count==max(BikeDataAutumn$Rented_Bike_Count))
Rented_Bike_Sum_Autumn <- sum(BikeDataAutumn$Rented_Bike_Count)
Rented_Bike_Sum_Autumn
BikeDataAutumn <- mutate(BikeDataAutumn, Rented_Bike_Mean=mean(BikeDataAutumn$Rented_Bike_Count))
BikeDataAutumn <- subset(BikeDataAutumn, select = c(1,2,15,3,4,5,6,7,8,9,10,11,12,13,14))
arrange(BikeDataSpring, BikeDataSpring$Date, desc(BikeDataSpring$Hour))



#Zadanie 3

# 1 ggplot2

ggplot(data=BikeData, aes(x=Date, y=Rented_Bike_Count, color=Seasons))+
  geom_point(shape=20, size=2)+
  scale_color_manual(values=c("#b76935", "#AACC00", "#FFD000", "#3A86FF"))+
  ggtitle("Rented Bike per Day")+
  xlab("Date")+
  ylab("Rented Bike")+
  theme(
        rect=element_rect(fill="gray93"),
        axis.text=element_text(color="gray20"),
        axis.title = element_text(color="gray5", size=11, face="bold"),
        plot.title=element_text(size=16, face="bold", color="gray1", hjust=0.5),
        panel.background = element_rect(fill="gray88"),
        legend.position="right",
        legend.margin=margin(10, 20, 10, 10, "pt"),
        legend.title = element_text(colour="gray5", size=10, face="bold", hjust = 0.5)
      )


# 2 lattice    

barchart_winter <- barchart(data = subset(BikeDataWinter, BikeDataWinter$Rainfall_mm > 0), Rainfall_mm ~ Date | Month,  
         horiz=FALSE, 
         origin = 0,
         layout=c(3,1),
         main = list( label = "Rainfalls in winter", font = 2, cex = 1.5, just = "top"),
         xlab = list( label = "Days", font = 2, cex = 1),
         ylab = list( label = "Rainfalls [mm]", font = 2, cex = 1),
         scales = list(x = list(rot = 90), relation = "free", y = list(rot = 0)),
         col = "#134074",
         ylim=c(0,40),
         box.ratio = 3
)
barchart_winter

barchart_spring <- barchart(data = subset(BikeDataSpring, BikeDataSpring$Rainfall_mm > 0), Rainfall_mm ~ Date | Month,  
                            origin = 0,
                            horiz=FALSE, 
                            layout=c(3,1),
                            main = list( label = "Rainfalls in spring", font = 2, cex = 1.5, just = "top"),
                            xlab = list( label = "Days", font = 2, cex = 1),
                            ylab = list( label = "Rainfalls [mm]", font = 2, cex = 1),
                            scales = list(x = list(rot = 90), relation = "free", y = list(rot = 0)),
                            col = "#134034",
                            ylim = c(0,40),
                            box.ratio = 3
)

barchart_spring

barchart_summer <- barchart(data = subset(BikeDataSummer, BikeDataSummer$Rainfall_mm > 0), Rainfall_mm ~ Date | Month,  
                            origin = 0,
                            horiz=FALSE, 
                            layout=c(3,1),
                            main = list( label = "Rainfalls in summer", font = 2, cex = 1.5, just = "top"),
                            xlab = list( label = "Days", font = 2, cex = 1),
                            ylab = list( label = "Rainfalls [mm]", font = 2, cex = 1),
                            scales = list(x = list(rot = 90), relation = "free", y = list(rot = 0)),
                            col = "#BBBAAA",
                            ylim = c(0,40),
                            box.ratio = 3
)

barchart_summer

barchart_autumn <- barchart(data = subset(BikeDataAutumn, BikeDataAutumn$Rainfall_mm > 0), Rainfall_mm ~ Date | Month,  
                            origin = 0,
                            horiz=FALSE, 
                            layout=c(3,1),
                            main = list( label = "Rainfalls in autumn", font = 2, cex = 1.5, just = "top"),
                            xlab = list( label = "Days", font = 2, cex = 1),
                            ylab = list( label = "Rainfalls [mm]", font = 2, cex = 1),
                            scales = list(x = list(rot = 90), relation = "free", y = list(rot = 0)),
                            col = "#13BCCC",
                            ylim = c(0,40),
                            box.ratio = 3
)

barchart_autumn


plot(barchart_winter, split=c(2,2,2,2))
plot(update(barchart_spring), split = c(1,1,2,2), newpage = FALSE)
plot(update(barchart_summer), split = c(2,1,2,2), newpage = FALSE)
plot(update(barchart_autumn), split = c(1,2,2,2), newpage = FALSE)


# 3 graphics
par(mar=c(3.5, 3.5, 7, 1), mfrow=c(2,2), bg="#e9ecef")

stripchart(`Temperature_°C`~Month,
           data=BikeDataWinter,
           main="Zima",
           xlab=list("Temperatura", col="black", font=2),
           ylab=list("Miesiac",col="black", font=2),
           col= "#3A86FF",
           pch=10,
           ylim=c(-20,40),
           vertical=TRUE,
           
)
stripchart(`Temperature_°C`~Month,
           data=BikeDataSpring,
           main="Wiosna",
           xlab=list("Temperatura", col="black", font=2),
           ylab=list("Miesiac",col="black", font=2),
           col = "#AACC00",
           pch=10,           
           ylim=c(-20,40),
           vertical=TRUE
)
stripchart(`Temperature_°C`~Month,
           data=BikeDataSummer,
           main="Lato",
           xlab=list("Temperatura", col="black", font=2),
           ylab=list("Miesiac",col="black", font=2),
           col= "#FFD000",
           ylim=c(-20,40),
           pch=10,
           vertical=TRUE
)
stripchart(`Temperature_°C`~Month,
           data=BikeDataAutumn,
           main="Jesien",
           xlab=list("Temperatura", col="black", font=2),
           ylab=list("Miesiac",col="black", font=2),
           col = "#b76935",
           ylim=c(-20,40),
           pch=10,
           vertical=TRUE
)
title("Temperatura w poszczegolnych porach roku", line = -2, outer = TRUE, cex.main=1.5, col.main="darkblue")

# 4 plotly

plot_3d <- plot_ly( BikeData, x=~Date, y=~`Wind_speed_m/s`, z=~Rented_Bike_Count, 
                    color=~Seasons, 
                    colors = c("#b76935", "#AACC00", "#FFD000", "#3A86FF"),
                    type="scatter3d", 
                    mode="markers",
                    marker = list( size=4, line=list(color="black", width = 1))
                    )
  
plot_3d %>% layout(title="Wplyw predkosci wiatru na ilosc wypozyczonych rowerow", 
                   titlefont=list(size=25, color="black"),
                   scene = list(
                          xaxis = list(title = "Data", color="black"),
                          yaxis = list(title = "Predkosc wiatru [m/s]", color="black"),
                          zaxis = list(title = "Ilosc wypozyczonych rowerow", color="black")
                          ),
                   paper_bgcolor="#d6e2e9",
                   margin=10
)                   

#5 corrplot
str(BikeData)
correl<-data.frame(BikeData$Rented_Bike_Count, BikeData$Hour, BikeData$`Humidity_%`, BikeData$Visibility_10m)
colnames(correl) <- c("Rented Bikes", "Opening Hours", "Humidity", "Visibility")
par(mfrow=c(1,1))
corrplot(cor(correl), 
         method = "color", 
         type="upper", 
         col=colorRampPalette(c("lightblue", "darkblue"))(50),
         tl.col = "black",
         addCoef.col = "black",
         outline = TRUE,
         title = "Diagram korelacji wybranych wartosci",
         mar = c(0,1,4,1),
         )


  