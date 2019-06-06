  # install.packages("httr") à besoin de sudo apt-get install libssl-dev
  library(httr)
  #install.packages("jsonlite")
  library(jsonlite)
  #install.packages("dplyr")
  library(dplyr)
  # ----------------------------------------------Faire venir les données avec GET------------------------------------------------------------------
  colnames(Mensuelfeatures)
  
  MensuelGeoJson <-"https://services1.arcgis.com/HzzPcgRsxxyIZdlU/arcgis/rest/services/mes_centre_val_de_loire_mensuel_poll_princ_1/FeatureServer/0/query?where=1%3D1&outFields=*&outSR=4326&f=json"
  
  
  MensuelGET<-httr::GET(MensuelGeoJson)                              #Communiquer avec l'API gâce à l'URL
  Mensuelcontent <- httr::content(MensuelGET)                        #Lire les données
  MensuelFROM <- jsonlite::fromJSON(Mensuelcontent, flatten = TRUE)  #Exctraires le contenu et organiser avec les titres flatten = TRUE
  Mensuelfeatures<-MensuelFROM$features
  
  #---------------------------------------------------------Trier le tableau-------------------------------------------------------------------------------------------------
  

  ListeStations<-Mensuelfeatures %>%
    group_by(attributes.nom_com,attributes.nom_polluant,attributes.nom_station,attributes.code_station,attributes.x_wgs84,attributes.y_wgs84,attributes.influence,attributes.statut_valid) %>%
    summarise(moyenne= mean(attributes.valeur))
  ListeStations<-dplyr::filter(ListeStations, attributes.statut_valid == 1,attributes.influence !="Rurale régionale")
  
  ListeMoyennePM10<-dplyr::filter(ListeStations, attributes.nom_polluant =="PM10")
  
  ListeMoyennePM2.5<-dplyr::filter(ListeStations, attributes.nom_polluant =="PM2.5")
  
  ListeMoyenneO3<-dplyr::filter(ListeStations, attributes.nom_polluant =="O3")
  
  ListeMoyenneNO2<-dplyr::filter(ListeStations, attributes.nom_polluant =="NO2")
  
  
  #------------------------------------------Graphique avec ggplot------------------------------------------------------------
  
  
  #LSNO2  All 17 stations are designated Urbaine dark brown 
  #NO2  Dioxyde d'azote   
  # Valeurs limites En moyenne annuelle : 40 µg/m³ 
  # Seuils de recommandation  En moyenne horaire : 200 µg/m³   
  # Seuils d'alerte  400 µg/m3 dépassé pendant 3 h consécutives - 200 µg/m3 si dépassement de ce seuil la veille, et risque de dépassement de ce seuil le lendemain.
  
  LSNO2 <-ListeStations %>% #Afficher les colonnes à numérotées 4,5 et 18,19
    group_by(attributes.nom_com,attributes.nom_polluant,attributes.code_station,attributes.nom_station, attributes.x_wgs84,attributes.y_wgs84,attributes.statut_valid,attributes.influence) %>%
    summarise(moyenne= mean(ListeStations$moyenne))
  LSNO2 <-dplyr::filter(ListeStations, attributes.statut_valid == 1, attributes.nom_polluant == "NO2")
  
  Commune2 <- LSNO2$attributes.nom_com
  Polluant.NO2 <- LSNO2$attributes.nom_polluant
  Station2 <- LSNO2$attributes.nom_station
  Moyenne2 <- LSNO2$moyenne
  Influence2 <- LSNO2$attributes.influence
  
  ggplot(data = LSNO2, aes(x=Station2, y=Moyenne2, fill = Polluant.NO2)) + geom_col  (position = "dodge")+
    labs(title="Moyenne des relevés mensuels de NO2 (Dioxyde d'azote) μg/m³", y="Valeur μg/m³", x="Commune", 
         caption="Source: Lig'Air - Concentrations moyennes mensuelles de polluants dans l'air ambiant en région Centre-Val de Loire", 
         subtitle="    Valeurs limites en moyenne annuelle : 40 µg/m³ 
      Seuils de recommandation en moyenne horaire : 200 µg/m³
      Seuils d'alerte : 400 µg/m³ dépassé pendant 3 h consécutives")+
    geom_col(fill = "coral4")+
    coord_flip()+
    theme(legend.position = "none")+
    geom_hline(yintercept = 40, linetype="dotted", color = "red", size=1.5)+
    theme(title=element_text(size=10, face="bold", colour="coral4"))+
    theme(plot.subtitle=element_text(size=9, face="plain", color="gray24"))
  
  
  #LSO3 The 14 stations include a mix of Urbaine, Périurbaine, Rurale régionale, Rurale nationale very pale blue 
  # O3   Ozone    
  #valeurs limites :  N/A  
  #Objectifs de qualité ; Seuil de protection de la santé En moyenne sur 8 heures :  120 µg/m3
  #Seuils de recommandation :   En moyenne horaire :  180 µg/m3
  #Seuils d'alerte  : N/A En moyenne horaire :  240 µg/m3 
  
  LSO3 <-ListeStations %>% #Afficher les colonnes à numérotées 4,5 et 18,19
    group_by(attributes.nom_com,attributes.nom_polluant,attributes.code_station,attributes.nom_station, attributes.x_wgs84,attributes.y_wgs84,attributes.statut_valid,attributes.influence) %>%
    summarise(moyenne= mean(ListeStations$moyenne))
  LSO3 <-dplyr::filter(ListeStations, attributes.statut_valid == 1, attributes.nom_polluant == "O3")
  
  Commune3 <- LSO3$attributes.nom_com
  Polluant.O3 <- LSO3$attributes.nom_polluant
  Station3 <- LSO3$attributes.nom_station
  Moyenne3 <- LSO3$moyenne
  Influence3 <- LSO3$attributes.influence
  
  ggplot(data = LSO3, aes(x=Station3, y=Moyenne3, fill = Polluant.O3)) + geom_col  (position = "dodge")+
    labs(title="Moyenne des relevés mensuels de O3 (Ozone) μg/m³", y="Valeur μg/m³", x="Commune", 
         caption="Source: Lig'Air - Concentrations moyennes mensuelles de polluants dans l'air ambiant en région Centre-Val de Loire", 
         subtitle="    Objectifs de qualité ; Seuil de protection de la santé En moyenne sur 8 heures :  120 µg/m³ 
      Seuils de recommandation en moyenne horaire : 180 µg/m³
      Seuils d'alerte en moyenne horaire :  240 µg/m³")+
    geom_col(fill = "skyblue2")+
    coord_flip()+
    theme(legend.position = "none")+
    theme(legend.position = "none")+
    geom_hline(yintercept = 120, linetype="dotted", color = "red", size=1.5)+
    theme(title=element_text(size=10, face="bold", colour="coral4"))+
    theme(title=element_text(size=10, face="bold", colour="skyblue3"))+
    theme(plot.subtitle=element_text(size=9, face="plain", color="gray24"))
  
  
  #LSPM10 All 13 observations stations are designated Urbaine
  #PM10 (Particules fines de diamètre inférieur ou  égal à 10 micromètres)
  #valeurs limites En moyenne annuelle : 40 µg/m³
  #Seuils de recommandation En moyenne sur 24h :  50 µg/m3
  #Seuils d'alerte En moyenne sur 24h : 80 µg/m3
  
  LSPM10 <-ListeStations %>% #Afficher les colonnes à numérotées 4,5 et 18,19
    group_by(attributes.nom_com,attributes.nom_polluant,attributes.code_station,attributes.nom_station, attributes.x_wgs84,attributes.y_wgs84,attributes.statut_valid,attributes.influence) %>%
    summarise(moyenne= mean(ListeStations$moyenne))
  LSPM10 <-dplyr::filter(ListeStations, attributes.statut_valid == 1, attributes.nom_polluant == "PM10")
  
  Commune10 <- LSPM10$attributes.nom_com
  Polluant.PM10 <- LSPM10$attributes.nom_polluant
  Station10 <- LSPM10$attributes.nom_station
  Moyenne10 <- LSPM10$moyenne
  Influence10 <- LSPM10$attributes.influence
  
  ggplot(data = LSPM10, aes(x=Station10, y=Moyenne10, fill = Polluant.PM10)) + geom_col  (position = "dodge")+
    labs(title="Moyenne des relevés mensuels de PM10 \n(Particules fines de diamètre ≤ 10 μm) μg/m³", y="Valeur  μg/m³", x="Commune", 
         caption="Source: Lig'Air - Concentrations moyennes mensuelles de polluants dans l'air ambiant en région Centre-Val de Loire", 
         subtitle="    Valeurs limites en moyenne annuelle : 40 µg/m³ 
      Seuils de recommandation en moyenne sur 24h : 50 µg/m³
      Seuils d'alerte en moyenne sur 24h : 80 µg/m³")+
    geom_col(fill = "yellow4")+
    coord_flip()+
    theme(legend.position = "none")+
    geom_hline(yintercept = 40, linetype="dotted", color = "red", size=1.5)+
    theme(title=element_text(size=10, face="bold", colour="yellow4"))+
    theme(plot.subtitle=element_text(size=9, face="plain", color="gray24"))
  
  
  #LSPM25 has 9 observations, 8 Urbaine, Rurale nationale
  #PM2,5 (Particules fines de diamètre inférieur ou égal à 2,5 micromètres)   
  #valeurs limites En moyenne annuelle 25 µg/m³  20 µg/m3 en 2020 (à confirmer) 
  #Seuils de recommandation :  N/A
  #Seuils d'alerte  : N/A
  
  LSPM25 <-ListeStations %>% #Afficher les colonnes à numérotées 4,5 et 18,19
    group_by(attributes.nom_com,attributes.nom_polluant,attributes.code_station,attributes.nom_station, attributes.x_wgs84,attributes.y_wgs84,attributes.statut_valid,attributes.influence) %>%
    summarise(moyenne= mean(ListeStations$moyenne))
  LSPM25 <-dplyr::filter(ListeStations, attributes.statut_valid == 1, attributes.nom_polluant == "PM2.5")
  
  Commune25 <- LSPM25$attributes.nom_com
  Polluant.PM2.5 <- LSPM25$attributes.nom_polluant
  Station25 <- LSPM25$attributes.nom_station
  Moyenne25 <- LSPM25$moyenne
  Influence25 <- LSPM25$attributes.influence
  
  ggplot(data = LSPM25, aes(x=Station25, y=Moyenne25, fill = Polluant.PM2.5)) + geom_col  (position = "dodge")+
    labs(title="Moyenne des relevés mensuels de PM2.5 \n(Particules fines de diamètre ≤ 2.5 μm) μg/m³", y="Valeur μg/m³", x="Commune", 
         caption="Source: Lig'Air - Concentrations moyennes mensuelles de polluants dans l'air ambiant en région Centre-Val de Loire", 
         subtitle="    Valeurs limites en moyenne annuelle : 25 µg/m³ (20 µg/m³ en 2020 à confirmer)  
      Seuils de recommandation en moyenne sur 24h : aucune information
      Seuils d'alerte en moyenne sur 24h : aucune information")+
    geom_col(fill = "orange3")+
    coord_flip()+
    theme(legend.position = "none")+
    geom_hline(yintercept = 25, linetype="dotted", color = "red", size=1.5)+
    theme(title=element_text(size=10, face="bold", colour="orange3"))+
    theme(plot.subtitle=element_text(size=9, face="plain", color="gray24"))
  
  
  
  
  
  
  
  
  
  
  
  #------------------------------------------Carte final method hopefully----------------------------------------------------------------------
  
  ## We will use the mean of PM10 and PM2.5
  
  library(leaflet)
  library(rgdal)
  library(sf)
  
  
  # get domain of numeric data
  domainPM10 <- range(ListeMoyennePM10$moyenne)
  domainPM2.5 <- range(ListeMoyennePM2.5$moyenne)
  domainO3 <- range(ListeMoyenneO3$moyenne)
  domainNO2 <- range(ListeMoyenneNO2$moyenne)
  
  # Palette de couleur perso--------------------------------------------------------------------------------------------------------------------
  
  palPM10<-colorNumeric(palette = c('green', 'yellow', 'orange', 'red'),domain = domainPM10)
  palPM2.5<-colorNumeric(palette = c('green', 'yellow', 'orange', 'red'),domain = domainPM2.5)
  palO3<-colorNumeric(palette = c('green', 'yellow', 'orange', 'red'),domain = domainO3)
  palNO2<-colorNumeric(palette = c('green', 'yellow', 'orange', 'red'),domain = domainNO2)
                       
  #---Ajout des contours des départements avec readOGR et ajout à la carte avec addPolygons------------------------------------------------------------------------------------------
  
  Contour <- readOGR(dsn ="/home/tarik/Documents/Utilisation API journée5/departements-centre-val-de-loire.geojson")
  leaflet() %>% 
    addTiles() %>%
    setView(lng = 1.4107, lat = 47.2850, zoom = 7.3) %>% 
    addPolygons(data = Contour,fillOpacity = 0) %>%
   
  #Les CircleMarkers-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------  
    
     addCircleMarkers(lng=ListeMoyennePM10$attributes.x_wgs84, lat=ListeMoyennePM10$attributes.y_wgs84,group = "PM10" , radius=ListeMoyennePM10$moyenne, weight = 3,color = palPM10(ListeMoyennePM10$moyenne),popup = paste("Code Station = ",ListeMoyennePM10$attributes.code_station,"<br>",
                                                                                                                                                                                            "Moyenne",ListeMoyennePM10$attributes.nom_polluant, "= ",ListeMoyennePM10$moyenne,"<br>",
                                                                                                                                                                                            "<img src='https://ww3.arb.ca.gov/research/aaqs/common-pollutants/pm/pm_sizes.jpg' height='40' width='50'>","<br>",
                                                                                                                                                                                            "<a href='https://www.respire-asso.org/particules-en-suspension-pm10-pm-25/'>Voir le site </a>"))%>%
    
    addCircleMarkers(lng=ListeMoyennePM2.5$attributes.x_wgs84, lat=ListeMoyennePM2.5$attributes.y_wgs84,group = "PM2.5" , radius=ListeMoyennePM2.5$moyenne*2, weight = 3,color = palPM2.5(ListeMoyennePM2.5$moyenne),popup = paste("Code Station = ",ListeMoyennePM2.5$attributes.code_station,"<br>",
                                                                                                                                                                                                           "Moyenne",ListeMoyennePM2.5$attributes.nom_polluant, "= ",ListeMoyennePM2.5$moyenne,"<br>",
                                                                                                                                                                                                           "<img src='https://ww3.arb.ca.gov/research/aaqs/common-pollutants/pm/pm_sizes.jpg' height='40' width='50'>","<br>",
                                                                                                                                                                                                           "<a href='https://www.respire-asso.org/particules-en-suspension-pm10-pm-25/'>Voir le site </a>"))%>%                                                                                                                                  
   
    addCircleMarkers(lng=ListeMoyenneO3$attributes.x_wgs84, lat=ListeMoyenneO3$attributes.y_wgs84,group = "O3" , radius=ListeMoyenneO3$moyenne/2, weight = 3,color = palO3(ListeMoyenneO3$moyenne),popup = paste("Code Station = ",ListeMoyenneO3$attributes.code_station,"<br>",
                                                                                                                                                                                                                                 "Moyenne",ListeMoyenneO3$attributes.nom_polluant, "= ",ListeMoyenneO3$moyenne,"<br>",
                                                                                                                                                                                                                 "<img src='https://previews.123rf.com/images/molekuul/molekuul1409/molekuul140900148/31177822-l-ozone-trioxygen-o3-mol%C3%A9cule-la-structure-chimique-les-atomes-sont-repr%C3%A9sent%C3%A9s-par-des-sph%C3%A8res-avec-coda.jpg' height='40' width='50'>","<br>",
                                                                                                                                                                                                                                 "<a href='https://www.respire-asso.org/ozone-o3/'>Voir le site </a>"))%>%     
  
    addCircleMarkers(lng=ListeMoyenneNO2$attributes.x_wgs84, lat=ListeMoyenneNO2$attributes.y_wgs84,group = "NO2" , radius=ListeMoyenneNO2$moyenne/2, weight = 3,color = palNO2(ListeMoyenneNO2$moyenne),popup = paste("Code Station = ",ListeMoyenneNO2$attributes.code_station,"<br>",
                                                                                                                                                                                                                 "Moyenne",ListeMoyenneNO2$attributes.nom_polluant, "= ",ListeMoyenneNO2$moyenne,"<br>",
                                                                                                                                                                                                                 "<img src='https://www.alamy.com/thumbs/4/a5056a6f-b6f9-44aa-8e90-9517da5e977d/D8AWCY.jpg' height='40' width='50'>","<br>",
                                                                                                                                                                                                                 "<a href='https://www.respire-asso.org/dioxyde-dazote-no2/'>Voir le site </a>"))%>%   
    #Legende------------------------------------------------------------------------------------------------------------------ 
    addLegend(pal = palPM10,
              values  = ListeMoyennePM10$moyenne,
              position = "bottomright",
              title = "Valeur moyenne du polluant PM10<br> en µg/m3",
              group = "PM10")%>%
    
    addLegend(pal = palPM2.5,
              values  = ListeMoyennePM2.5$moyenne,
              position = "bottomright",
              title = "Valeur moyenne du polluant PM2.5 <br> en µg/m3",
              group = "PM2.5")%>%
    
    addLegend(pal = palO3,
              values  = ListeMoyenneO3$moyenne,
              position = "bottomleft",
              title = "Valeur moyenne du polluant Ozone <br> en µg/m3",
              group = "O3")%>%
   
    addLegend(pal = palNO2,
              values  = ListeMoyenneNO2$moyenne,
              position = "bottomleft",
              title = "Valeur moyenne du polluant Ozone <br> en µg/m3",
              group = "NO2")%>%
  
    #Checkbox---------------------------------------------------------------------------------------------------------------
    addLayersControl(overlayGroups = c("PM10", "PM2.5","O3","NO2"),
                     options = layersControlOptions(collapsed = FALSE))%>%
    hideGroup(group = "O3")%>%
    hideGroup(group = "PM10")%>%
    hideGroup(group = "PM2.5")%>%
    hideGroup(group = "NO2")
  


