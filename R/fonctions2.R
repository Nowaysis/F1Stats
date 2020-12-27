#' Title
#'
#' @param pilote pilote de f1
#'
#' @return differentes stats sur le pilote
#' @export
#'
#'
Stat_Pilote<-function(pilote){

  Titres<-0
  for(i in 1:71){
    df<-Classements_Pilotes_Finaux[[i]]
    if(isTRUE(df$Pts[df$Pilotes==pilote]==max(df$Pts) & pilote %in% df$Pilotes)){
      Titre=1
    }else{
      Titre=0
    }
    Titres<-Titres+Titre
  }


  total<-0
  for(i in 1:71){
    df<-Classements_Pilotes_Finaux[[i]]
    if(length(df$Pts[df$Pilotes==pilote])>0){
      pts<-df$Pts[df$Pilotes==pilote]
    }else{
      pts<-0
    }
    total<-total+pts
  }
  Nb_GP_Total<-0
  for(i in 1:71){
    df<-Courses_F1[[i]]
    Nb_GP_Annee<-0
    for(j in 1:length(df)){
      if(isTRUE(pilote %in% df[[j]]$Pilote)){
        Nb_GP_Annee<-Nb_GP_Annee+1
      }else{
        Nb_GP_Annee<-Nb_GP_Annee
      }

    }
    Nb_GP_Total[i]<-Nb_GP_Annee
  }
  Nombre_GP<-sum(Nb_GP_Total)

  Victoires<-0
  for(i in 1:71){
    df<-Classements_Pilotes_Finaux[[i]]
    Victoire_Annee<-0
    for(j in 2:(length(names(df))-1)){
      if(isTRUE(df[[j]][df$Pilotes==pilote]==max(df[j]) & pilote %in% df$Pilotes)){
        Place=1
      }else{
        Place=0
      }
      Victoire_Annee<-Victoire_Annee+Place
    }
    Victoires[i]<-Victoire_Annee

  }

  Podiums<-0
  for(i in 1:71){
    df<-Classements_Pilotes_Finaux[[i]]
    Podiums_Annee<-0
    for(j in 2:(length(names(df))-1)){
      liste_points<-sort(df[[j]],TRUE)
      if(isTRUE(pilote %in% df$Pilotes & df[[j]][df$Pilotes==pilote]>=liste_points[3])){
        pod=1
      }else{
        pod=0
      }
      Podiums_Annee<-Podiums_Annee+pod
    }
    Podiums[i]<-Podiums_Annee
  }

  Poles_Positions<-0
  for(i in 1:71){
    df<-Qualifs_F1[[i]]
    Poles_Annee<-0
    for(j in 1:length(df)){
      df2<-df[[j]]
      if(isTRUE(pilote %in% df2$Pilote & df2$Pos[df2$Pilote==pilote]=="1")){
        Pole=1
      }else{
        Pole=0
      }
      Poles_Annee<-Poles_Annee+Pole
    }
    Poles_Positions[i]<-Poles_Annee
  }
  Position_Par_Course<-list()
  for(i in 1:71){
    df<-Qualifs_F1[[i]]
    Position<-0
    for(j in 1:length(df)){
      df2<-df[[j]]
      if(isTRUE(pilote %in% df2$Pilote)){
        pos<-df2$Pos[df2$Pilote==pilote]
        Position[j]<-as.integer(pos)
      }else{
        Position[j]<-0
      }
    }
    Position_Par_Course[[i]]<-Position
  }
  Somme_Pos=0
  for(i in 1:71){
    sum=sum(Position_Par_Course[[i]])
    Somme_Pos=Somme_Pos+sum
  }
  Moyenne_Position=round(Somme_Pos/Nombre_GP,2)

  Tours_en_tete<-0
  for(i in 1:71){
    df<-Tour_Par_Tour_F1[[i]]
  }

  return(c(Titres, total, Nombre_GP, sum(Victoires),sum(Podiums),sum(Poles_Positions), Moyenne_Position))
}

#' Title
#'
#' @param pilote1 pilote de f1
#' @param pilote2 second pilote de f1
#' @import ggplot2
#' @import ggpubr
#'
#' @return comparaison entre les pilotes, plusieurs barplots concernant leurs statistiques personnelles
#' @export
#'
#'
compare2<-function(pilote1="L. HAMILTON",pilote2="N. ROSBERG"){
  df<-df_Pilotes[c(pilote1,pilote2),]
  r<-list()
  for(i in 1:7){
    temp<-ggplot(df,aes(x=row.names(df),y=.data[[names(df)[i]]]))+geom_bar(stat='identity',fill="steelblue")+
      geom_text(aes(label=.data[[names(df)[i]]]), vjust=3.4, color="white", size=5)+
      theme(axis.title=element_blank())
    r[[i]]<-temp
  }
  plot<-ggarrange(r[[1]], r[[2]], r[[3]], r[[4]], r[[5]], r[[6]], r[[7]], labels=c("Titres","Points totaux","GP disput?s","Victoires", "Podiums","Poles Positions", "Moyenne Qualif"),vjust=1.0,font.label=list(size=12),ncol=4, nrow=2)
  return(plot)
}
