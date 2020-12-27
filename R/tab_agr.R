#' Title
#'
#' @param n année du GP
#' @param p numéro du GP
#' @import reshape
#' @import ggplot2
#' @import directlabels
#' @import ggrepel
#' @import grDevices
#' @return a plot containing a representation of the evolution of the points during a season
#' @export
#'
#'
#'

tab_agr<-function(n,p){
  df<-Classements_Pilotes_Finaux[[n-1949]]
  if(p==1){
    df<-df[c(1,2)]
    df$Pilotes<-Abrv4(df$Pilotes)
    plot<-ggplot(df, aes(x=Pilotes, y=df[[2]], group= Pilotes, color = Pilotes))+
      geom_point()
  }else{
    l2<-names(df[1:p+1])
    df<-df[1:(p+1)]
    for(i in 1:(length(l2)-1)){
      df[l2[[i+1]]]<-df[l2[[i]]]+df[l2[[i+1]]]
    }
    k<-length(df$Pilotes)
    df$Pilotes<-Abrv4(df$Pilotes)
    truc<-melt(df,id.var="Pilotes")
    mycolors <- colorRampPalette(brewer.pal(8, "Paired"))(k)
    plot<-ggplot(truc, aes(x=variable, y=value, group = Pilotes, color = Pilotes, label = Pilotes))+
      geom_line()+
      scale_color_manual(values=mycolors)+
      geom_label_repel(data=truc[truc$variable==names(df)[length(df)],])+
      theme(legend.position="none")
    ggtitle("Evolution du nombre de points après les résultats du GP sélectionné")
  }


  return(plot)
}
