#' Title
#'
#' @param n année du GP
#' @param p numéro du GP
#' @import reshape
#' @import ggplot2
#' @import directlabels
#' @return a plot containing a representation of the evolution of the points during a season
#' @export
#'
#'
#'

tab_agr<-function(n,p){
  if(p==1){
    print("Passez au GP suivant")
  }
  df<-Classements_Pilotes_Finaux[[n-1949]]
  l2<-names(Classements_Pilotes_Finaux[[n-1949]][1:p+1])
  df<-df[1:(p+1)]
  for(i in 1:(length(l2)-1)){
    df[l2[[i+1]]]<-df[l2[[i]]]+df[l2[[i+1]]]
  }
  df$Pilotes<-Abrv4(df$Pilotes)
  truc<-melt(df,id.var="Pilotes")
  plot<-ggplot(truc, aes(x=`variable`, y=`value`, group = `Pilotes`, colour = `Pilotes`))+
    geom_line()+
    geom_dl(aes(label=Pilotes),method=list("last.qp"))+
    scale_fill_manual(values=colors)+
    ggtitle("Evolution du nombre de points après les résultats du GP sélectionné")

  return(plot)
}
