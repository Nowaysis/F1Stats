#' Title
#'
#' @param l liste de noms de pilotes
#'
#' @return 3 premières lettres du nom d'un pilote ainsi que son prenom
#' @export
#'
#'
Abrv4<-function(l){
  list<-0
  for(i in 1:length(l)){
    nom<-substr(l[i],1,6)
    list[i]<-nom
  }
  return(list)
}

#' Title
#'
#' @param ch premier caractere
#' @param ch2 second caractere
#'
#' @return concatenation des caracteres
#' @export
#'
#' @examples "je"%+%"suis
"%+%"<-function(ch,ch2){
  paste(ch,ch2,sep="")
}

