#' Friends and Foe and Famous function
#' 
#' This is the function that get the friends, foe, and famous person  
#' related to the birthday desired from birthday analyzer.
#' 
#' This function asks the user to input birth year, month and day. 
#' The function will return the related friends, foe, and famous  
#' person on the birthday of this user's input.
#' 
#' @param Year A number in "yyyy" format
#' @param Month A number in "mm" format
#' @param Day A number in "dd" format
#' @return a dataframe of related friends, foe, and famous person of the birthday input
#' @examples
#' Horoscope(1993,11,15)
#' Horoscope(1962,09,14)
#' @export

FFF=function(Year,Month,Day){
  clientinput=paste(Year,Month,Day,sep = "-")
  birthday=as.data.frame(clientinput)
  colnames(birthday)=("Birthday")
  corresnum=as.numeric(as.Date(clientinput))
  corresnum=ifelse(corresnum <365, corresnum+7314 ,ifelse(corresnum>=365 & corresnum<7670,corresnum+7314+1,corresnum+7314+2))
  url=paste("https://www.thehoroscope.co/birthday-analyser/",clientinput,"-horoscope-and-zodiac-sign-meanings-",corresnum,".html",sep = "")
  url=xml2::read_html(url)
  friend1=(rvest::html_text(rvest::html_node(x=url,css="#main > div > div > div > article > div:nth-child(6) > div > ul > ul.bullet-green > li:nth-child(1) > a")))
  friend2=rvest::html_text(rvest::html_node(x=url,css = "#main > div > div > div > article > div:nth-child(6) > div > ul > ul.bullet-green > li:nth-child(2) > a"))
  friend3=rvest::html_text(rvest::html_node(x=url,css = "#main > div > div > div > article > div:nth-child(6) > div > ul > ul.bullet-green > li:nth-child(3) > a"))
  foe1=(rvest::html_text(rvest::html_node(x=url,css="#main > div > div > div > article > div:nth-child(6) > div > ul > ul.bullet-red > li:nth-child(1) > a")))
  foe2=rvest::html_text(rvest::html_node(x=url,css = "#main > div > div > div > article > div:nth-child(6) > div > ul > ul.bullet-red > li:nth-child(2) > a"))
  famous1=(rvest::html_text(rvest::html_node(x=url,css="#main > div > div > div > article > div:nth-child(42) > div > ul > li:nth-child(1)")))
  famous2=rvest::html_text(rvest::html_node(x=url,css = "#main > div > div > div > article > div:nth-child(42) > div > ul > li:nth-child(2)"))
  famous3=rvest::html_text(rvest::html_node(x=url,css = "#main > div > div > div > article > div:nth-child(42) > div > ul > li:nth-child(3)"))
  famous=data.frame("Famous1"=famous1,"Famous2"=famous2,"Famous3"=famous3)
  friend=data.frame("Friend1"=friend1,"Friend2"=friend2,"Friend3"=friend3)
  foe=data.frame("Foe1"=foe1,"Foe2"=foe2)
  fff=dplyr::bind_cols(birthday,friend,foe,famous)
  fff
}
FFF(1993,11,15)
