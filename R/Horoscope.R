#' Horoscope function
#' 
#' This is the function that get the starsign, element, and birthstone
#' of the birthday desired from birthday analyzer.
#' 
#' This function asks the user to input birth year, month and day. 
#' The function will return the related starsign, element, and 
#' birthstone on rhe birthday of this user's input.
#' 
#' @param Year A number in "yyyy" format
#' @param Month A number in "mm" format
#' @param Day A number in "dd" format
#' @return a dataframe of related starsign, element birthstone of the birthday input
#' @examples
#' Horoscope(1993,11,15)
#' Horoscope(1962,09,14)
#' @export

Horoscope=function(Year,Month,Day){
  clientinput=paste(Year,Month,Day,sep = "-")
  birthday=as.data.frame(clientinput)
  colnames(birthday)=("Birthday")
  corresnum=as.numeric(as.Date(clientinput))
  corresnum=ifelse(corresnum <365, corresnum+7314 ,ifelse(corresnum>=365 & corresnum<7670,corresnum+7314+1,corresnum+7314+2))
  url=paste("https://www.thehoroscope.co/birthday-analyser/",clientinput,"-horoscope-and-zodiac-sign-meanings-",corresnum,".html",sep = "")
  url=xml2::read_html(url)
  element=(rvest::html_text(rvest::html_node(x=url,css="#main > div > div > div > article > div:nth-child(6) > div > ul > li:nth-child(5) > a")))
  element=substr(element,5,nchar(element))
  stone=(rvest::html_text(rvest::html_node(x=url,css="#main > div > div > div > article > div:nth-child(49) > div > p:nth-child(1) > a:nth-child(3)")))
  starsign=ifelse(stone=="Garnet","Capricorn",ifelse(stone=="Amethyst","Aquarians",ifelse(stone=="Aquamarine","Pisces",ifelse(stone=="Diamond","Aries",ifelse(stone=="Emerald","Taurus",ifelse(stone=="Agate","Gemini",ifelse(stone=="Pearl","Cancer",ifelse(stone=="Ruby","Leo",ifelse(stone=="Sapphire","Virgo",ifelse(stone=="Opal","Libra",ifelse(stone=="Topaz","Scorpio",ifelse(stone=="Turquoise","Sagittarius"))))))))))))
  element=data.frame("Element"=element)
  stone=data.frame("Brithstone"=stone)
  starsign=data.frame("Starsign"=starsign)
  horoscope=dplyr::bind_cols(birthday,starsign,stone,element)
  horoscope
}
Horoscope(1993,11,15)
