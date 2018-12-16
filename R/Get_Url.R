#' Get_Url function
#' 
#' This is the function that get the webstie of the birthday desired for birthday analyzer.
#' 
#' This function asks the user to input birth year, month and day. 
#' The function will return the website link on one birthday of 
#' this user's input.
#' 
#' @param Year A number in "yyyy" format
#' @param Month A number in "mm" format
#' @param Day A number in "dd" format
#' @return website url of the birthday inout
#' @examples
#' Get_Url(1993,11,15)
#' Get_Url(1962,09,14)
#' @export

Get_Url=function(Year=1993,Month=11,Day=15){
  clientinput=paste(Year,Month,Day,sep = "-")
  corresnum=as.numeric(as.Date(clientinput))
  corresnum=ifelse(corresnum <365, corresnum+7314 ,ifelse(corresnum>=365 & corresnum<7670,corresnum+7314+1,corresnum+7314+2))
  paste("https://www.thehoroscope.co/birthday-analyser/",clientinput,"-horoscope-and-zodiac-sign-meanings-",corresnum,".html",sep = "")
  }
Get_Url(1993,11,15)

