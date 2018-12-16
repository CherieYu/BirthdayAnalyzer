#' Create sample dataset function
#' 
#' This is the code that creates a dataset of birthday analyzer.
#' 
#' This function takes the user to input two dates, 
#' first is the begin date and second is the end date. 
#' The function will return the data on one birthday of 
#' each month between the begin date and end date.
#' @param begin A date in "yyyy-mm-dd" format
#' @param end A date in "yyyy-mm-dd" format
#' @param frequency A frequency of months or days
#' @return a saved dataset from begin to end, one birthday out of every month's starsign, guarding element, birthstone, 3 friends, 2 foes, and 3 famous people of that year.
#' @examples
#' sampledata("1990-01-01","1992-01-01","months")
#' sampledata("1990-01-01","1992-01-01","days")
#' @export


sampledata=function(begin,end,frequency){
datelist=as.data.frame(c(seq(as.Date(begin), as.Date(end), by=frequency)))
  colnames(datelist)=("Dates")
  datelist=dplyr::mutate(datelist,asnum=as.numeric(Dates))
  datelist=dplyr::mutate(datelist,corrnum = ifelse(asnum < 365, asnum+7314 ,ifelse(asnum>=365 & asnum<7670,asnum+7314+1,asnum+7314+2)))
    datelist= dplyr::mutate(datelist,date=format(Dates,"%B-%d-%Y"))
    datelist= dplyr::select(datelist,corrnum,date)
  links=c()
  for (row in 1:nrow(datelist)){
    date=datelist[row,"date"]
    number=datelist[row,"corrnum"]
    urls=paste("https://www.thehoroscope.co/birthday-analyser/",date,"-horoscope-and-zodiac-sign-meanings-",number,".html",sep = "")
    links=append(links,urls)}
  
  FNelement=function(url){
    url=xml2::read_html(url)
    element=(rvest::html_text(rvest::html_node(x=url,css="#main > div > div > div > article > div:nth-child(6) > div > ul > li:nth-child(5) > a")))
    element=substr(element,5,nchar(element))
    element=data.frame("Element"=element)}
  
  FNstone=function(url){
    url=xml2::read_html(url)
    stone=(rvest::html_text(rvest::html_node(x=url,css="#main > div > div > div > article > div:nth-child(49) > div > p:nth-child(1) > a:nth-child(3)")))
    stone=data.frame("Brithstone"=stone)}
  
  FNstarsign=function(url){
    url=xml2::read_html(url)
    stone=(rvest::html_text(rvest::html_node(x=url,css="#main > div > div > div > article > div:nth-child(49) > div > p:nth-child(1) > a:nth-child(3)")))
    starsign=ifelse(stone=="Garnet","Capricorn",ifelse(stone=="Amethyst","Aquarians",ifelse(stone=="Aquamarine","Pisces",ifelse(stone=="Diamond","Aries",ifelse(stone=="Emerald","Taurus",ifelse(stone=="Agate","Gemini",ifelse(stone=="Pearl","Cancer",ifelse(stone=="Ruby","Leo",ifelse(stone=="Sapphire","Virgo",ifelse(stone=="Opal","Libra",ifelse(stone=="Topaz","Scorpio",ifelse(stone=="Turquoise","Sagittarius"))))))))))))
    starsign=data.frame("Starsign"=starsign)}
  
  FNfriend=function(url){
    url=xml2::read_html(url)
    friend1=(rvest::html_text(rvest::html_node(x=url,css="#main > div > div > div > article > div:nth-child(6) > div > ul > ul.bullet-green > li:nth-child(1) > a")))
    friend2=rvest::html_text(rvest::html_node(x=url,css = "#main > div > div > div > article > div:nth-child(6) > div > ul > ul.bullet-green > li:nth-child(2) > a"))
    friend3=rvest::html_text(rvest::html_node(x=url,css = "#main > div > div > div > article > div:nth-child(6) > div > ul > ul.bullet-green > li:nth-child(3) > a"))
    friend=data.frame("Friend1"=friend1,"Friend2"=friend2,"Friend3"=friend3)
  }
  

  FNfoes=function(url){
    url=xml2::read_html(url)
    foe1=(rvest::html_text(rvest::html_node(x=url,css="#main > div > div > div > article > div:nth-child(6) > div > ul > ul.bullet-red > li:nth-child(1) > a")))
    foe2=rvest::html_text(rvest::html_node(x=url,css = "#main > div > div > div > article > div:nth-child(6) > div > ul > ul.bullet-red > li:nth-child(2) > a"))
    foe=data.frame("Foe1"=foe1,"Foe2"=foe2)
  }
  
  FNfamous=function(url){
    url=xml2::read_html(url)
    famous1=(rvest::html_text(rvest::html_node(x=url,css="#main > div > div > div > article > div:nth-child(42) > div > ul > li:nth-child(1)")))
    famous2=rvest::html_text(rvest::html_node(x=url,css = "#main > div > div > div > article > div:nth-child(42) > div > ul > li:nth-child(2)"))
    famous3=rvest::html_text(rvest::html_node(x=url,css = "#main > div > div > div > article > div:nth-child(42) > div > ul > li:nth-child(3)"))
    famous=data.frame("Famous1"=famous1,"Famous2"=famous2,"Famous3"=famous3)
  }
  
  Elementresult=purrr::map(links,FNelement)
  Friendresult=purrr::map(links,FNfriend)
  Famousresult=purrr::map(links,FNfamous)
  Foesresult=purrr::map(links,FNfoes)
  Starsignresult=purrr::map(links,FNstarsign)
  Stoneresult=purrr::map(links,FNstone)
  
  elemdata=dplyr::bind_rows(Elementresult)
  fridata=dplyr::bind_rows(Friendresult)
  famodata=dplyr::bind_rows(Famousresult)
  foedata=dplyr::bind_rows(Foesresult)
  Starsigndata=dplyr::bind_rows(Starsignresult)
  Stonedata=dplyr::bind_rows(Stoneresult)
  
  all=dplyr::bind_cols(datelist,Starsigndata,Stonedata,elemdata,fridata,foedata,famodata)
  }
sampledata("1990-01-01","1992-01-01","months")
