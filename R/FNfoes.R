#' Foes function
#' 
#' This is the function that get 2 starsigns that are not compatible with the birthday desired from birthday analyzer web url.
#' 
#' This function asks the user to input url of the birthday analyzer page. 
#' The function will return the related 2 starsigns that are not compatible with the birthday of this user's input.
#' 
#' @param url from birthday analyzer
#' @return a dataframe of related 2 starsigns that are not compatible with the birthday url input
#' @export


FNfoes=function(url){
  url=xml2::read_html(url)
  foe1=(rvest::html_text(rvest::html_node(x=url,css="#main > div > div > div > article > div:nth-child(6) > div > ul > ul.bullet-red > li:nth-child(1) > a")))
  foe2=rvest::html_text(rvest::html_node(x=url,css = "#main > div > div > div > article > div:nth-child(6) > div > ul > ul.bullet-red > li:nth-child(2) > a"))
  foe=data.frame("Foe1"=foe1,"Foe2"=foe2)
  foe
}

