#' Friends function
#' 
#' This is the function that get 3 starsigns that are most compatible with the birthday desired from birthday analyzer web url.
#' 
#' This function asks the user to input url of the birthday analyzer page. 
#' The function will return the related 3 starsigns that are most compatible with the birthday of this user's input.
#' 
#' @param url from birthday analyzer
#' @return a dataframe of related 3 starsigns that are most compatible with the birthday url input
#' @export


FNfriend=function(url){
  url=xml2::read_html(url)
  friend1=(rvest::html_text(rvest::html_node(x=url,css="#main > div > div > div > article > div:nth-child(6) > div > ul > ul.bullet-green > li:nth-child(1) > a")))
  friend2=rvest::html_text(rvest::html_node(x=url,css = "#main > div > div > div > article > div:nth-child(6) > div > ul > ul.bullet-green > li:nth-child(2) > a"))
  friend3=rvest::html_text(rvest::html_node(x=url,css = "#main > div > div > div > article > div:nth-child(6) > div > ul > ul.bullet-green > li:nth-child(3) > a"))
  friend=data.frame("Friend1"=friend1,"Friend2"=friend2,"Friend3"=friend3)
  friend
  }


