#' Stone function
#' 
#' This is the function that get the birthstone of the birthday desired from birthday analyzer web url.
#' 
#' This function asks the user to input url of the birthday analyzer page. 
#' The function will return the related birthstone of the birthday of this user's input.
#' 
#' @param url from birthday analyzer
#' @return a dataframe of related birthstone the birthday url input
#' @export


FNstone=function(url){
  url=xml2::read_html(url)
  stone=(rvest::html_text(rvest::html_node(x=url,css="#main > div > div > div > article > div:nth-child(49) > div > p:nth-child(1) > a:nth-child(3)")))
  stone=data.frame("Brithstone"=stone)
  stone}

