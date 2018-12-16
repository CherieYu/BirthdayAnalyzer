#' Element function
#' 
#' This is the function that get the element of the birthday desired from birthday analyzer web url.
#' 
#' This function asks the user to input url of the birthday analyzer page. 
#' The function will return the related element of the birthday of this user's input.
#' 
#' @param url from birthday analyzer
#' @return a dataframe of related element the birthday url input
#' @export


FNelement=function(url){
  url=xml2::read_html(url)
  element=(rvest::html_text(rvest::html_node(x=url,css="#main > div > div > div > article > div:nth-child(6) > div > ul > li:nth-child(5) > a")))
  element=substr(element,5,nchar(element))
  element=data.frame("Element"=element)
  element}



