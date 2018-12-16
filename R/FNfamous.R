#' Famous function
#' 
#' This is the function that get 3 famous people born on the same year of the birthday desired from birthday analyzer web url.
#' 
#' This function asks the user to input url of the birthday analyzer page. 
#' The function will return the related 3 famous people born on the same year of the birthday of this user's input.
#' 
#' @param url from birthday analyzer
#' @return a dataframe of related 3 famous people born on the same year of the birthday url input
#' @export


FNfamous=function(url){
  url=xml2::read_html(url)
  famous1=(rvest::html_text(rvest::html_node(x=url,css="#main > div > div > div > article > div:nth-child(42) > div > ul > li:nth-child(1)")))
  famous2=rvest::html_text(rvest::html_node(x=url,css = "#main > div > div > div > article > div:nth-child(42) > div > ul > li:nth-child(2)"))
  famous3=rvest::html_text(rvest::html_node(x=url,css = "#main > div > div > div > article > div:nth-child(42) > div > ul > li:nth-child(3)"))
  famous=data.frame("Famous1"=famous1,"Famous2"=famous2,"Famous3"=famous3)
  famous
}
