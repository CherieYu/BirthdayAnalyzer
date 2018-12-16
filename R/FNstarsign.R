#' Starsign function
#' 
#' This is the function that get the starsign of the birthday desired from birthday analyzer web url.
#' 
#' This function asks the user to input url of the birthday analyzer page. 
#' The function will return the related starsign of the birthday of this user's input.
#' 
#' @param url from birthday analyzer
#' @return a dataframe of related starsign the birthday url input
#' @export

FNstarsign=function(url){
  url=xml2::read_html(url)
  stone=rvest::html_text(rvest::html_node(x=url,css="#main > div > div > div > article > div:nth-child(49) > div > p:nth-child(1) > a:nth-child(3)"))
  starsign=ifelse(stone=="Garnet","Capricorn",
                  ifelse(stone=="Amethyst","Aquarians",
                  ifelse(stone=="Aquamarine","Pisces",
                  ifelse(stone=="Diamond","Aries",
                  ifelse(stone=="Emerald","Taurus",
                  ifelse(stone=="Agate","Gemini",
                  ifelse(stone=="Pearl","Cancer",
                  ifelse(stone=="Ruby","Leo",
                  ifelse(stone=="Sapphire","Virgo",
                  ifelse(stone=="Opal","Libra",
                  ifelse(stone=="Topaz","Scorpio",
                  ifelse(stone=="Turquoise","Sagittarius"))))))))))))
  starsign=data.frame("Starsign" = starsign)
  starsign}


