readBbcLiveText <- function(bbc_url) {
  
  remDr <- RSelenium::remoteDriver(
    remoteServerAddr = "localhost",
    port = 4444L,
    browserName = "chrome"
  )
  remDr$open()
  
  remDr$navigate(bbc_url)
  
  webElem <- 
    remDr$findElement(
      using = "link text", 
      "Live Text")
  webElem$clickElement()
  
  showMoreCSS <- "#tab-2 > div > div > div.lx-stream.gs-u-box-size.gs-t-sport.lx-lts--robo-text > div.lx-stream__show-more.lx-stream-show-more > button"
  
  #first click of show more
  webElem <- 
    remDr$findElement(
      using = "css selector", 
      showMoreCSS
    )
  webElem$clickElement()
  
  # read the live stream text thats there 
  webElems <- 
    remDr$findElements(
      using = "class name", 
      "lx-stream-post__body"
    )
  resHeaders <- unlist(lapply(webElems, function(x) {x$getElementText()}))
  
  #while the live stream text doesnt include the first row keep showing more
  while (is.na(match("First Half begins.", resHeaders))) {
    
    #click show more
    webElem <- 
      remDr$findElement(
        using = "css selector", 
        showMoreCSS
      )
    webElem$clickElement()
    #sleep to stop suspicsion
    Sys.sleep(5)
    
    #read live text
    webElems <- 
      remDr$findElements(
        using = "class name", 
        "lx-stream-post__body"
      )
    resHeaders <- unlist(lapply(webElems, function(x) {x$getElementText()}))
    
  }
  
  teamElems <- 
    remDr$findElements(
      using = "tag name", 
      "abbr"
    ) 

  teams <- unlist(lapply(teamElems, function(x) {x$getElementAttribute("title")}))
  home_team <- teams[1]
  away_team <- teams[2]
  
  dateElem <- 
    remDr$findElement(
      using = "tag name", 
      "time"
    ) 
  match_date <- 
    unlist(
      dateElem$getElementAttribute("datetime")
    )
  
  remDr$close()
  
  data.frame(
    live_text = rev(resHeaders),
    home_team = home_team,
    away_team = away_team,
    match_date = match_date,
    link = bbc_url,
    stringsAsFactors = FALSE
  )
}