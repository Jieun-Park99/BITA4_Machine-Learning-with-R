library(RSelenium)
library(dplyr)
library(rvest)

remDr = remoteDriver(remoteServerAddr = "localhost" ,
                     port = 4445L,
                     browserName = "chrome")  
remDr$open() #창 띄우기

remDr$navigate("https://www.youtube.com") # 유튜브 사이트 접속

searchButton = remDr$findElement(using='css selector',
                                      value='#search') #검색창 버튼 만들기

searchButton$sendKeysToElement(list(key='shift', key='home',
                                        key='delete')) #검색창 지우기

searchButton$sendKeysToElement(list('펭수')) #검색창에 '펭수'입력하기

clickbutton = remDr$findElement(using='css selector',
                                value='#search-icon-legacy') #클릭버튼만들기 
clickbutton$clickElement(); Sys.sleep(2) #클릭하여 검색하기 

clickbutton = remDr$findElement(using='css selector',
                                value='#channel-title') #클릭버튼 만들기

clickbutton$clickElement(); Sys.sleep(2) #클릭하여 페이지 이동하기 

clickbutton = remDr$findElement(using='css selector',
                                value='#tabsContent > paper-tab:nth-child(4) > div') #클릭버튼 만들기 

clickbutton$clickElement() ; Sys.sleep(2) # 클릭하여 페이지 이동하기 

clickbutton = remDr$findElement(using='css selector',
                                value='#items > ytd-grid-video-renderer:nth-child(1)') #클릭버튼 만들기

clickbutton$clickElement() ; Sys.sleep(2) #영상 클릭하여 이동하기

url = "https://www.youtube.com/watch?v=v7XpDzgUQbg"
remDr$getPageSource()[[1]]
#html 읽어오기
html = read_html(remDr$getPageSource()[[1]]);Sys.sleep(1)
#htxt = read_html(url, encoding="UTF-8") #한글 깨지는거 방지 

#node 읽기
content = html_nodes(html, 'yt-formatted-string#text.style-scope.ytd-toggle-button-renderer.style-text')

#text 읽기
text = html_text(content) ; text
text[c(1,2)]

good = c()
bad = c()
for (i in 1:5){
  value_base='#items > ytd-grid-video-renderer:nth-child('
  clickbutton = remDr$findElement(using='css selector',
                                  value=paste(value_base,i,')')) #클릭버튼 만들기
  clickbutton$clickElement() ; Sys.sleep(2) #영상 클릭하여 이동하기
  remDr$getPageSource()[[1]]
  
  #html 읽어오기
  html = read_html(remDr$getPageSource()[[1]]);Sys.sleep(1)
  
  #node 읽기
  content = html_nodes(html, 'yt-formatted-string#text.style-scope.ytd-toggle-button-renderer.style-text')
  
  #text 읽기
  text = html_text(content)
  print(text)
  good = c(good,text[1])
  bad = c(bad,text[2])
  remDr$goBack()
}
cbind(good,bad)
