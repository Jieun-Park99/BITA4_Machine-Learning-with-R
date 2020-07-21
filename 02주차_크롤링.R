install.packages('RSelenium')
library(RSelenium)
remDr = remoteDriver(remoteServerAddr = "localhost" ,
                      
                      port = 4445L,   # port 번호 입력
                      
                      browserName = "chrome")  

# browserName : 실행 브라우저 입력

remDr$open()

# 브라우저가 실행되면 성공

# 접속할 사이트 입력

remDr$navigate("https://www.google.com")     # google로 연결 됨

library(dplyr)
library(rvest)


url_base="https://movie.naver.com/movie/bi/mi/pointWriteFormList.nhn?code=70254&type=after&isActualPointWriteExecute=false&isMileageSubscriptionAlready=false&isMileageSubscriptionReject=false&page="
paste(url_base,1,sep='')

#주소설정
url = paste(url_base,1,sep='')

#html 읽어오기
htxt = read_html(url, encoding="UTF-8") #한글 깨지는거 방지 

#node 읽기
table = html_nodes(htxt,".score_result")
content = html_nodes(table, ".score_reple")
content2 = html_nodes(content, paste("#_filtered_ment_",1,sep='')) 
#여기서 #을 한 이유는 id는 #으로 표현해야하기 때문!

#text 읽기
reviews = html_text(content2) ; reviews

all.reviews = c()
for(page in 1:10){
  for(num in 1:9){
    url<-paste(url_base,page,sep='') 
    htxt<-read_html(url,encoding="UTF-8") 
    table<-html_nodes(htxt,".score_result") 
    content<-html_nodes(table,".score_reple") 
    content2<-html_nodes(content,paste("#_filtered_ment_",num,sep='')) 
    reviews<-html_text(content2) 
    if(length(reviews)==0){break} 
    all.reviews<-c(all.reviews,reviews) 
    print(page) 
  }
}
head(all.reviews)

data = gsub("[[:cntrl:]]", "", all.reviews)
head(data)

############## 실습 ######################
all.stars = c()
for(page in 1:10){
  for(num in 1:9){
    url<-paste(url_base,page,sep='') 
    htxt<-read_html(url,encoding="UTF-8") 
    table<-html_nodes(htxt,".input_netizen") 
    content<-html_nodes(table,".score_result") 
    content2<-html_nodes(content, ".star_score")
    content3<-html_nodes(content2,"em") 
    scores<-html_text(content3) 
    if(length(reviews)==0){break} 
    all.stars<-c(all.stars,scores) 
    print(page) 
  }
}
head(all.stars)

as.numeric(all.stars)
hist(as.numeric(all.stars))

############## dplyr #####################
all_score2 = list() # append 대신 list 형식을 활용
for(page in 1:10){
  all_score2[[page]]= read_html(paste(url_base, page, sep=""), encoding="UTF-8") %>%
    html_nodes(".star_score") %>% 
    html_nodes("em") %>% 
    html_text()
}
all_score2

############## Selenium ##################

remDr = remoteDriver(port = 4445L, browserName="chrome")
remDr$open()
remDr$navigate("http://www.naver.com")

blogButton = remDr$findElement(using="xpath",
                               value='//*[@id="PM_ID_ct"]/div[1]/div[2]/div[1]/ul[1]/li[3]/a/span[1]')
blogButton$clickElement() ; Sys.sleep(2)

# 요소를 찾게 되면 그 요소를 자주 쓰게 됨!!

#그럼 이제 검색창에 접근하자! (이번에는 css selector)
#webElementButton = remDr$findElement(using="css selector",
#                                     value = '#PM_ID_ct > div.header > div.section_navbar > div.area_navigation > ul:nth-child(1) > li:nth-child(3) > a > span.an_icon')

#webElementButton$clickElement() ; Sys.sleep(2)

######################## 날짜 지정해서 검색하기 #####################
webElementButton <- remDr$findElement(using='css selector',
                              value='#header > div.header_common > div > div.area_search > form > fieldset > div > input')

webElementButton$sendKeysToElement(list(key='shift', key='home',
                                        key='delete'))

webElementButton$sendKeysToElement(list('박지은'))

clickbutton = remDr$findElement(using='css selector',
                                value='#header > div.header_common > div > div.area_search > form > fieldset > a.button.button_blog > i')
clickbutton$clickElement(); Sys.sleep(2)

clickbutton = remDr$findElement(using='css selector',
                                value='#content > section > div.category_search > div.search_information > div > div > a > i')

clickbutton$clickElement();Sys.sleep(2)

clickbutton = remDr$findElement(using='css selector',
                                value='#content > section > div.category_search > div.search_information > div > div > div > a:nth-child(2)')

clickbutton$clickElement(); Sys.sleep(2)

clickbutton = remDr$findElement(using='css selector',
                                value='#search_start_date')
clickbutton$clickElement(); Sys.sleep(2)

clickbutton = remDr$findElement(using='css selector',
                                value='#search_start_date')
clickbutton$sendKeysToElement(list(key='shift', key='home',
                                        key='delete'))
clickbutton$sendKeysToElement(list('2020-02-01'))

clickbutton = remDr$findElement(using='css selector',
                                value='#periodSearch')

clickbutton$clickElement(); Sys.sleep(2)

####################### 이제 검색했으니 크롤링!! #######################
html = read_html(remDr$getPageSource()[[1]]); Sys.sleep(1)
content = html_nodes(html, ".search_number")
num = html_text(content)
num


###################################
what = read.csv('example.csv', header=F)

start_date = '2020-02-01'
end_date = '2020-02-22'

############## 건수가 아닌 블로그 제목을 갖고오고 싶다면? ###########




