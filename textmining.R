library(rvest)

# 네이버 블로그에 '스타벅스 이벤트' 친 후 게시글 순서 변경할 때마다 뒤에 숫자가 11, 21, 31로 바뀜

target_url <- 'https://search.naver.com/search.naver?date_from=&date_option=0&date_to=&dup_remove=1&nso=&post_blogurl=&post_blogurl_without=&query=%EC%8A%A4%ED%83%80%EB%B2%85%EC%8A%A4%20%EC%9D%B4%EB%B2%A4%ED%8A%B8&sm=tab_pge&srchby=all&st=sim&where=post&start='

urls <- NULL
for(i in 0:99){
  urls[i+1] <- paste0(target_url, i*10+1)
}
length(urls) # 100

html_starbucks1 <- read_html(urls[1])
html_starbucks1 %>% html_nodes("a.sh_blog_title._sp_each_url._sp_each_title") %>% html_text()


text_starbucks2 = rep(NA, 1000)
for(i in 0:99){
  html_starbucks = read_html(urls[i+1])
  text_starbucks2[(1+10*i):(10+10*i)] = html_starbucks %>% html_nodes("a.sh_blog_title._sp_each_url._sp_each_title") %>% html_text()
}

text_starbucks <- gsub(
  pattern = '스타벅스', replacement = '',
  x = text_starbucks2)
text_starbucks <- gsub(
  pattern = '이벤트', replacement = '',
  x = text_starbucks)
text_starbucks <- gsub(
  pattern = '음료', replacement = '',
  x = text_starbucks)
text_starbucks <- gsub(
  pattern = '커피', replacement = '',
  x = text_starbucks)
text_starbucks <- gsub(
  pattern = '카페', replacement = '',
  x = text_starbucks)
text_starbucks <- gsub(
  pattern = '프리퀀시', replacement = '',
  x = text_starbucks)

library(wordcloud)
library(RColorBrewer)
library(KoNLP)

useSejongDic()

noun_starbucks = sapply(text_starbucks, extractNoun, USE.NAMES = F) # 명사 추출

noun_starbucks_unlist = unlist(noun_starbucks)
#n_starbucks_unlist = unlist(n_starbucks)

# 단어 추가
add_dic <- readLines("C:/bg/Rcafe/adddic.txt")

for(i in 1:length(add_dic)){
  mergeUserDic(data.frame(add_dic[i], "ncn"))
}

# Extra Noun부터 다시 반복
noun_starbucks = sapply(text_starbucks, extractNoun, USE.NAMES = F)
noun_starbucks_unlist = unlist(noun_starbucks)

# 두 글자 이상인 것만 추출: 이것만으로도 의존명사 많이 거를 수 있음

noun_starbucks_unlist <- Filter(function(x){
  nchar(x)>=2}, noun_starbucks_unlist)

opo_starbucks = length(grep("1\\+1", noun_starbucks_unlist)) # 1+1은 스타벅스에 16개

# 불용어처리

# 1) 특수문자 삭제
noun_starbucks_unlist <- gsub('[~!@#$%&*()_+=?<>]','', noun_starbucks_unlist)
noun_starbucks_unlist <- gsub("\\[", "", noun_starbucks_unlist)
noun_starbucks_unlist <- gsub("\\/", "", noun_starbucks_unlist)
noun_starbucks_unlist <- gsub("\\]", "", noun_starbucks_unlist)
noun_starbucks_unlist <- gsub("★","", noun_starbucks_unlist)
noun_starbucks_unlist <- gsub("♡","", noun_starbucks_unlist)
noun_starbucks_unlist <- gsub("^","", noun_starbucks_unlist)
noun_starbucks_unlist <- gsub("-","", noun_starbucks_unlist)
noun_starbucks_unlist <- gsub('[ㄱ-ㅎ]','',noun_starbucks_unlist)
noun_starbucks_unlist<- gsub('(ㅜ|ㅠ|ㅡ)','',noun_starbucks_unlist)
noun_starbucks_unlist <- gsub("\\d+","",noun_starbucks_unlist) # 숫자

# 다시 필터 반복(2글자 이상만 추출)
noun_starbucks_unlist <- Filter(function(x){
  nchar(x)>=2}, noun_starbucks_unlist)

# 1+1 16개 추가해주기(불용어 처리로 사라졌으므로)
noun_starbucks_unlist <- c(noun_starbucks_unlist, rep("1+1", opo_starbucks))

# 순서대로 정리
count_starbucks = table(noun_starbucks_unlist)
top_starbucks = head(sort(count_starbucks, decreasing = T), 100)

# Word Cloud
color <- brewer.pal(12, "Set3")

library(plyr)
library(extrafont)

windowsFonts(malgun=windowsFont("맑은 고딕"))
windowsFonts(bedal=windowsFont("배달의민족 한나체 Pro"))
windowsFonts(godic=windowsFont("한컴 고딕 굵게"))

wordcloud(names(count_starbucks), freq=count_starbucks, scale=c(3,.7), min.freq=5,
          max.words=Inf, random.order=F, rot.per=.1, colors=color6, family="bedal")


##################### Twosome ###############

target_url<-"https://search.naver.com/search.naver?date_from=&date_option=0&date_to=&dup_remove=1&nso=&post_blogurl=&post_blogurl_without=&query=%ED%88%AC%EC%8D%B8%ED%94%8C%EB%A0%88%EC%9D%B4%EC%8A%A4%20%EC%9D%B4%EB%B2%A4%ED%8A%B8&sm=tab_pge&srchby=all&st=sim&where=post&start="

urls <- NULL
for(i in 0:99){
  urls[i+1] <- paste0(target_url, i*10+1)
}

text_twosome2 = rep(NA, 1000)
for(i in 0:99){
  html_twosome = read_html(urls[i+1])
  text_twosome2[(1+10*i):(10+10*i)] = html_twosome %>% html_nodes("a.sh_blog_title._sp_each_url._sp_each_title") %>% html_text()
}

text_twosome <- gsub(
  pattern = '투썸플레이스', replacement = '',
  x = text_twosome2)
text_twosome <- gsub(
  pattern = '투썸', replacement = '',
  x = text_twosome)
text_twosome <- gsub(
  pattern = '플레이스', replacement = '',
  x = text_twosome)
text_twosome <- gsub(
  pattern = '이벤트', replacement = '',
  x = text_twosome)
text_twosome <- gsub(
  pattern = '음료', replacement = '',
  x = text_twosome)
text_twosome <- gsub(
  pattern = '커피', replacement = '',
  x = text_twosome)
text_twosome <- gsub(
  pattern = '카페', replacement = '',
  x = text_twosome)

useSejongDic()

add_dic <- readLines("C:/bg/Rcafe/add_twosome.txt")

for(i in 1:length(add_dic)){
  mergeUserDic(data.frame(add_dic[i], "ncn"))
}

noun_twosome = sapply(text_twosome, extractNoun, USE.NAMES = F)
noun_twosome_unlist = unlist(noun_twosome)

bt = length(grep("BT21", noun_twosome_unlist))
st11 = length(grep("11번가", noun_twosome_unlist))
opo = length(grep("1\\+1", noun_twosome_unlist))

# text_twosome 불용어처리

text_twosome <- gsub('[~!@#$%&*()_+=?<>]','', text_twosome)
text_twosome <- gsub("\\[", "", text_twosome)
text_twosome <- gsub("\\/", "", text_twosome)
text_twosome <- gsub("\\]", "", text_twosome)
text_twosome <- gsub("★","", text_twosome)
text_twosome <- gsub("♡","", text_twosome)
text_twosome <- gsub("^","", text_twosome)
text_twosome <- gsub("-","", text_twosome)
text_twosome <- gsub('[ㄱ-ㅎ]','',text_twosome)
text_twosome<- gsub('(ㅜ|ㅠ|ㅡ)','',text_twosome)
text_twosome <- gsub("\\d+","",text_twosome)

noun_twosome = sapply(text_twosome, extractNoun, USE.NAMES = F)
noun_twosome_unlist = unlist(noun_twosome)

# 두 글자 이상인 것만 추출

noun_twosome_unlist <- Filter(function(x){
  nchar(x)>=2}, noun_twosome_unlist)

noun_twosome_unlist <- c(noun_twosome_unlist, rep("BT21", bt))
noun_twosome_unlist <- c(noun_twosome_unlist, rep("11번가", st11))
noun_twosome_unlist <- c(noun_twosome_unlist, rep("1+1", opo))

count_twosome = table(noun_twosome_unlist)
top_twosome = head(sort(count_twosome, decreasing = T), 100)

wordcloud(names(count_twosome),freq=count_twosome, scale=c(5,.8), min.freq=5, max.words=Inf, random.order=F, rot.per=.1, colors=color6, family="bedal")


##################### ediya ######################

target_url <- 'https://search.naver.com/search.naver?date_from=&date_option=0&date_to=&dup_remove=1&nso=&post_blogurl=&post_blogurl_without=&query=%EC%9D%B4%EB%94%94%EC%95%BC%20%EC%9D%B4%EB%B2%A4%ED%8A%B8&sm=tab_pge&srchby=all&st=sim&where=post&start='

urls <- NULL
for(i in 0:99){
  urls[i+1] <- paste0(target_url, i*10+1)
}

html_ediya1 <- read_html(urls[1])
html_ediya1 %>% html_nodes("a.sh_blog_title._sp_each_url._sp_each_title") %>% html_text()


text_ediya2 = rep(NA, 1000)
for(i in 0:99){
  html_ediya = read_html(urls[i+1])
  text_ediya2[(1+10*i):(10+10*i)] = html_ediya %>% html_nodes("a.sh_blog_title._sp_each_url._sp_each_title") %>% html_text()
}

text_ediya <- gsub(
  pattern = '이디야', replacement = '',
  x = text_ediya2)
text_ediya <- gsub(
  pattern = '이벤트', replacement = '',
  x = text_ediya)
text_ediya <- gsub(
  pattern = '음료', replacement = '',
  x = text_ediya)
text_ediya <- gsub(
  pattern = '커피', replacement = '',
  x = text_ediya)
text_ediya <- gsub(
  pattern = '카페', replacement = '',
  x = text_ediya)

useSejongDic()

noun_ediya = sapply(text_ediya, extractNoun, USE.NAMES = F) # 명사 추출
n_ediya = noun_ediya

noun_ediya_unlist = unlist(noun_ediya)
n_ediya_unlist = unlist(n_ediya)

ann = length(grep("10주년", noun_ediya_unlist))

add_dic <- readLines("C:/bg/Rcafe/add_ediya.txt", encoding='UTF-8')

for(i in 1:length(add_dic)){
  mergeUserDic(data.frame(add_dic[i], "ncn"))
}

text_ediya <- gsub('[~!@#$%&*()_+=?<>]','', text_ediya)
text_ediya <- gsub("\\[", "", text_ediya)
text_ediya <- gsub("\\/", "", text_ediya)
text_ediya <- gsub("\\]", "", text_ediya)
text_ediya <- gsub("★","", text_ediya)
text_ediya <- gsub("♡","", text_ediya)
text_ediya <- gsub("^","", text_ediya)
text_ediya <- gsub("-","", text_ediya)
text_ediya <- gsub('[ㄱ-ㅎ]','',text_ediya)
text_ediya<- gsub('(ㅜ|ㅠ|ㅡ)','',text_ediya)
text_ediya <- gsub("\\d+","",text_ediya)

# Extra Noun부터 다시 반복
noun_ediya = sapply(text_ediya, extractNoun, USE.NAMES = F)
noun_ediya_unlist = unlist(noun_ediya)

# 두 글자 이상인 것만 추출

noun_ediya_unlist <- Filter(function(x){
  nchar(x)>=2}, noun_ediya_unlist)

noun_ediya_unlist <- c(noun_ediya_unlist, rep("10주년", ann))

count_ediya = table(noun_ediya_unlist)
top_ediya = head(sort(count_ediya, decreasing = T), 100)

grep('', top_ediya)

# Word Cloud
wordcloud(names(count_ediya),freq=count_ediya, scale=c(5,.8),min.freq=5,
          max.words=Inf, random.order=F, rot.per=.1, colors=color6, family="bedal")
