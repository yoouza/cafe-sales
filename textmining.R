library(rvest)

# ���̹� ���α׿� '��Ÿ���� �̺�Ʈ' ģ �� �Խñ� ���� ������ ������ �ڿ� ���ڰ� 11, 21, 31�� �ٲ�

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
  pattern = '��Ÿ����', replacement = '',
  x = text_starbucks2)
text_starbucks <- gsub(
  pattern = '�̺�Ʈ', replacement = '',
  x = text_starbucks)
text_starbucks <- gsub(
  pattern = '����', replacement = '',
  x = text_starbucks)
text_starbucks <- gsub(
  pattern = 'Ŀ��', replacement = '',
  x = text_starbucks)
text_starbucks <- gsub(
  pattern = 'ī��', replacement = '',
  x = text_starbucks)
text_starbucks <- gsub(
  pattern = '��������', replacement = '',
  x = text_starbucks)

library(wordcloud)
library(RColorBrewer)
library(KoNLP)

useSejongDic()

noun_starbucks = sapply(text_starbucks, extractNoun, USE.NAMES = F) # ���� ����

noun_starbucks_unlist = unlist(noun_starbucks)
#n_starbucks_unlist = unlist(n_starbucks)

# �ܾ� �߰�
add_dic <- readLines("C:/bg/Rcafe/adddic.txt")

for(i in 1:length(add_dic)){
  mergeUserDic(data.frame(add_dic[i], "ncn"))
}

# Extra Noun���� �ٽ� �ݺ�
noun_starbucks = sapply(text_starbucks, extractNoun, USE.NAMES = F)
noun_starbucks_unlist = unlist(noun_starbucks)

# �� ���� �̻��� �͸� ����: �̰͸����ε� �������� ���� �Ÿ� �� ����

noun_starbucks_unlist <- Filter(function(x){
  nchar(x)>=2}, noun_starbucks_unlist)

opo_starbucks = length(grep("1\\+1", noun_starbucks_unlist)) # 1+1�� ��Ÿ������ 16��

# �ҿ��ó��

# 1) Ư������ ����
noun_starbucks_unlist <- gsub('[~!@#$%&*()_+=?<>]','', noun_starbucks_unlist)
noun_starbucks_unlist <- gsub("\\[", "", noun_starbucks_unlist)
noun_starbucks_unlist <- gsub("\\/", "", noun_starbucks_unlist)
noun_starbucks_unlist <- gsub("\\]", "", noun_starbucks_unlist)
noun_starbucks_unlist <- gsub("��","", noun_starbucks_unlist)
noun_starbucks_unlist <- gsub("��","", noun_starbucks_unlist)
noun_starbucks_unlist <- gsub("^","", noun_starbucks_unlist)
noun_starbucks_unlist <- gsub("-","", noun_starbucks_unlist)
noun_starbucks_unlist <- gsub('[��-��]','',noun_starbucks_unlist)
noun_starbucks_unlist<- gsub('(��|��|��)','',noun_starbucks_unlist)
noun_starbucks_unlist <- gsub("\\d+","",noun_starbucks_unlist) # ����

# �ٽ� ���� �ݺ�(2���� �̻� ����)
noun_starbucks_unlist <- Filter(function(x){
  nchar(x)>=2}, noun_starbucks_unlist)

# 1+1 16�� �߰����ֱ�(�ҿ�� ó���� ��������Ƿ�)
noun_starbucks_unlist <- c(noun_starbucks_unlist, rep("1+1", opo_starbucks))

# ������� ����
count_starbucks = table(noun_starbucks_unlist)
top_starbucks = head(sort(count_starbucks, decreasing = T), 100)

# Word Cloud
color <- brewer.pal(12, "Set3")

library(plyr)
library(extrafont)

windowsFonts(malgun=windowsFont("���� ����"))
windowsFonts(bedal=windowsFont("����ǹ��� �ѳ�ü Pro"))
windowsFonts(godic=windowsFont("���� ���� ����"))

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
  pattern = '�����÷��̽�', replacement = '',
  x = text_twosome2)
text_twosome <- gsub(
  pattern = '����', replacement = '',
  x = text_twosome)
text_twosome <- gsub(
  pattern = '�÷��̽�', replacement = '',
  x = text_twosome)
text_twosome <- gsub(
  pattern = '�̺�Ʈ', replacement = '',
  x = text_twosome)
text_twosome <- gsub(
  pattern = '����', replacement = '',
  x = text_twosome)
text_twosome <- gsub(
  pattern = 'Ŀ��', replacement = '',
  x = text_twosome)
text_twosome <- gsub(
  pattern = 'ī��', replacement = '',
  x = text_twosome)

useSejongDic()

add_dic <- readLines("C:/bg/Rcafe/add_twosome.txt")

for(i in 1:length(add_dic)){
  mergeUserDic(data.frame(add_dic[i], "ncn"))
}

noun_twosome = sapply(text_twosome, extractNoun, USE.NAMES = F)
noun_twosome_unlist = unlist(noun_twosome)

bt = length(grep("BT21", noun_twosome_unlist))
st11 = length(grep("11����", noun_twosome_unlist))
opo = length(grep("1\\+1", noun_twosome_unlist))

# text_twosome �ҿ��ó��

text_twosome <- gsub('[~!@#$%&*()_+=?<>]','', text_twosome)
text_twosome <- gsub("\\[", "", text_twosome)
text_twosome <- gsub("\\/", "", text_twosome)
text_twosome <- gsub("\\]", "", text_twosome)
text_twosome <- gsub("��","", text_twosome)
text_twosome <- gsub("��","", text_twosome)
text_twosome <- gsub("^","", text_twosome)
text_twosome <- gsub("-","", text_twosome)
text_twosome <- gsub('[��-��]','',text_twosome)
text_twosome<- gsub('(��|��|��)','',text_twosome)
text_twosome <- gsub("\\d+","",text_twosome)

noun_twosome = sapply(text_twosome, extractNoun, USE.NAMES = F)
noun_twosome_unlist = unlist(noun_twosome)

# �� ���� �̻��� �͸� ����

noun_twosome_unlist <- Filter(function(x){
  nchar(x)>=2}, noun_twosome_unlist)

noun_twosome_unlist <- c(noun_twosome_unlist, rep("BT21", bt))
noun_twosome_unlist <- c(noun_twosome_unlist, rep("11����", st11))
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
  pattern = '�̵��', replacement = '',
  x = text_ediya2)
text_ediya <- gsub(
  pattern = '�̺�Ʈ', replacement = '',
  x = text_ediya)
text_ediya <- gsub(
  pattern = '����', replacement = '',
  x = text_ediya)
text_ediya <- gsub(
  pattern = 'Ŀ��', replacement = '',
  x = text_ediya)
text_ediya <- gsub(
  pattern = 'ī��', replacement = '',
  x = text_ediya)

useSejongDic()

noun_ediya = sapply(text_ediya, extractNoun, USE.NAMES = F) # ���� ����
n_ediya = noun_ediya

noun_ediya_unlist = unlist(noun_ediya)
n_ediya_unlist = unlist(n_ediya)

ann = length(grep("10�ֳ�", noun_ediya_unlist))

add_dic <- readLines("C:/bg/Rcafe/add_ediya.txt", encoding='UTF-8')

for(i in 1:length(add_dic)){
  mergeUserDic(data.frame(add_dic[i], "ncn"))
}

text_ediya <- gsub('[~!@#$%&*()_+=?<>]','', text_ediya)
text_ediya <- gsub("\\[", "", text_ediya)
text_ediya <- gsub("\\/", "", text_ediya)
text_ediya <- gsub("\\]", "", text_ediya)
text_ediya <- gsub("��","", text_ediya)
text_ediya <- gsub("��","", text_ediya)
text_ediya <- gsub("^","", text_ediya)
text_ediya <- gsub("-","", text_ediya)
text_ediya <- gsub('[��-��]','',text_ediya)
text_ediya<- gsub('(��|��|��)','',text_ediya)
text_ediya <- gsub("\\d+","",text_ediya)

# Extra Noun���� �ٽ� �ݺ�
noun_ediya = sapply(text_ediya, extractNoun, USE.NAMES = F)
noun_ediya_unlist = unlist(noun_ediya)

# �� ���� �̻��� �͸� ����

noun_ediya_unlist <- Filter(function(x){
  nchar(x)>=2}, noun_ediya_unlist)

noun_ediya_unlist <- c(noun_ediya_unlist, rep("10�ֳ�", ann))

count_ediya = table(noun_ediya_unlist)
top_ediya = head(sort(count_ediya, decreasing = T), 100)

grep('', top_ediya)

# Word Cloud
wordcloud(names(count_ediya),freq=count_ediya, scale=c(5,.8),min.freq=5,
          max.words=Inf, random.order=F, rot.per=.1, colors=color6, family="bedal")