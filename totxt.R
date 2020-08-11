############################################

# t <- data.frame(text_starbucks)
# n <- data.frame(n_starbucks_unlist)

head(noun_starbucks)
length(noun_starbucks)

# unlist할때 파일로 보내기 위해 구분자 넣어줌
for(i in 1:1000){
  n = length(n_starbucks[[i]])
  n_starbucks[[i]][n+1] = "-------"
}

n_starbucks[[1000]]
n_starbucks = noun_starbucks


write.table(t, file = "chayong.txt", sep = "\t", row.names = FALSE)
write.table(n, file = "chayong2.txt", sep = "\t", row.names = FALSE)

mergeUserDic(data.frame(c("마일리지"), "ncn"))
mergeUserDic(data.frame(c("프라푸치노"), "ncn"))

for(i in 1:100){
  print(text_starbucks[i])
  print(noun_starbucks[i])
  print("-----------------")
}

noun_starbucks[1:10]
head(text_starbucks,30)

grep("션", noun_starbucks_unlist)
noun_starbucks[grep("프로모션", noun_starbucks)]
grep("션", noun_starbucks)

text_starbucks[700]
head(text_starbucks)
###############################################