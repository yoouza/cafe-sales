############################################

# t <- data.frame(text_starbucks)
# n <- data.frame(n_starbucks_unlist)

head(noun_starbucks)
length(noun_starbucks)

# unlist�Ҷ� ���Ϸ� ������ ���� ������ �־���
for(i in 1:1000){
  n = length(n_starbucks[[i]])
  n_starbucks[[i]][n+1] = "-------"
}

n_starbucks[[1000]]
n_starbucks = noun_starbucks


write.table(t, file = "chayong.txt", sep = "\t", row.names = FALSE)
write.table(n, file = "chayong2.txt", sep = "\t", row.names = FALSE)

mergeUserDic(data.frame(c("���ϸ���"), "ncn"))
mergeUserDic(data.frame(c("����Ǫġ��"), "ncn"))

for(i in 1:100){
  print(text_starbucks[i])
  print(noun_starbucks[i])
  print("-----------------")
}

noun_starbucks[1:10]
head(text_starbucks,30)

grep("��", noun_starbucks_unlist)
noun_starbucks[grep("���θ��", noun_starbucks)]
grep("��", noun_starbucks)

text_starbucks[700]
head(text_starbucks)
###############################################