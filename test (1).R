test_data <- read.csv("https://stepic.org/media/attachments/course/724/hard_task.csv")
correct_answer <- read.csv("https://stepic.org/media/attachments/course/724/hard_task_ans.csv")

test_data <- read.csv("https://stepic.org/media/attachments/course/724/test_data_2.csv")
correct_answer <- read.csv("https://stepic.org/media/attachments/course/724/test_data_2_ans.csv")
is.na(test_data$x)
head(test_data)
head(correct_answer, 10)

data <- ToothGrowth
head(data)
aggregate(len ~ supp + dose, data , mean)
aggregate(data$len, by = list(data$supp,data$dose) , sd)



test_data[,sapply(test_data, function(x) is.numeric(x))]

aggregate(data[,1])

num <- sapply(test_data, function(x) is.numeric(x))
test_data[num]
fac <- sapply(test_data, function(x) is.factor(x))


aggregate(test_data[,num], by = list(test_data[,fac]), sd)

aggregate(test_data[,num], by = as.list(test_data[,fac]), sd)
aggregate(mpg, by=list(mtcars$cyl,mtcars$vs), 
          FUN=mean, na.rm=TRUE)
mtcars <- mtcars

aggregate(x ~ factor_2 + factor_3 + factor_4, test_data, sd)



num <- sapply(x, function(x) is.numeric(x))
fac <- sapply(x, function(x) is.factor(x))
tab <- as.data.frame(aggregate(test_data[,num], by = as.list(test_data[,fac]), mean))
std <- as.data.frame(aggregate(test_data[,num], by = as.list(test_data[,fac]), sd))
tab[,ncol(tab)]
tl <- split(x, x[,1])
tl_2 <- lapply(tl, function(x) (x > (tab[,ncol(tab)]+std[,ncol(std)]*2)) )
tl_2
lapply(tl, function(x) print(x[is.numeric(x)]))

tl_3 <- do.call(rbind, tl_2)
tl_3
tl_4 <- tl_3[ order(as.numeric(row.names(tl_3))), ]
tl_4
NNAcol <- apply(tl_4, 2, function(x) any(x[!is.na(x)]))
NNAcol
tl_5 <- sapply(tl_4[,NNAcol], as.numeric)
tl_5
x["is_outlier"] <- tl_5
x

test_function <- function(x){
  num <- sapply(x, function(x) is.numeric(x))
  fac <- sapply(x, function(x) is.factor(x))
  tab <- as.data.frame(aggregate(test_data[,num], by = as.list(test_data[,fac]), mean))
  std <- as.data.frame(aggregate(test_data[,num], by = as.list(test_data[,fac]), sd))
  tab[,ncol(tab)]
  tl <- split(x, x[,1])
  tl_2 <- lapply(tl, function(x) (abs(x) > (tab[,ncol(tab)]+std[,ncol(std)]*2)))
  tl_2
  tl_3 <- do.call(rbind, tl_2)
  tl_3
  tl_4 <- tl_3[ order(as.numeric(row.names(tl_3))), ]
  NNAcol <- apply(tl_4, 2, function(x) any(x[!is.na(x)]))
  NNAcol
  tl_5 <- sapply(tl_4[,NNAcol], as.numeric)
  tl_5
  x["is_outlier"] <- tl_5
  as.data.frame(x)
  #tl_5 <- sapply(tl_4[,!NAcol], as.numeric)
  #tl_5
  #x["is_outlier"] <- tl_5
  #x
  #apply(tl_4, 2, is.logical)
  #tl_4[,cols] <- lapply(tl_4[,cols], as.numeric)
  #head(tl_4)
  #head(tl_2[[1]])
  #lapply(tl, function(x) print(tab[,3]) )
  # lapply(tl, function(x) x[,3] > c(1,100) )
  # tab[sapply(tab, function(x) is.factor(x))][,1]
}


test_data[230,]
new_1 <- test_function(test_data)
new_1

new_1[new_1$is_outlier == 1,] 
correct_answer[correct_answer$is_outlier == 1,]

length(new_1$is_outlier[new_1$is_outlier == 1])
length(correct_answer$is_outlier[correct_answer$is_outlier == 1])


test_function_2 <- function(x){
  num <- sapply(x, function(x) is.numeric(x))
  fac <- sapply(x, function(x) is.factor(x))
  tab <- as.data.frame(aggregate(test_data[,num], by = as.list(test_data[,fac]), mean))
  sdt <- as.data.frame(aggregate(test_data[,num], by = as.list(test_data[,fac]), sd))
  
  tl <- split(x, x[,1])
  
  
  lapply(tl, function(x) x > (tab[,3]+sdt[,3]*2))
  tl
  #(tab[1,3]+sdt[1,3]*2)
  #lapply(tl, function(x) print(tab[,3]) )
  # lapply(tl, function(x) x[,3] > c(1,100) )
  # tab[sapply(tab, function(x) is.factor(x))][,1]
}


new_2 <- test_function_2(test_data)
new_2


rbind(new_2[[1]],new_2[[2]])

d$index <- as.numeric(row.names(d))
d[order(d$index), ]

tl <- split(test_data, test_data$factor_3)
length(tl)
head(tl[[2]])
names(tl)
tl2 <- split(test_data, test_data$factor_2)

tl2
names(tl2)
head(tl[[1]])
lapply(split(data2, data2[,1]), function(x) mean(x[,2]))


plot(mtcars$hp,mtcars$disp)
abline(lm(mtcars$disp~mtcars$hp) , col = "red")
summary(lm(mtcars$disp~mtcars$hp))

2%%2


