data <- read.table('variant1.csv', header = TRUE, sep = ';', encoding = "UTF-8")
write.table(data, 'data.csv', sep = '\t', row.names = TRUE, col.names = NA, fileEncoding = "UTF-8")
saveRDS(data, 'data.RDS')
data <- readRDS('data.RDS')
save(data, file = 'data.RData')
load('data.RData')
View(data)
View(data$'X.п.п'[data$'возраст' > 30])
View(subset(data, `возраст` > 20))
data1 <- subset(data, data$`возраст` > 30, select = c(`возраст`, `пол`))

View(data$`возраст`[1:3])
View(data[c(1, 3, 5)])
View(data[1:100,])

data <- na.omit(data)
View(data[1:3])
data <- edit(data)
names(data) <- c("Ð²Ð¾Ð·ÑÐ°ÑÑ", "стаж", "часы", "доход")
View(data)

data$`X.п.п` <- NULL

a <- c(1, 2, 3)
is.numeric(a)
is.vector(a)
a <- as.character(a)
str(data)
summary(data)

mystats <- function(x) {
  if (is.numeric(x)) {
    m <- mean(x)
    n <- length(x)
    s <- sd(x)
    skew <- sum((x - m)^3 / s^3) / n
    kurt <- sum((x - m)^4 / s^4) / n - 3
    return(c(n=n, mean=m, stdev=s, skew=skew, kurtosis=kurt))
  }
}
mystats(data$`возраст`)


