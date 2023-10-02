# 1. Установить рабочую дирректорию, загрузить файл с данными
setwd("C:/Users/filon/DataspellProjects/Lab1")
Sys.setlocale("LC_ALL", "Russian_Russia.1251")
data <- read.table('variant1.csv', header = TRUE, sep = ';', encoding = "windows-1251")
write.table(data, 'data.csv', sep = '\t', row.names = TRUE, col.names = NA, fileEncoding = "windows-1251")
saveRDS(data, 'data.RDS')
data <- readRDS('data.RDS')
save(data, file = 'data.RData')
load('data.RData')
View(data)


#TODO/-------------------------------------------------------------------------------------------------------------/
# Подготовка таблиц к анализу
data$`X.п.п` <- NULL
# Очистка пустых значений из таблицы
data <- na.omit(data)
colnames(data)
# Переименование столбцов на английский
colnames(data) <- c('group', 'gender', 'age', 'experience', 'in_time', 'mistakes', 'score', 'qualitative', 'documentation')
colnames(data)
View(data)


#TODO/-------------------------------------------------------------------------------------------------------------/
# 2. Посмотреть таблицу с данными
# data$qualitative
# data$age[data$documentation == 1]
# data$age[1:3]
# View(data[c(1,3,5)])
# View(data[1:100,])
# View(data[1:3])


#TODO/-------------------------------------------------------------------------------------------------------------/
# Разбиваем таблицы на основе стажа
data1 <- subset(data, data$experience < 3.5)
View(data1)
data2 <- subset(data, data$experience >= 3.5)
View(data2)


#TODO/-------------------------------------------------------------------------------------------------------------/
mystats <- function(x) {
  if (is.numeric(x)) {
    m <- mean(x)
    n <- length(x)
    s <- sd(x)
    skew <- sum((x - m)^3 / s^3) / n
    kurt <- sum((x - m)^4 / s^4) / n - 3
    return(c(n = n, mean = m, stdev = s, skew = skew, kurtosis = kurt))
  }
}

# 3. Расчитатаь основные статические хпрактеристики
library('psych')
# Для общей выборки
data <- na.omit(data)
sm <- describe(data)
View(sm[c(2, 3, 4, 5, 7, 8, 9, 10, 11, 12)])
summary(data)
mystats(data)
#/---------------------------------------------/
# Для подвыборки со стажем < 3.5
data1 <- na.omit(data1)
sm1 <- describe(data1)
View(sm1[c(2, 3, 4, 5, 7, 8, 9, 10, 11, 12)])
summary(data1)
mystats(data$group)
#/---------------------------------------------/
# Для подвыборки со стажем >= 3.5
data2 <- na.omit(data2)
sm2 <- describe(data2)
View(sm2[c(2, 3, 4, 5, 7, 8, 9, 10, 11, 12)])
summary(data2)


#TODO/-------------------------------------------------------------------------------------------------------------/
# 4. Графический анализ
# Диаграмма рассяния для data
par(mfrow = c(1, 1))
attach(data)
plot(data.frame(data$age, data$score), xlab = 'age', ylab = 'score', col = 'blue', pch = 16)
abline(lm(data$score ~ data$age), col = 'red', lwd = 2)
title(main = 'scatter diagramm for data')
dev.off()
# Диаграмма рассеяния для data1
attach(data1)
plot(data.frame(data1$age, data1$score), xlab = 'age', ylab = 'score', col = 'green', pch = 16)
abline(lm(data1$score ~ data1$age), col = 'red', lwd = 2)
title(main = 'scatter diagramm for data1')
dev.off()
# Диаграмма рассеяния для data2
png('diagramm1.png')
attach(data2)
plot(data2$age, data2$score, xlab = 'age', ylab = 'score', col = 'purple', pch = 16)
abline(lm(data2$score ~ data2$age), col = 'red', lwd = 2)
title(main = 'scatter diagramm for data2')
dev.off()


#TODO/-------------------------------------------------------------------------------------------------------------/
# Радиальная диаграмма для data
par(mfrow = c(1, 3))
x <- table(data$documentation)
pieper <- round(100 * x / sum(x), 1)
pie(x, labels = pieper, radius = 1, col = c('green', 'yellow', 'purple', 'black'), clockwise = TRUE, main = 'data')
legend('topright', c('1', '2', '3', '4'), cex = 0.8, fill = c('green', 'yellow', 'purple', 'black'))
# Радиальная диаграмма для data1
x1 <- table(data1$documentation)
pieper1 <- round(100 * x1 / sum(x1), 1)
pie(x1, labels = pieper1, radius = 1, col = c('green', 'yellow', 'purple', 'black'), clockwise = TRUE, main = 'data1')
legend('topright', c('1', '2', '3', '4'), cex = 0.8, fill = c('green', 'yellow', 'purple', 'black'))
# Радиальная диаграмма для data2
x2 <- table(data2$documentation)
pieper2 <- round(100 * x2 / sum(x2), 1)
pie(x2, labels = pieper2, radius = 1, col = c('green', 'yellow', 'purple', 'black'), clockwise = TRUE, main = 'data2')
legend('topright', c('1', '2', '3', '4'), cex = 0.8, fill = c('green', 'yellow', 'purple', 'black'))


#TODO/-------------------------------------------------------------------------------------------------------------/
# Категориальная радиальная диаграмма для data
png('diagramm1.png')
data_11 <- data[data$group == 1 & data$gender == 1,]
data_12 <- data[data$group == 1 & data$gender == 2,]
data_21 <- data[data$group == 2 & data$gender == 1,]
data_22 <- data[data$group == 2 & data$gender == 2,]
x_0 <- table(data_11$qualitative)
x_1 <- table(data_12$qualitative)
x_2 <- table(data_21$qualitative)
x_3 <- table(data_22$qualitative)
pieper_0 <- round(100 * x_0 / sum(x_0), 1)
pieper_1 <- round(100 * x_1 / sum(x_1), 1)
pieper_2 <- round(100 * x_2 / sum(x_2), 1)
pieper_3 <- round(100 * x_3 / sum(x_3), 1)
par(mfrow = c(2, 2))
pie(x_0, pieper_0, main = "Radial chart", radius = 0.5,
    xlab = "gender - 1", ylab = "group - 1", col = c('green', 'purple', 'cyan'), clockwise = TRUE)
legend('topright', c('low', 'average', 'high'), cex = 0.5, fill = c('green', 'purple', 'cyan'))
pie(x_1, pieper_1, main = "Radial chart", radius = 0.5,
    xlab = "gender - 1", ylab = "group - 2", col = c('green', 'purple', 'cyan'), clockwise = TRUE)
legend('topright', c('low', 'average', 'high'), cex = 0.5, fill = c('green', 'purple', 'cyan'))
pie(x_2, pieper_2, main = "Radial chart", radius = 0.5,
    xlab = "gender - 2", ylab = "group - 1", col = c('green', 'purple', 'cyan'), clockwise = TRUE)
legend('topright', c('low', 'average', 'high'), cex = 0.5, fill = c('green', 'purple', 'cyan'))
pie(x_3, pieper_3, main = "Radial chart", radius = 0.5,
    xlab = "gender - 2", ylab = "group - 2", col = c('green', 'purple', 'cyan'), clockwise = TRUE)
legend('topright', c('low', 'average', 'high'), cex = 0.5, fill = c('green', 'purple', 'cyan'))
dev.off()
# /-------------------------------------------------------------------------------------------------/
# Категориальная радиальная диаграмма для data1
png("diagramm1.png")
data1_11 <- data1[data1$group == 1 & data1$gender == 1,]
data1_12 <- data1[data1$group == 1 & data1$gender == 2,]
data1_21 <- data1[data1$group == 2 & data1$gender == 1,]
data1_22 <- data1[data1$group == 2 & data1$gender == 2,]
x1_0 <- table(data1_11$qualitative)
x1_1 <- table(data1_12$qualitative)
x1_2 <- table(data1_21$qualitative)
x1_3 <- table(data1_22$qualitative)
pieper1_0 <- round(100 * x1_0 / sum(x1_0), 1)
pieper1_1 <- round(100 * x1_1 / sum(x1_1), 1)
pieper1_2 <- round(100 * x1_2 / sum(x1_2), 1)
pieper1_3 <- round(100 * x1_3 / sum(x1_3), 1)
par(mfrow = c(1, 2))
pie(x1_0, pieper1_0, main = "Radial chart", radius = 0.5,
    xlab = "gender - 1", ylab = "group - 1", col = c('green', 'purple', 'cyan'), clockwise = TRUE)
legend('topright', c('low', 'average', 'high'), cex = 0.5, fill = c('green', 'purple', 'cyan'))
pie(x1_1, pieper1_1, main = "Radial chart", radius = 0.5,
    xlab = "gender - 1", ylab = "group - 2", col = c('green', 'purple', 'cyan'), clockwise = TRUE)
legend('topright', c('low', 'average', 'high'), cex = 0.5, fill = c('green', 'purple', 'cyan'))
dev.off()
# /-------------------------------------------------------------------------------------------------/
# Категориальная радиальная диаграмма для data2
png("diagramm1.png")
data2_11 <- data2[data2$group == 1 & data2$gender == 1,]
data2_12 <- data2[data2$group == 1 & data2$gender == 2,]
data2_21 <- data2[data2$group == 2 & data2$gender == 1,]
data2_22 <- data2[data2$group == 2 & data2$gender == 2,]
x2_0 <- table(data2_11$qualitative)
x2_1 <- table(data2_12$qualitative)
x2_2 <- table(data2_21$qualitative)
x2_3 <- table(data2_22$qualitative)
pieper2_0 <- round(100 * x2_0 / sum(x2_0), 1)
pieper2_1 <- round(100 * x2_1 / sum(x2_1), 1)
pieper2_2 <- round(100 * x2_2 / sum(x2_2), 1)
pieper2_3 <- round(100 * x2_3 / sum(x2_3), 1)
par(mfrow = c(1, 3))
pie(x2_1, pieper2_1, main = "Radial chart", radius = 1,
    xlab = "gender - 1", ylab = "group - 2", col = c('green', 'purple', 'cyan'), clockwise = TRUE)
legend('topright', c('low', 'average', 'high'), cex = 0.8, fill = c('green', 'purple', 'cyan'))
pie(x2_2, pieper2_2, main = "Radial chart", radius = 1,
    xlab = "gender - 2", ylab = "group - 1", col = c('green', 'purple', 'cyan'), clockwise = TRUE)
legend('topright', c('low', 'average', 'high'), cex = 0.8, fill = c('green', 'purple', 'cyan'))
pie(x2_3, pieper2_3, main = "Radial chart", radius = 1,
    xlab = "gender - 2", ylab = "group - 2", col = c('green', 'purple', 'cyan'), clockwise = TRUE)
legend('topright', c('low', 'average', 'high'), cex = 0.8, fill = c('green', 'purple', 'cyan'))
dev.off()
#TODO/-------------------------------------------------------------------------------------------------------------/
# Категориальная столбиковая диаграмма для data
par(mfrow = c(1, 2))
barplot(table(data$age[data$group == 1]), col = 'red', main = '1', ylim = c(0, 25), xlab = 'age')
barplot(table(data$age[data$group == 2]), col = 'green', main = '2', ylim = c(0, 25), xlab = 'age')
# Категориальная столбиковая диаграмма для data1
par(mfrow = c(1, 1))
barplot(table(data1$age[data1$group == 1]), col = 'red', main = '1', ylim = c(0, 25), xlab = 'age')
# Так как нет людей во 2й группе, мы можем убрать эту строку
# barplot(table(data1$age[data1$group == 2]), col = 'green', main = '2', xlab = 'age')
# Категориальная столбиковая диаграмма для data2
par(mfrow = c(1, 2))
barplot(table(data2$age[data2$group == 1]), col = 'red', main = '1', xlab = 'age')
barplot(table(data2$age[data2$group == 2]), col = 'green', main = '2', ylim = c(0, 25), xlab = 'age')


#TODO/-------------------------------------------------------------------------------------------------------------/
# Диаграмма размаха для data
boxplot(data$age ~ data$gender, main = 'boxplot for data', col = 'green', xlab = 'gender', ylab = 'age')
# Диаграмма размаха для data1
boxplot(data1$age ~ data1$gender, main = 'boxplot for data1', col = 'green', xlab = 'gender', ylab = 'age')
# Диаграмма размаха для data2
boxplot(data2$age ~ data2$gender, main = 'boxplot for data2', col = 'green', xlab = 'gender', ylab = 'age')


#TODO/-------------------------------------------------------------------------------------------------------------/
# Гистограмма для data
par(mfrow = c(2, 3))
hist(data$age, col = 'red', breaks = 12, freq = FALSE, ylim = c(0.00, 0.20), xlab = 'Age', main = 'Histogram for data')
lines(density(data$age), col = 'blue', lty = 2, lwd = 2)
hist(data$experience, col = 'red', breaks = 12, freq = FALSE, ylim = c(0.00, 0.5), xlab = 'Experience', main = 'Histogram for data')
lines(density(data$experience), col = 'blue', lty = 2, lwd = 2)
hist(data$in_time, col = 'red', breaks = 12, freq = FALSE, ylim = c(0.00, 0.1), xlab = 'In time', main = 'Histogram for data')
lines(density(data$in_time), col = 'blue', lty = 2, lwd = 2)
hist(data$mistakes, col = 'red', breaks = 12, freq = FALSE, ylim = c(0.00, 0.1), xlab = 'Mistakes', main = 'Histogram for data')
lines(density(data$mistakes), col = 'blue', lty = 2, lwd = 2)
hist(data$score, col = 'red', breaks = 12, freq = FALSE, ylim = c(0.00, 0.04), xlab = 'Score', main = 'Histogram for data')
lines(density(data$score), col = 'blue', lty = 2, lwd = 2)
# Гистограмма для data1
par(mfrow = c(2, 3))
hist(data1$age, col = 'red', breaks = 12, freq = FALSE, ylim = c(0.00, 0.25), xlab = 'Age', main = 'Histogram for data1')
lines(density(data1$age), col = 'blue', lty = 2, lwd = 2)
hist(data1$experience, col = 'red', breaks = 12, freq = FALSE, ylim = c(0.00, 1.3), xlab = 'Experience', main = 'Histogram for data1')
lines(density(data1$experience), col = 'blue', lty = 2, lwd = 2)
hist(data1$in_time, col = 'red', breaks = 12, freq = FALSE, ylim = c(0.00, 0.1), xlab = 'In time', main = 'Histogram for data1')
lines(density(data1$in_time), col = 'blue', lty = 2, lwd = 2)
hist(data1$mistakes, col = 'red', breaks = 12, freq = FALSE, ylim = c(0.00, 0.1), xlab = 'Mistakes', main = 'Histogram for data1')
lines(density(data1$mistakes), col = 'blue', lty = 2, lwd = 2)
hist(data1$score, col = 'red', breaks = 12, freq = FALSE, ylim = c(0.00, 0.05), xlab = 'Score', main = 'Histogram for data1')
lines(density(data1$score), col = 'blue', lty = 2, lwd = 2)
# Гистограмма для data2
par(mfrow = c(2, 3))
hist(data2$age, col = 'red', breaks = 12, freq = FALSE, ylim = c(0.00, 0.25), xlab = 'Age', main = 'Histogram for data2')
lines(density(data2$age), col = 'blue', lty = 2, lwd = 2)
hist(data2$experience, col = 'red', breaks = 12, freq = FALSE, ylim = c(0.00, 1.3), xlab = 'Experience', main = 'Histogram for data2')
lines(density(data2$experience), col = 'blue', lty = 2, lwd = 2)
hist(data2$in_time, col = 'red', breaks = 12, freq = FALSE, ylim = c(0.00, 0.1), xlab = 'In time', main = 'Histogram for data2')
lines(density(data2$in_time), col = 'blue', lty = 2, lwd = 2)
hist(data2$mistakes, col = 'red', breaks = 12, freq = FALSE, ylim = c(0.00, 0.1), xlab = 'Mistakes', main = 'Histogram for data2')
lines(density(data2$mistakes), col = 'blue', lty = 2, lwd = 2)
hist(data2$score, col = 'red', breaks = 12, freq = FALSE, ylim = c(0.00, 0.05), xlab = 'Score', main = 'Histogram for data2')
lines(density(data2$score), col = 'blue', lty = 2, lwd = 2)


#TODO/-------------------------------------------------------------------------------------------------------------/
# Матрица диаграмм для data
pairs(~data$age +
  data$experience +
  data$in_time +
  data$mistakes +
  data$score, pch = 16, upper.panel = NULL, labels = c('Age', 'Experience', 'In time', 'Mistakes', 'Score'), col = 'cyan')
# Матрица диаграмм для data1
pairs(~data1$age +
  data1$experience +
  data1$in_time +
  data1$mistakes +
  data1$score, pch = 16, upper.panel = NULL, labels = c('Age', 'Experience', 'In time', 'Mistakes', 'Score'), col = 'light green')
# Матрица диаграмм для data2
pairs(~data2$age +
  data2$experience +
  data2$in_time +
  data2$mistakes +
  data2$score, pch = 16, upper.panel = NULL, labels = c('Age', 'Experience', 'In time', 'Mistakes', 'Score'), col = 'light blue')


#TODO/-------------------------------------------------------------------------------------------------------------/
# 5. Корреляционный анализ данных
# Корреляционый анализ для data
# Chi-квадрат 5.1
chisq.test(data$qualitative, data$gender)
# Критерий фишера 5.1
fisher.test(table(data$qualitative, data$gender))
# Расчёт коэфициентов корреляции (таблица м это таблица лишь с числовыми значениями)
M <- data[, unlist(lapply(data, is.numeric))]
# Коэфициент корреляции Пирсона 5.2
N <- cor(M, use = "pairwise.complete.obs")
N[is.na(N)] <- 0
rowSums(N)
# Коэфициент корреляции Спирмана 5.2
cor(M, use = "pairwise.complete.obs", method = 'spearman')
# Коэфициент корреляции Кендала 5.2
cor(M, use = "pairwise.complete.obs", method = 'kendall')
library('ggm')
# Кэфициент частной корреляции 5.3
pcor(c(1, 4, 2, 3, 5, 6, 7, 8), cov(M))
# Матрицы коэффициентов корреляции 5.4
library('corrplot')
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD",
                          "#4477AA"))
corrplot(N, method = "color", col = NULL,
         type = "upper", order = "hclust",
         addCoef.col = "black", tl.col = "black", tl.srt = 45,
         sig.level = 0.01, insig = "blank",
         diag = FALSE)
#/--------------------------------------------------------/
# Корреляционый анализ для data1
# Chi-квадрат
chisq.test(data1$qualitative, data1$gender)
# Критерий фишера
fisher.test(table(data1$qualitative, data1$gender))
# Расчёт коэфициентов корреляции
M1 <- data1[, unlist(lapply(data1, is.numeric))]
# Коэфициент корреляции Пирсона
N1 <- cor(M1, use = "pairwise.complete.obs")
N1[is.na(N1)] <- 0
rowSums(N1)
# Коэфициент корреляции Спирмана
cor(M1, use = "pairwise.complete.obs", method = 'spearman')
# Коэфициент корреляции Кендала
cor(M1, use = "pairwise.complete.obs", method = 'kendall')
library('ggm')
# Кэфициент частной корреляции
pcor(c(4, 8, 2, 3, 5, 6, 7), cov(M1))
corrplot(N1, method = "color", col = COL1(),
         type = "upper", order = "hclust",
         addCoef.col = "black", tl.col = "black", tl.srt = 45,
         sig.level = 0.01, insig = "blank",
         diag = FALSE)
#/--------------------------------------------------------/
# Корреляционый анализ для data2
# Chi-квадрат
chisq.test(data2$qualitative, data2$gender)
# Критерий фишера
fisher.test(table(data2$gender, data2$gender))
# Расчёт коэфициентов корреляции
M2 <- data2[, unlist(lapply(data2, is.numeric))]
# Коэфициент корреляции Пирсона
N2 <- cor(M2, use = "pairwise.complete.obs")
rowSums(N2)
# Коэфициент корреляции Спирмана
cor(M2, use = "pairwise.complete.obs", method = 'spearman')
# Коэфициент корреляции Кендала
cor(M2, use = "pairwise.complete.obs", method = 'kendall')
library('ggm')
# Кэфициент частной корреляции
pcor(c(4, 8, 1, 2, 3, 5, 6, 7), cov(M2))
corrplot(N2, method = "color", col = NULL,
         type = "upper", order = "hclust",
         addCoef.col = "black", tl.col = "black", tl.srt = 45,
         sig.level = 0.01, insig = "blank",
         diag = FALSE)