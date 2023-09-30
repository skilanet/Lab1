# Создание таблиц
Sys.setlocale("LC_ALL", "Russian_Russia.1251")
data <- read.table('variant1.csv', header = TRUE, sep = ';', encoding = "RU.1251")
write.table(data, 'data.csv', sep = '\t', row.names = TRUE, col.names = NA, fileEncoding = "1251")
saveRDS(data, 'data.RDS')
data <- readRDS('data.RDS')
save(data, file = 'data.RData')
load('data.RData')

# Отображение таблиц с разными параметрами
View(data)
View(data$`X.п.п`[data$`возраст` > 30])
View(subset(data, `возраст` > 20))
data1 <- subset(data, data$`возраст` > 30, select = c(`возраст`, `пол`))
View(data$`возраст`[1:3])
View(data[c(1, 3, 5)])
View(data[1:100,])

# Очистка пробелов
data <- na.omit(data)

# Изменение названий столбцов
View(data[1:3])
data <- edit(data)
names(data) <- c("возраст", "стаж", "часы", "доход")
View(data)
names(data)

# Удаление столбцов
data$`X.п.п` <- NULL

# Формат данных
a <- c(1, 2, 3)
is.numeric(a)
is.vector(a)
a <- as.character(a)

# Сводная информация по тоблице
str(data)

# Основные статистицеские характеристики
summary(data)

# Создание функции
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

# Компктная сводка по таблице
library('psych')
sm <- describe(data)
View(sm)

# Построение графиков
View(data[0:2,])
plot(data.frame(data$`возраст`, data$`стаж`))
plot(data.frame(data$`возраст`, data$`стаж`), type = 'b', col = 'red'
  , lty = 2, pch = 2, lwd = 2
  , main = 'zavisimost vozrasta ot staza'
  , xlab = 'vozrast'
  , ylab = 'staz'
  , xlim = c(10, 40), ylim = c(0, 10))
#type= - тип графика
#col= - цвет элементов
#lty= - тип линии (2 – dashed, пунктирный)
#pch= - тип точки
#lwd= - ширина линии
#xlim= - диапазон изменения оси X
#ylim= - диапазон изменения оси Y

attach(data)
plot(data$`возраст`, data$`стаж`)
abline(lm(data$`стаж`~data$`возраст`))
title('name')
detach(data)

png('mygraph.png')
attach(data)
plot(data$`возраст`, data$`стаж`)
abline(lm(data$`стаж`~data$`возраст`))
title('name')
dev.off()

dev.new()

# Радиальная диаграмма
x <- c(summary(data$`группа`))
piepeecent <- round(100 * x / sum(x), 1)
pie(x, piepeecent, radius = 1, main = 'radialnaya diagramma', col = c('red', 'blue'), clockwise = TRUE)
legend('topright', c('Yes', 'No'), cex = 0.8, fill = c('red', 'blue'))

# Столбиковая диаграмма
par(mfrow = c(1, 2))
barplot(table(data$age[data$group == 1]), col = 'red', main = '1', xlab = 'age')
barplot(table(data$age[data$group == 2]), col = 'green', main = '2', xlab = 'age')

# Диаграмма размахов
par(mfrow = c(1, 1))
boxplot(data$age, col = 'green')
boxplot(data$budget~data$mistakes, col = 'coral', data = data)

# Гистограма
hist(data$mistakes, freq = FALSE, breaks = 12)

# Матричный график
View(data)
pairs(~data$age + data$documentation + data$experience)

table1 <- table(data$age, data$group)
prop.table(table1)
addmargins(table1)

chisq.test(table(data$mistakes, data$budget))
fisher.test(table(data$age, data$budget))

