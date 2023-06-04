# 1. Генерируем выборки
x_1=rnorm(100,20, 7) 
x_2=rnorm(100,35, 7) 
x_3=rnorm(100,30, 7) 

sum_length = length(x_1) + length(x_2) + length(x_3)

# 2. Считаем средние
## общее
general_average = sum(x_1, x_2, x_3) / sum_length
## частные
x_1_average = mean(x_1)
x_2_average = mean(x_2)
x_3_average = mean(x_3)

# 3. Строим график
## создадим y координаты точек
y_1 = c(1:length(x_1))
y_2 = c(length(x_1)+1:length(x_2))
y_3 = c(length(x_2)+1:length(x_2)+length(x_3))
## отображаем точки
plot(y_1, x_1, pch = 19, col = "red", ylim=c(0,max(x_1, x_2, x_3)), xlim=c(0,sum_length),
     xlab="number of points", ylab="values")
points(y_2, x_2, col = "green", pch = 19)
points(y_3, x_3, col = "blue", pch = 19)
## оторбражаем линии средних
xspline(c(0, length(x_1)),c(x_1_average, x_1_average), border = "red", lty = 5)
xspline(c(length(x_1)+1, length(x_1)+length(x_2)),c(x_2_average, x_2_average), border = "green", lty = 5)
xspline(c(length(x_1)+length(x_2)+1, length(x_1)+length(x_2)+length(x_3)),c(x_3_average, x_3_average), border = "blue", lty = 5)
abline(h = general_average, col = "black", lty = 5)


# 4. считаем дисперсии
## считаем общую дисперсию
total_variance =  
  (sum((x_1 - general_average)^2)
  + sum((x_2 - general_average)^2)
  + sum((x_3 - general_average)^2)) /
  (length(x_1) 
   + length(x_2) 
   + length(x_3))

## частные внутригрупповые дисперсии
intragroup_variance_1 = sum((x_1 - x_1_average)^2) / length(x_1)
intragroup_variance_2 = sum((x_2 - x_2_average)^2) / length(x_2)
intragroup_variance_3 = sum((x_3 - x_3_average)^2) / length(x_3)

## среднюю внутригруповую дисперсию
average_intragroup_variance = 
  (intragroup_variance_1*length(x_1)
   + intragroup_variance_2*length(x_2)
   + intragroup_variance_3*length(x_3))  / sum_length

## межгрупповую дисперию
intergroup_variance = 
  ((x_1_average - general_average)^2 * length(x_1)
  +(x_2_average - general_average)^2 * length(x_2)
  +(x_3_average - general_average)^2 * length(x_3)) / sum_length

# 5. Проверяем правило сложения дисперсий
potential_total_variance = average_intragroup_variance + intergroup_variance
## TRUE если правило выполняется, FALSE если нет
setequal(round(potential_total_variance, digits = 13), round(total_variance, digits = 13))

# 6. Находим корреляционное отношение
correlation_ratio = sqrt(intergroup_variance / total_variance)

# 7. Находим коэффициент детерминации
determination_coef = intergroup_variance / total_variance * 100

# 8. Проверяем силу влияния группирующего фактора по шкале Чеддока
if ( correlation_ratio < 0) {
  cat("Ошибка в рассчётах!")
} else if (correlation_ratio == 0.0) {
  cat("Связь отсутствует")
} else if (0 < correlation_ratio && correlation_ratio < 0.2) {
  cat("Связь очень слабая")
} else if (0.2 <= correlation_ratio && correlation_ratio < 0.3) {
  cat("Связь слабая")
} else if (0.3 <= correlation_ratio && correlation_ratio < 0.5) {
  cat("Связь умеренная")
} else if (0.5 <= correlation_ratio && correlation_ratio < 0.7) {
  cat("Связь заметная")
} else if (0.7 <= correlation_ratio && correlation_ratio < 0.9) {
  cat("Связь сильная")
} else if (0.9 <= correlation_ratio && correlation_ratio < 1.0) {
  cat("Связь очень сильная")
} else if (correlation_ratio == 1.0) {
  cat("Связь функциональная")
} else {
  cat("Ошибка в рассчётах!")
}
cat("Корреляционное отношение равно: ", correlation_ratio)

