main <- function() {
  #CONSTS
  N = 100
  a=2
  b=1
  sigma=1
  ALPHA = 0.025
  
  i=1:N
  x=1+0.1*i
  eps=rnorm(N,0,sigma**2)
  
  y=a+b*x+eps # формируем зависимую переменную
  xy=data.frame(cbind(x,y)) # объединяем в таблицу
  plot(y~x,data=xy,main='Регрессия Y на X')
  
  # 1. МНК оценка параметров
  b_stat = ( mean(x*y) - (mean(x) * mean(y)) ) / ( mean(x ** 2)  - ( mean(x) ** 2 ) )
  a_stat = mean(y) - b_stat * mean(x)
  
  cat('Оценка параметра a: ', a_stat, '\n')
  cat('Оценка параметра b: ', b_stat, '\n')
  
  # 2. Дисперсии наблюдений и оценкок параметров
  S2 = (1 / (N-2) ) * sum( (y - a_stat - b_stat*x) ** 2)
  Sb2 = S2 / sum((x - mean(x)) ** 2)
  Sa2 = S2 * sum(x ** 2) / (  N * sum((x - mean(x)) ** 2) )
  
  
  cat('Оценка дисперсии оценки параметра a: ', Sa2, ' ( Std. Error: ', sqrt(Sa2), ')\n')
  cat('Оценка дисперсии оценки параметра b: ', Sb2, ' ( Std. Error: ', sqrt(Sb2), ')\n')
  
  # 3. Строим доверительные интервалы
  
  # std. errors
  S = sqrt(S2)
  Sa = sqrt(Sa2)
  Sb = sqrt(Sb2)
  
  a2 = (a_stat - ( qt(ALPHA / 2, N - 2) * Sa) )
  a1 = (a_stat + ( qt(ALPHA / 2, N - 2) * Sa) )
  
  b2 = (b_stat - ( qt(ALPHA / 2, N - 2) * Sb) )
  b1 = (b_stat + ( qt(ALPHA / 2, N - 2) * Sb) )
  
  cat('Доверительные интервалы параметров: ', ALPHA*100 / 2, '%  / ', 100-ALPHA*100 / 2, '%\n')
  cat('                              a: ', '( ', a1, '; ',
      a2, ' )', '\n')
  
  cat('                              b: ', '( ', b1, '; ',
      b2, ' )', '\n')
  
  
  #confint(reg, level=1 - ALPHA)
  
  # 4. Проверка значимости коэффициентов регрессии
  ta = a_stat / Sa
  tb = b_stat / Sb
  t_crit = qt(1 - (ALPHA / 2), N - 2)
  
  cat('t-value параметра a равно', ta ,'\n')
  cat('t-value параметра b равно', tb ,'\n')
  cat('t-critical равно', t_crit ,'\n')
  
  # если один интервал плюс другой минус - гипотеза незначима 
  # если обе полож то значимы-полож
  # если обе отриц - то значим-отриц
  
  # a значим (значим подтвержд)
  if (sign(a1) != sign(a2)){
    cat('Коэффициент a незначим\n')
  } else if (sign(b1) != sign(b2)){
    cat('Коэффициент b незначим\n')
  } else{
    cat('Коэффициент a значим: ', abs(ta) >=  t_crit,'\n')  #прох через нач коорд
    cat('Коэффициент b значим: ', abs(tb) >=  t_crit, '\n')  #y независ от х 
  }
  
  # 5. Коэф. детерминации модели
  y_pred = a_stat + b_stat * x
  
  RSS = sum((y - y_pred) ** 2)
  TSS = sum((y - mean(y)) ** 2)
  ESS = sum((y_pred - mean(y)) ** 2)
  
  R2 = ESS / TSS
  
  cat('Коэффициент детерминации модели R-squared равен: ', R2, '\n')
  
  if (R2 == 0){
    cat('Отсутствует влияние фактора. Вариативность зависимой
      переменной полностью обуславливается шумом\n')
  } else if (R2 == 1){
    cat('Отсутствует влияние шума. Вариативность зависимой
      переменной полностью обуславливается фактора(функциональная зависимость)\n')
  } else{
    cat(R2 * 100, '% дисперсии зависимой переменной объясняется 
      дисперсией независимой переменной\n')
  }
  
  # 6. Проверка гипотезы об адекватности
  F = (R2 / (1 - R2)) * (N - 2)
  f_crit = qf(1 - ALPHA, 1, N - 2)
  cat('Модель адекватна: ', F >=  f_crit, '\n')
  
  # 7. Повторяем рассчеты через lm()
  reg = lm(formula = y ~ x, data = xy)
  summary(reg) # выводим итоги регрессионного анализа
  cat('Вывод: все полученный результаты равны\n')
  
  # 8. Строим график
  # Строим функцию регрессии
  xspline(x, b_stat*x + a_stat, border = "red", lty = 5)
  
  # Интервал предсказания
  pred_int_up = a_stat + b_stat * x + 
    t_crit * S * sqrt(1 + (1 / N) + ((x - mean(x)) ** 2) / sum((x - mean(x)) ** 2) )
  
  pred_int_down = a_stat + b_stat * x - 
    t_crit * S * sqrt(1 + (1 / N) + ((x - mean(x)) ** 2) / sum((x - mean(x)) ** 2) )
  
  # Строим доверительный интервал для функции регрессии 
  xspline(x, pred_int_up, border = "green", lty = 5)
  xspline(x, pred_int_down, border = "green", lty = 5)
  
  
  # Доверительный интервал для функции регрессии
  conf_int_reg_fun_up = a_stat + b_stat * x + 
    t_crit * S * sqrt( (1 / N) + ((x - mean(x)) ** 2) / sum((x - mean(x)) ** 2) )
  
  conf_int_reg_fun_down = a_stat + b_stat * x - 
    t_crit * S * sqrt( (1 / N) + ((x - mean(x)) ** 2) / sum((x - mean(x)) ** 2) )
  
  # Строим доверительный интервал для функции регрессии 
  xspline(x, conf_int_reg_fun_up, border = "blue", lty = 5)
  xspline(x, conf_int_reg_fun_down, border = "blue", lty = 5)
}

main()




