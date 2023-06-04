main <- function(){
  ##################################################
  #############ЭКСПОТЕНЦИАЛЬНАЯ МОДЕЛЬ##############
  ##################################################
  
  # 1.Генерация выборки
  #CONSTS
  N = 100
  FUNC_NAME = 'Экспотенциальная модель' 
  a=2
  b=1.5
  sigma=1
  ALPHA = 0.025
  
  i=1:N
  x=1+0.03*i
  
  ln_eps=rnorm(N,0,sigma**2)
  eps = exp(ln_eps)
  
  # 2.Строим диграммы рассеивания
  y = a * exp(b * x) * eps 
  xy=data.frame(cbind(x,y))
  plot(y~x,data=xy,main=paste(FUNC_NAME,'.  Без линеаризации', sep=''))
  
  #ln_y = ln_a + b * x + ln eps
  
  # 3. Проводим линеаризацию и строим график
  lny = log(y)
  lna = log(a)
  lneps = ln_eps
  
  # lny = lna + b * x + lneps
  df_x_lny=data.frame(cbind(x,lny)) 
  plot(lny~x,data=df_x_lny,main=paste(FUNC_NAME,'.  C линеаризацией', sep=''))
  func = lm(formula = lny ~ x, data = df_x_lny)
  summary = summary(func)
  
  # 4. Находим МНК-оценки параметров модели.
  lna_stat = summary$coefficients[ , 1][1]
  b_stat = summary$coefficients[ , 1][2]
  a_stat = exp(lna_stat)
  
  cat('Оценка параметра a: ', a_stat, '\n')
  cat('Оценка параметра b: ', b_stat, '\n')
  
  # 5. Находим дисперсии наблюдений и оценок параметров.
  # Дисперсия наблюдений
  S2 = (1 / (N-2) ) * sum( (lny - lna_stat - b_stat*x) ** 2)
  S = sqrt(S2)
  
  # Дисперсии оценко параметров
  Slna = summary$coefficients[ , 2][1]
  Sa = exp(Slna)
  Sa2 = Sa ** 2
  Sb = summary$coefficients[ , 2][2]
  Sb2 = Sb ** 2
  cat('Оценка дисперсии оценки параметра a: ', Sa2, ' ( Std. Error: ', sqrt(Sa2), ')\n')
  cat('Оценка дисперсии оценки параметра b: ', Sb2, ' ( Std. Error: ', sqrt(Sb2), ')\n')
  
  
  # 6. Строим доверительные интервалы для неизвестных параметров
  
  a2 = (a_stat - ( qt(ALPHA / 2, N - 2) * Sa) )
  a1 = (a_stat + ( qt(ALPHA / 2, N - 2) * Sa) )
  
  b2 = (b_stat - ( qt(ALPHA / 2, N - 2) * Sb) )
  b1 = (b_stat + ( qt(ALPHA / 2, N - 2) * Sb) )
  
  cat('Доверительные интервалы параметров: ', ALPHA*100 / 2, '%  / ', 100-ALPHA*100 / 2, '%\n')
  cat('                              a: ', '( ', a1, '; ',
      a2, ' )', '\n')
  
  cat('                              b: ', '( ', b1, '; ',
      b2, ' )', '\n')
  
  # 7. Проверяем гипотезы о значимости коэффициентов регрессии.
  ta = a_stat / Sa
  tb = b_stat / Sb
  t_crit = qt(1 - (ALPHA / 2), N - 2)
  
  cat('t-value параметра a равно', ta ,'\n')
  cat('t-value параметра b равно', tb ,'\n')
  cat('t-critical равно', t_crit ,'\n')
  
  if (sign(a1) != sign(a2) | sign(b1) != sign(b2)){
    if (sign(a1) != sign(a2)){
      cat('Коэффициент a незначим\n')
    }
    else {
      cat('Коэффициент a значим\n')
    }
    if (sign(b1) != sign(b2)){
      cat('Коэффициент b незначим\n')
    } else {
      cat('Коэффициент b значим\n')
    }
  } else {
    cat('Коэффициент a значим: ', abs(ta) >=  t_crit,'\n')
    cat('Коэффициент b значим: ', abs(tb) >=  t_crit, '\n')
  }

  # 8. Находим коэффициент детерминации модели: Multiple-R2-squared
  R2 = summary$r.squared

  cat('Коэффициент детерминации модели R-squared равен: ', R2, ', вывод: \n')

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
  
  # 9. Проверяем гипотезу об адекватности модели.
  F_stat = summary$fstatistic[1]
  cat('F-stitistic равно: ', F_stat, '\n')
  f_crit = qf(1 - ALPHA, 1, N - 2)
  cat('Если F-stitistic больше, то модель адекватна. F-crititcal равно: ', f_crit, '\n')
  cat('Модель адекватна: ', F_stat >=  f_crit, '\n')
  
  # 10. Строим доверительные интервалы
  
  # Строим функцию регрессии
  xspline(x, b_stat*x + lna_stat, border = "red", lty = 5)
  
  # Интервал предсказания
  pred_int_up = lna_stat + b_stat * x + 
    t_crit * S * sqrt(1 + (1 / N) + ((x - mean(x)) ** 2) / sum((x - mean(x)) ** 2) )
  pred_int_down = lna_stat + b_stat * x - 
    t_crit * S * sqrt(1 + (1 / N) + ((x - mean(x)) ** 2) / sum((x - mean(x)) ** 2) )
  
  # Строим доверительный интервал для функции регрессии 
  xspline(x, pred_int_up, border = "green", lty = 5)
  xspline(x, pred_int_down, border = "green", lty = 5)
  
  
  # Доверительный интервал для функции регрессии
  conf_int_reg_fun_up = lna_stat + b_stat * x + 
    t_crit * S * sqrt( (1 / N) + ((x - mean(x)) ** 2) / sum((x - mean(x)) ** 2) )
  conf_int_reg_fun_down = lna_stat + b_stat * x - 
    t_crit * S * sqrt( (1 / N) + ((x - mean(x)) ** 2) / sum((x - mean(x)) ** 2) )
  
  # Строим доверительный интервал для функции регрессии 
  xspline(x, conf_int_reg_fun_up, border = "blue", lty = 5)
  xspline(x, conf_int_reg_fun_down, border = "blue", lty = 5) 
}

main()

