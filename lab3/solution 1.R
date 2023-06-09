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
  
  y=a+b*x+eps # ��������� ��������� ����������
  xy=data.frame(cbind(x,y)) # ���������� � �������
  plot(y~x,data=xy,main='��������� Y �� X')
  
  # 1. ��� ������ ����������
  b_stat = ( mean(x*y) - (mean(x) * mean(y)) ) / ( mean(x ** 2)  - ( mean(x) ** 2 ) )
  a_stat = mean(y) - b_stat * mean(x)
  
  cat('������ ��������� a: ', a_stat, '\n')
  cat('������ ��������� b: ', b_stat, '\n')
  
  # 2. ��������� ���������� � ������� ����������
  S2 = (1 / (N-2) ) * sum( (y - a_stat - b_stat*x) ** 2)
  Sb2 = S2 / sum((x - mean(x)) ** 2)
  Sa2 = S2 * sum(x ** 2) / (  N * sum((x - mean(x)) ** 2) )
  
  
  cat('������ ��������� ������ ��������� a: ', Sa2, ' ( Std. Error: ', sqrt(Sa2), ')\n')
  cat('������ ��������� ������ ��������� b: ', Sb2, ' ( Std. Error: ', sqrt(Sb2), ')\n')
  
  # 3. ������ ������������� ���������
  
  # std. errors
  S = sqrt(S2)
  Sa = sqrt(Sa2)
  Sb = sqrt(Sb2)
  
  a2 = (a_stat - ( qt(ALPHA / 2, N - 2) * Sa) )
  a1 = (a_stat + ( qt(ALPHA / 2, N - 2) * Sa) )
  
  b2 = (b_stat - ( qt(ALPHA / 2, N - 2) * Sb) )
  b1 = (b_stat + ( qt(ALPHA / 2, N - 2) * Sb) )
  
  cat('������������� ��������� ����������: ', ALPHA*100 / 2, '%  / ', 100-ALPHA*100 / 2, '%\n')
  cat('                              a: ', '( ', a1, '; ',
      a2, ' )', '\n')
  
  cat('                              b: ', '( ', b1, '; ',
      b2, ' )', '\n')
  
  
  #confint(reg, level=1 - ALPHA)
  
  # 4. �������� ���������� ������������� ���������
  ta = a_stat / Sa
  tb = b_stat / Sb
  t_crit = qt(1 - (ALPHA / 2), N - 2)
  
  cat('t-value ��������� a �����', ta ,'\n')
  cat('t-value ��������� b �����', tb ,'\n')
  cat('t-critical �����', t_crit ,'\n')
  
  # ���� ���� �������� ���� ������ ����� - �������� ��������� 
  # ���� ��� ����� �� �������-�����
  # ���� ��� ����� - �� ������-�����
  
  # a ������ (������ ���������)
  if (sign(a1) != sign(a2)){
    cat('����������� a ��������\n')
  } else if (sign(b1) != sign(b2)){
    cat('����������� b ��������\n')
  } else{
    cat('����������� a ������: ', abs(ta) >=  t_crit,'\n')  #���� ����� ��� �����
    cat('����������� b ������: ', abs(tb) >=  t_crit, '\n')  #y ������� �� � 
  }
  
  # 5. ����. ������������ ������
  y_pred = a_stat + b_stat * x
  
  RSS = sum((y - y_pred) ** 2)
  TSS = sum((y - mean(y)) ** 2)
  ESS = sum((y_pred - mean(y)) ** 2)
  
  R2 = ESS / TSS
  
  cat('����������� ������������ ������ R-squared �����: ', R2, '\n')
  
  if (R2 == 0){
    cat('����������� ������� �������. ������������� ���������
      ���������� ��������� ��������������� �����\n')
  } else if (R2 == 1){
    cat('����������� ������� ����. ������������� ���������
      ���������� ��������� ��������������� �������(�������������� �����������)\n')
  } else{
    cat(R2 * 100, '% ��������� ��������� ���������� ����������� 
      ���������� ����������� ����������\n')
  }
  
  # 6. �������� �������� �� ������������
  F = (R2 / (1 - R2)) * (N - 2)
  f_crit = qf(1 - ALPHA, 1, N - 2)
  cat('������ ���������: ', F >=  f_crit, '\n')
  
  # 7. ��������� �������� ����� lm()
  reg = lm(formula = y ~ x, data = xy)
  summary(reg) # ������� ����� �������������� �������
  cat('�����: ��� ���������� ���������� �����\n')
  
  # 8. ������ ������
  # ������ ������� ���������
  xspline(x, b_stat*x + a_stat, border = "red", lty = 5)
  
  # �������� ������������
  pred_int_up = a_stat + b_stat * x + 
    t_crit * S * sqrt(1 + (1 / N) + ((x - mean(x)) ** 2) / sum((x - mean(x)) ** 2) )
  
  pred_int_down = a_stat + b_stat * x - 
    t_crit * S * sqrt(1 + (1 / N) + ((x - mean(x)) ** 2) / sum((x - mean(x)) ** 2) )
  
  # ������ ������������� �������� ��� ������� ��������� 
  xspline(x, pred_int_up, border = "green", lty = 5)
  xspline(x, pred_int_down, border = "green", lty = 5)
  
  
  # ������������� �������� ��� ������� ���������
  conf_int_reg_fun_up = a_stat + b_stat * x + 
    t_crit * S * sqrt( (1 / N) + ((x - mean(x)) ** 2) / sum((x - mean(x)) ** 2) )
  
  conf_int_reg_fun_down = a_stat + b_stat * x - 
    t_crit * S * sqrt( (1 / N) + ((x - mean(x)) ** 2) / sum((x - mean(x)) ** 2) )
  
  # ������ ������������� �������� ��� ������� ��������� 
  xspline(x, conf_int_reg_fun_up, border = "blue", lty = 5)
  xspline(x, conf_int_reg_fun_down, border = "blue", lty = 5)
}

main()




