main <- function(){
  ##################################################
  ##################�������� ������#################
  ##################################################
  
  # 1.��������� �������
  #CONSTS
  N = 30
  FUNC_NAME = '�������� ������' 
  a=3
  b=20
  sigma=1
  ALPHA = 0.025
  
  i=1:N
  x=0+1*i
  xinv = x ** -1
  
  eps=rnorm(length(xinv),0,sigma**2)
  
  # 2.������ �������� �����������
  y = a + b * xinv + eps
  xy=data.frame(cbind(x,y))
  plot(y~x,data=xy,main=paste(FUNC_NAME,'.  ��� ������������', sep=''))
  
  #y = a + b * lnx + eps
  
  # 3. �������� ������������ � ������ ������
  # �������� ���������� (���������� ������������)
  # ������ ��� ������� �� ���������� (�� �� �������� �� �������)
  cat('������������: ������ ��� ������� �� ���������� (�� �� �������� �� �������) \n')
  
  # lny = lna + b * x + lneps
  df_xinv_y=data.frame(cbind(xinv,y)) 
  plot(y~xinv,data=df_xinv_y,main=paste(FUNC_NAME,'.  C �������������', sep=''))
  func = lm(formula = y ~ xinv, data = df_xinv_y)
  summary = summary(func)
  
  # 4. ������� ���-������ ���������� ������.
  a_stat = summary$coefficients[ , 1][1]
  b_stat = summary$coefficients[ , 1][2]
  
  cat('������ ��������� a: ', a_stat, '\n')
  cat('������ ��������� b: ', b_stat, '\n')
  
  # 5. ������� ��������� ���������� � ������ ����������.
  # ��������� ����������
  S2 = (1 / (N-2) ) * sum( (y - a_stat - b_stat*xinv) ** 2)
  S = sqrt(S2)
  
  # ��������� ������ ����������
  Sa = summary$coefficients[ , 2][1]
  Sa2 = Sa ** 2
  Sb = summary$coefficients[ , 2][2]
  Sb2 = Sb ** 2
  cat('������ ��������� ������ ��������� a: ', Sa2, ' ( Std. Error: ', sqrt(Sa2), ')\n')
  cat('������ ��������� ������ ��������� b: ', Sb2, ' ( Std. Error: ', sqrt(Sb2), ')\n')
  
  # 6. ������ ������������� ��������� ��� ����������� ����������
  
  a2 = (a_stat - ( qt(ALPHA / 2, N - 2) * Sa) )
  a1 = (a_stat + ( qt(ALPHA / 2, N - 2) * Sa) )
  
  b2 = (b_stat - ( qt(ALPHA / 2, N - 2) * Sb) )
  b1 = (b_stat + ( qt(ALPHA / 2, N - 2) * Sb) )
  
  cat('������������� ��������� ����������: ', ALPHA*100 / 2, '%  / ', 100-ALPHA*100 / 2, '%\n')
  cat('                              a: ', '( ', a1, '; ',
      a2, ' )', '\n')
  
  cat('                              b: ', '( ', b1, '; ',
      b2, ' )', '\n')
  
  # 7. ��������� �������� � ���������� ������������� ���������.
  ta = a_stat / Sa
  tb = b_stat / Sb
  t_crit = qt(1 - (ALPHA / 2), N - 2)
  
  cat('t-value ��������� a �����', ta ,'\n')
  cat('t-value ��������� b �����', tb ,'\n')
  cat('t-critical �����', t_crit ,'\n')
  
  if (sign(a1) != sign(a2) | sign(b1) != sign(b2)){
    if (sign(a1) != sign(a2)){
      cat('����������� a ��������\n')
    }
    else {
      cat('����������� a ������\n')
    }
    if (sign(b1) != sign(b2)){
      cat('����������� b ��������\n')
    } else {
      cat('����������� b ������\n')
    }
  } else {
    cat('����������� a ������: ', abs(ta) >=  t_crit,'\n')
    cat('����������� b ������: ', abs(tb) >=  t_crit, '\n')
  }
  
  # 8. ������� ����������� ������������ ������: Multiple-R2-squared
  R2 = summary$r.squared
  
  cat('����������� ������������ ������ R-squared �����: ', R2, ', �����: \n')
  
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
  
  # 9. ��������� �������� �� ������������ ������.
  F_stat = summary$fstatistic[1]
  cat('F-stitistic �����: ', F_stat, '\n')
  f_crit = qf(1 - ALPHA, 1, N - 2)
  cat('���� F-stitistic ������, �� ������ ���������. F-crititcal �����: ', f_crit, '\n')
  cat('������ ���������: ', F_stat >=  f_crit, '\n')
  
  # 10. ������ ������������� ���������
  
  # ������ ������� ���������
  xspline(xinv, b_stat*xinv + a_stat, border = "red", lty = 5)
  
  # �������� ������������
  pred_int_up = a_stat + b_stat * xinv + 
    t_crit * S * sqrt(1 + (1 / N) + ((xinv - mean(xinv)) ** 2) / sum((xinv - mean(xinv)) ** 2) )
  pred_int_down = a_stat + b_stat * xinv - 
    t_crit * S * sqrt(1 + (1 / N) + ((xinv - mean(xinv)) ** 2) / sum((xinv - mean(xinv)) ** 2) )
  
  # ������ ������������� �������� ��� ������� ��������� 
  xspline(xinv, pred_int_up, border = "green", lty = 5)
  xspline(xinv, pred_int_down, border = "green", lty = 5)
  
  
  # ������������� �������� ��� ������� ���������
  conf_int_reg_fun_up = a_stat + b_stat * xinv + 
    t_crit * S * sqrt( (1 / N) + ((xinv - mean(xinv)) ** 2) / sum((xinv - mean(xinv)) ** 2) )
  conf_int_reg_fun_down = a_stat + b_stat * xinv - 
    t_crit * S * sqrt( (1 / N) + ((xinv - mean(xinv)) ** 2) / sum((xinv - mean(xinv)) ** 2) )
  
  # ������ ������������� �������� ��� ������� ��������� 
  xspline(xinv, conf_int_reg_fun_up, border = "blue", lty = 5)
  xspline(xinv, conf_int_reg_fun_down, border = "blue", lty = 5) 
}

main()

