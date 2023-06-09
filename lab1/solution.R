# 1. ���������� �������
x_1=rnorm(100,20, 7) 
x_2=rnorm(100,35, 7) 
x_3=rnorm(100,30, 7) 

sum_length = length(x_1) + length(x_2) + length(x_3)

# 2. ������� �������
## �����
general_average = sum(x_1, x_2, x_3) / sum_length
## �������
x_1_average = mean(x_1)
x_2_average = mean(x_2)
x_3_average = mean(x_3)

# 3. ������ ������
## �������� y ���������� �����
y_1 = c(1:length(x_1))
y_2 = c(length(x_1)+1:length(x_2))
y_3 = c(length(x_2)+1:length(x_2)+length(x_3))
## ���������� �����
plot(y_1, x_1, pch = 19, col = "red", ylim=c(0,max(x_1, x_2, x_3)), xlim=c(0,sum_length),
     xlab="number of points", ylab="values")
points(y_2, x_2, col = "green", pch = 19)
points(y_3, x_3, col = "blue", pch = 19)
## ����������� ����� �������
xspline(c(0, length(x_1)),c(x_1_average, x_1_average), border = "red", lty = 5)
xspline(c(length(x_1)+1, length(x_1)+length(x_2)),c(x_2_average, x_2_average), border = "green", lty = 5)
xspline(c(length(x_1)+length(x_2)+1, length(x_1)+length(x_2)+length(x_3)),c(x_3_average, x_3_average), border = "blue", lty = 5)
abline(h = general_average, col = "black", lty = 5)


# 4. ������� ���������
## ������� ����� ���������
total_variance =  
  (sum((x_1 - general_average)^2)
  + sum((x_2 - general_average)^2)
  + sum((x_3 - general_average)^2)) /
  (length(x_1) 
   + length(x_2) 
   + length(x_3))

## ������� ��������������� ���������
intragroup_variance_1 = sum((x_1 - x_1_average)^2) / length(x_1)
intragroup_variance_2 = sum((x_2 - x_2_average)^2) / length(x_2)
intragroup_variance_3 = sum((x_3 - x_3_average)^2) / length(x_3)

## ������� �������������� ���������
average_intragroup_variance = 
  (intragroup_variance_1*length(x_1)
   + intragroup_variance_2*length(x_2)
   + intragroup_variance_3*length(x_3))  / sum_length

## ������������ ��������
intergroup_variance = 
  ((x_1_average - general_average)^2 * length(x_1)
  +(x_2_average - general_average)^2 * length(x_2)
  +(x_3_average - general_average)^2 * length(x_3)) / sum_length

# 5. ��������� ������� �������� ���������
potential_total_variance = average_intragroup_variance + intergroup_variance
## TRUE ���� ������� �����������, FALSE ���� ���
setequal(round(potential_total_variance, digits = 13), round(total_variance, digits = 13))

# 6. ������� �������������� ���������
correlation_ratio = sqrt(intergroup_variance / total_variance)

# 7. ������� ����������� ������������
determination_coef = intergroup_variance / total_variance * 100

# 8. ��������� ���� ������� ������������� ������� �� ����� �������
if ( correlation_ratio < 0) {
  cat("������ � ���������!")
} else if (correlation_ratio == 0.0) {
  cat("����� �����������")
} else if (0 < correlation_ratio && correlation_ratio < 0.2) {
  cat("����� ����� ������")
} else if (0.2 <= correlation_ratio && correlation_ratio < 0.3) {
  cat("����� ������")
} else if (0.3 <= correlation_ratio && correlation_ratio < 0.5) {
  cat("����� ���������")
} else if (0.5 <= correlation_ratio && correlation_ratio < 0.7) {
  cat("����� ��������")
} else if (0.7 <= correlation_ratio && correlation_ratio < 0.9) {
  cat("����� �������")
} else if (0.9 <= correlation_ratio && correlation_ratio < 1.0) {
  cat("����� ����� �������")
} else if (correlation_ratio == 1.0) {
  cat("����� ��������������")
} else {
  cat("������ � ���������!")
}
cat("�������������� ��������� �����: ", correlation_ratio)

