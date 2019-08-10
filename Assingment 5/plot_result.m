% Your input file should have following structure
% N_1 avg_method_1 err_method_1 avg_method_2 err_method_2 ...
% N_2 avg_method_1 err_method_1 avg_method_2 err_method_2 ...
%  ...
name_input_file = 'data.dat'; % Replace with the name of your input file
methods = {"Single Step","Two Step"}; % Replace with the correct method names
DATA = dlmread(name_input_file);
if(size(DATA,2) ~= length(methods)*2+1)
    disp('Invalid number of columns \n')
end
N = DATA(:,1);
figure
set(gca, 'YScale', 'log')
hold on
avg = 1:1:size(N,1);
err = 1:1:size(N,1);
for i=1:length(methods)
  for j=1:size(N,1)
    avg(j) = DATA(j,i*2);%megaflops(size(N,1))/(mean(DATA(j,i*3-1:i*3+1)));5
    err(j) = DATA(j,1+(i*2));%megaflops(size(N,1))/(max(DATA(j,i*3-1:i*3+1)));
  end
  err_min = -err;
  err_plus = err;
  errorbar(N,avg,err_min,err_plus); %%%%%%
end
Theo1 = N;
Theo2 = N;
for i=1:length(N)
    Theo1(i) = theocomplx_one_step(5e-10,N(i));
    Theo2(i) = theocomplx_two_step(5e-10,N(i));
end
plot(N,Theo1)
plot(N,Theo2)
N_width = N(end)-N(1);
Y_heigth = max(max(DATA(:,[2 4])))-min(min(DATA(:,[2 4])));
xlim([N(1)-0.005*N_width,N(end)+0.05*N_width])
%ylim([min(min(min(DATA(:,[2 4])))-0.05*Y_heigth,0),max(max(DATA(:,[2 4])))+0.05*Y_heigth]);
ylim([1e-7,max(max(DATA(:,[2 4])))+0.05*Y_heigth]);
legend([methods "Theor. 1 step" "Theor. 2 step"],'Location','southeast');
xlabel("N");
ylabel("Execution time (s)");
set(gcf,'units','points','position',[100,50,800,600])
function [t] = theocomplx_one_step(const,n)
%UNTITLED Summary of this function goes here
%   Detailed explanation goes here
t = const * (2*n^3 + n);
end

function [t] = theocomplx_two_step(const,n)
%UNTITLED Summary of this function goes here
%   Detailed explanation goes here
t = const * (4*n^2);
end