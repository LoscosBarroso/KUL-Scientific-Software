% Your input file should have following structure
% N_1 avg_method_1 min_method_1 max_method_1 avg_method_2 min_method_2 max_method_2 ...
% N_2 avg_method_1 min_method_1 max_method_1 avg_method_2 min_method_2 max_method_2 ...
%  ...
name_input_file = 'test.dat'; % Replace with the name of your input file
methods = {"Loop based","Dot product based","BLAS","Block","Matmul"}; % Replace with the correct method names
DATA = dlmread(name_input_file);
if(size(DATA,2) ~= length(methods)*3+1)
    disp('Invalid number of columns \n')
end
N = DATA(:,1);
figure
%set(gca, 'YScale', 'log')
set(gca, 'YScale')
hold on
avg = 1:1:size(N,1);
min_v = 1:1:size(N,1);
max_v = 1:1:size(N,1);
for i=1:length(methods)
  for j=1:size(N,1)
    avg(j) = megaflops(j)/(mean(DATA(j,i*3-1:i*3+1)));
    min_v(j) = megaflops(j)/(max(DATA(j,i*3-1:i*3+1)));
    max_v(j) = megaflops(j)/(min(DATA(j,i*3-1:i*3+1)));
  end
  err_min = avg-min_v;
  err_plus = max_v-avg;
  errorbar(N,avg,err_min,err_plus);
end
N_width = N(end)-N(1);
Y_heigth = max(max(max_v))-min(min(min_v));
xlim([N(1)-0.05*N_width,N(end)+0.05*N_width])
%ylim([min(min(min(min_v))-0.05*Y_heigth,0),max(max(max_v))+0.05*Y_heigth]);
ylim([0,3.5]);
legend(methods,'Location','northwest');
xlabel("N");
ylabel("MFlops/s")
set(gcf,'units','points','position',[100,50,800,600])
function [megaf] = megaflops(n)
%Megaflops needed to compute a matrix product between two NxN matrices
%For each element of C (n*n), n multiplications and (n-1) additions are
%needed, yielding (n*n)*(2n-1) floating point operations
megaf = ((n*n)*((2*n)-1))/1000000.0;
end