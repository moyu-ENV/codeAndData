options nocenter;
/*IMPORT DATA*/
PROC IMPORT DATAFILE= "g:\data\LANDSAT.XLS" 
OUT=data_1 
DBMS=XLS
replace;
SHEET='1984';
GETNAMES=YES;
RUN;
PROC PRINT DATA=DATA_1;
RUN;
data data; 
set DATA_1;
array Locations{15} $ L1-L15 ('Barataria' 'BretonSound' 'Pontchartrain' 'Terrebonne' 'Barataria' 'Pontchartrain' 'Terrebonne' 'Barataria' 'BretonSound' 'Pontchartrain' 'Terrebonne' 'Barataria' 'BretonSound' 'Pontchartrain' 'Terrebonne');
array Marshtypes{15} $ M1-M15 ('BrackishMarsh' 'BrackishMarsh' 'BrackishMarsh' 'BrackishMarsh' 'FreshwaterMarsh' 'FreshwaterMarsh' 'FreshwaterMarsh' 'IntermediateMarsh' 'IntermediateMarsh' 'IntermediateMarsh' 'IntermediateMarsh' 'SalineMarsh' 'SalineMarsh' 'SalineMarsh' 'SalineMarsh');
array units{15} unit1-unit15; 
do unit=1 to 15;
Marshtype = MarshTypes(unit);
Location = Locations(unit);
NDVI=units(unit);
output;
end;
keep year doy unit Marshtype Location NDVI;
run;
proc print data=data;
run;
/*end of import data*/


/*GAUSS distribution*/
ods select ConvergenceStatus Anova ParameterEstimates;
proc sort data=data;
by unit;
run;
proc nlin data=data maxiter=2000 method=newton outest=G_out;
parms b=0.09 A=0.3 mu=0.2 sigma1=0.2 sigma2=0.2; 
model NDVI = b + A *(DOY<1000*mu)*exp(-((DOY-1000*mu)/(100*sigma1))**2/2)+A *(DOY>1000*mu)*exp(-((DOY-1000*mu)/(100*sigma2))**2/2);
by unit;
run;
proc print data=g_out;
run;  

data byunit; 
input unit marshtype$ location$ b A mu sigma1 sigma2 MSe;
datalines;
1 Brackish Baratari 0.18589 0.21 0.097 0.000 0.14   0.00205
2 Brackish BretonSo 0.28490 0.20 0.232 0.019 0.03   0.00156
3 Brackish Pontchar 0.37603 0.09 0.240 -0.016 0.04  0.00193
4 Brackish Terrebon 0.30321 0.09 0.233 -0.006 0.03  0.00311
5 Freshwat Baratari 0.01693 0.58 0.234 0.206 0.08   0.00186
6 Freshwat Pontchar 0.11296 0.50 0.195 0.129 0.18   0.00318
7 Freshwat Terrebon 0.40843 0.21 0.239 0.033 0.03   0.00514
8 Intermed Baratari 0.29956 0.17 0.266 0.147 0.02   0.00254
9 Intermed BretonSo 0.07945 0.05 0.195 0.033 0.15   0.000072
10 Intermed Pontchar 0.45360 0.13 0.182 0.013 0.03  0.0156
11 Intermed Terrebon 0.39088 0.17 0.286 0.017 0.00  0.00111
12 SalineMa Baratari 0.22980 0.11 0.189 0.066 -0.01  0.0137
13 SalineMa BretonSo 0.32974 -0.06 0.203 0.033 0.05  0.00290
14 SalineMa Pontchar 0.23962 0.17 0.164 0.010 0.01   0.00411
15 SalineMa Terrebon 0.28200 -0.01 0.241 0.042 0.03  0.00787
RUN;
proc print data=byunit;
run;
PROC MEANS DATA = byunit MEAN VAR;
VAR b A mu sigma1 sigma2 MSe;
RUN;
PROC MEANS DATA = byunit MEAN VAR;
VAR b A mu sigma1 sigma2 MSe;
BY marshtype;
RUN; 
proc discrim data= byunit pcov;
class marshtype;
var b a mu sigma1 sigma2;
run;

proc sort data=data;
by unit;
run;
Proc nlmixed data=data method=firo;
if (marshtype='Freshwat') then do;zf=1;zi=0;zb=0;zs=0;end;
if (marshtype='Intermed') then do;zf=0;zi=1;zb=0;zs=0;end;
if (marshtype='Brackish') then do;zf=0;zi=0;zb=1;zs=0;end;
if (marshtype='SalineMa') then do;zf=0;zi=0;zb=0;zs=1;end;
parms b_b=0.2875075 A_b=0.1475000 mu_b=0.2005000 sigma1_b=-0.000750000 sigma2_b=0.0600000
      b_f=0.1794400 A_f=0.4300000 mu_f=0.2226667 sigma1_f=0.1226667 sigma2_f=0.0966667
      b_i=0.3058725 A_i=0.1300000 mu_i=0.2322500 sigma1_i=0.0525000 sigma2_i=0.0500000
      b_s=0.2702900 A_s=0.0525000 mu_s=0.1992500 sigma1_s=0.0377500 sigma2_s=0.0200000
      /*s2e=0.0046333*/; 
B=(B_f*zf) + (B_i*zi) + (B_b*zb) + (B_s*zs) + u_b;
A=(A_f*zf) + (A_i*zi) + (A_b*zb) + (A_s*zs) + u_A;
mu=(mu_f*zf) + (mu_i*zi) + (mu_b*zb) + (mu_s*zs) + u_mu;
sigma1=(sigma1_f*zf) + (sigma1_i*zi) + (sigma1_b*zb) + (sigma1_s*zs) + u_sigma1;
sigma2=(sigma2_f*zf) + (sigma2_i*zi) + (sigma2_b*zb) + (sigma2_s*zs) + u_sigma2;
pred =b + A *(DOY<1000*mu)*exp(-((DOY-1000*mu)/(1000*sigma1))**2/2)+A *(DOY>1000*mu)*exp(-((DOY-1000*mu)/(1000*sigma2))**2/2);
model NDVI ~ normal(pred,0.0044488);
random u_b u_a u_mu u_sigma1 u_sigma2 ~normal([0,0,0,0,0],[0.0171172208,
-.0076344659,0.0120318182,
0.0023413566,-.0012788636,0.0024115606,
-.0037832811,0.0034077273,0.0003560833,0.0026741061,
-.0049660000,0.0005636364,-.0018003030,0.0001960606,0.0032787879]) 
subject=unit;

contrast 'b0 F VS I' b_f, b_i;
contrast 'b0 F VS B' b_f, b_b;
contrast 'b0 F VS S' b_f, b_s;
contrast 'b0 I VS B' b_i, b_b;
contrast 'b0 I VS S' b_i, b_s;
contrast 'b0 B VS S' b_b, b_s;

contrast 'A F VS I' A_f, A_i;
contrast 'A F VS B' A_f, A_b;
contrast 'A F VS S' A_f, A_s;
contrast 'A I VS B' A_i, A_b;
contrast 'A I VS S' A_i, A_s;
contrast 'A B VS S' A_b, A_s;

contrast 'mu F VS I' mu_f, mu_i;
contrast 'mu F VS B' mu_f, mu_b;
contrast 'mu F VS S' mu_f, mu_s;
contrast 'mu I VS B' mu_i, mu_b;
contrast 'mu I VS S' mu_i, mu_s;
contrast 'mu B VS S' mu_b, mu_s;

contrast 'sigma1 F VS I' sigma1_f, sigma1_i;
contrast 'sigma1 F VS B' sigma1_f, sigma1_b;
contrast 'sigma1 F VS S' sigma1_f, sigma1_s;
contrast 'sigma1 I VS B' sigma1_i, sigma1_b;
contrast 'sigma1 I VS S' sigma1_i, sigma1_s;
contrast 'sigma1 B VS S' sigma1_b, sigma1_s;

contrast 'sigma2 F VS I' sigma2_f, sigma2_i;
contrast 'sigma2 F VS B' sigma2_f, sigma2_b;
contrast 'sigma2 F VS S' sigma2_f, sigma2_s;
contrast 'sigma2 I VS B' sigma2_i, sigma2_b;
contrast 'sigma2 I VS S' sigma2_i, sigma2_s;
contrast 'sigma2 B VS S' sigma2_b, sigma2_s;

predict pred out=out_G;
run;

proc sort data=out_G;
by  marshtype location;
run;
proc print data=out_G;
run;
 
proc sort data=out_G;
by doy marshtype;
run;
proc means data=out_G;
var pred;
by DOY marshtype;
output out=out_G_mean mean=mean stderr=stderr;
run;
proc print data=out_G_mean;
run;
data G;
merge out_G out_G_mean;
by doy marshtype;
run;
proc print data=G;
run;
/*GAUSS distribution*/

