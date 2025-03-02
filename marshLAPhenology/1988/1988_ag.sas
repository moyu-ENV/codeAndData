options nocenter;
/*IMPORT DATA*/
PROC IMPORT DATAFILE= "g:\data\LANDSAT.XLS" 
OUT=data_1 
DBMS=XLS
replace;
SHEET='19882';
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
parms b=0.05 A=0.4 mu=0.19 sigma1=0.026 sigma2=0.025; 
model NDVI = b + A *(DOY<1000*mu)*exp(-((DOY-1000*mu)/(1000*sigma1))**2/2)+A *(DOY>1000*mu)*exp(-((DOY-1000*mu)/(1000*sigma2))**2/2);
by unit;
run;
proc print data=g_out;
run;  

data byunit; 
input unit marshtype$ location$ b A mu sigma1 sigma2 MSe;
datalines;
1 Brackish Baratari 0.42026 0.10470 0.19000 0.026000 0.025000 0.00558
2 Brackish BretonSo 0.37083 0.28071 0.19473 0.058169 0.061327 0.00115
3 Brackish Pontchar 0.43141 0.14089 0.19000 0.026000 0.025000 0.00629
4 Brackish Terrebon 0.39339 0.33252 0.21631 0.055799 0.029006 0.00473
5 Freshwat Baratari 0.50267 0.28146 0.19000 0.026000 0.025000 0.0185
6 Freshwat Pontchar 0.50113 0.24025 0.19000 0.026000 0.025000 0.0154
7 Freshwat Terrebon 0.52526 0.20796 0.19000 0.026000 0.025000 0.0138
8 Intermed Baratari 0.40782 0.45979 0.21702 0.055040 0.029308 0.00473
9 Intermed BretonSo 0.29650 0.33974 0.17310 0.035651 0.096566 0.00334
10 Intermed Pontchar 0.46511 0.33063 0.19000 0.026000 0.025000 0.0147
11 Intermed Terrebon 0.46372 0.14558 0.19000 0.026000 0.025000 0.00940
12 SalineMa Baratari 0.36478 0.04699 0.19000 0.026000 0.025000 0.00343
13 SalineMa BretonSo 0.35798 0.10988 0.14601 0.031593 0.060278 0.000801
14 SalineMa Pontchar 0.32442 0.25800 0.21635 0.054386 0.017540 0.000759
15 SalineMa Terrebon 0.35332 0.23056 0.21171 0.049682 0.028822 0.00394
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
parms b_b=0.4039725 A_b=0.2147050 mu_b=0.1977600 sigma1_b=0.0414920 sigma2_b=0.0350833
      b_f=0.5096867 A_f=0.2432233 mu_f=0.2432233 sigma1_f=0.0260000 sigma2_f=0.0250000
      b_i=0.4082875 A_i=0.3189350 mu_i=0.1925300 sigma1_i=0.0356728 sigma2_i=0.0439685
      b_s=0.3501250 A_s=0.1613575 mu_s=0.1910175 sigma1_s=0.0404153 sigma2_s=0.0329100
      /*s2e=0.0046333*/; 
B=(B_f*zf) + (B_i*zi) + (B_b*zb) + (B_s*zs) + u_b;
A=(A_f*zf) + (A_i*zi) + (A_b*zb) + (A_s*zs) + u_A;
mu=(mu_f*zf) + (mu_i*zi) + (mu_b*zb) + (mu_s*zs) + u_mu;
sigma1=(sigma1_f*zf) + (sigma1_i*zi) + (sigma1_b*zb) + (sigma1_s*zs) + u_sigma1;
sigma2=(sigma2_f*zf) + (sigma2_i*zi) + (sigma2_b*zb) + (sigma2_s*zs) + u_sigma2;
pred =b + A *(DOY<1000*mu)*exp(-((DOY-1000*mu)/(1000*sigma1))**2/2)+A *(DOY>1000*mu)*exp(-((DOY-1000*mu)/(1000*sigma2))**2/2);
model NDVI ~ normal(pred,0.0071033);
random u_b u_a u_mu u_sigma1 u_sigma2 ~normal([0,0,0,0,0],[0.0020308309,
-.0021566753,0.0108019545,
0.0000443530,0.0011981478,0.0004145846,
-.0002803647,0.0012745033,0.0001760337,0.0001901562,
-.0007970436,0.0002448164,-.0002744048,0.0000331801,0.0005176882]) 
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
by  marshtype location ;
run;
proc print data=out_G;
run;

proc sort data=out_G;
by year marshtype doy  ;
run;
 
PROC MEANS DATA = out_G MEAN;
VAR NDVI;
by year marshtype doy ;
output out=data_NDVI mean=mean;
RUN;
proc print data=data_ndvi;
run;

PROC MEANS DATA = out_G MEAN;
VAR pred;
by year marshtype doy ;
output out=data_pred mean=mean;
RUN;
proc print data=data_pred;
run;

PROC MEANS DATA = out_G MEAN;
VAR lower;
by year marshtype doy ;
output out=data_lower mean=mean;
RUN;
proc print data=data_lower;
run;


PROC MEANS DATA = out_G MEAN;
VAR upper;
by year marshtype doy ;
output out=data_upper mean=mean;
RUN;
proc print data=data_upper;
run;
/*GAUSS distribution*/

