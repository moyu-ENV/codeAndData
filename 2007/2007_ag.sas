options nocenter;
/*IMPORT DATA*/
PROC IMPORT DATAFILE= "g:\data\LANDSAT.XLS" 
OUT=data2007_1 
DBMS=XLS
replace;
SHEET='2007';
GETNAMES=YES;
RUN;
PROC PRINT DATA=DATA2007_1;
RUN;
data data2007; 
set DATA2007_1;
array Locations{16} $ L1-L16 ('Barataria' 'BretonSound' 'Pontchartrain' 'Terrebonne' 'Barataria' 'BretonSound' 'Pontchartrain' 'Terrebonne' 'Barataria' 'BretonSound' 'Pontchartrain' 'Terrebonne' 'Barataria' 'BretonSound' 'Pontchartrain' 'Terrebonne');
array Marshtypes{16} $ M1-M16 ('BrackishMarsh' 'BrackishMarsh' 'BrackishMarsh' 'BrackishMarsh' 'FreshwaterMarsh' 'FreshwaterMarsh' 'FreshwaterMarsh' 'FreshwaterMarsh' 'IntermediateMarsh' 'IntermediateMarsh' 'IntermediateMarsh' 'IntermediateMarsh' 'SalineMarsh' 'SalineMarsh' 'SalineMarsh' 'SalineMarsh');
array units{16} unit1-unit16; 
do unit=1 to 16;
Marshtype = MarshTypes(unit);
Location = Locations(unit);
NDVI=units(unit);
output;
end;
keep year doy unit Marshtype Location NDVI;
run;
proc print data=data2007;
run;
/*end of import data*/

/*GAUSS distribution*/
ods select ConvergenceStatus Anova ParameterEstimates;
proc sort data=data2007;
by unit;
run;
proc nlin data=data2007 method=newton outest=G_out;
parms b=0.1 A=0.5 mu=0.21 sigma1=0.03 sigma2=0.015; 
model NDVI = b + A *(DOY<1000*mu)*exp(-((DOY-1000*mu)/(1000*sigma1))**2/2)+A *(DOY>1000*mu)*exp(-((DOY-1000*mu)/(1000*sigma2))**2/2);
by unit;
run;
proc print data=g_out;
run;  

data byunit; 
input unit marshtype$ location$ b A mu sigma1 sigma2 MSe;
datalines;
1 Brackish Baratari 0.41793 0.11234 0.23897 0.04319 0.04120    0.00221
2 Brackish BretonSo 0.48213 0.08530 0.21000 0.03000 0.01108    0.00515
3 Brackish Pontchar 0.37719 0.18380 0.29228 0.14577 0.02480    0.00228
4 Brackish Terrebon 0.45594 0.07103 0.21000 0.03000 0.01106    0.00375
5 Freshwat Baratari 0.51906 0.21283 0.20800 0.03000 0.02198    0.0160
6 Freshwat BretonSo 0.55337 0.12858 0.21000 0.03000 0.01050    0.00950
7 Freshwat Pontchar 0.48999 0.19274 0.22319 0.03000 0.02291    0.0120
8 Freshwat Terrebon 0.14961 0.55575 0.18078 0.11907 0.18351    0.00191
9 Intermed Baratari 0.44723 0.16843 0.24860 0.04464 0.02265    0.00543
10 Intermed BretonSo 0.47059 0.15209 0.21791 0.03000 0.07337   0.00699
11 Intermed Pontchar 0.46570 0.16637 0.22786 0.03000 0.04379   0.00612
12 Intermed Terrebon 0.46213 0.15553 0.22875 0.04771 0.02943   0.00493
13 SalineMa Baratari 0.39425 0.07164 0.22679 -0.00143 -0.02929  0.00162
14 SalineMa BretonSo 0.45753 0.02096 0.21000 0.03000 0.01079  0.00306
15 SalineMa Pontchar 0.42954 -0.05775 0.18914 -0.00350 0.00580   0.00311
16 SalineMa Terrebon 0.38237 0.01383 0.20635 0.03000 0.01483   0.00143
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

proc sort data=data2007;
by unit;
run;
Proc nlmixed data=data2007 method=firo;
if (marshtype='Freshwat') then do;zf=1;zi=0;zb=0;zs=0;end;
if (marshtype='Intermed') then do;zf=0;zi=1;zb=0;zs=0;end;
if (marshtype='Brackish') then do;zf=0;zi=0;zb=1;zs=0;end;
if (marshtype='SalineMa') then do;zf=0;zi=0;zb=0;zs=1;end;
parms b_b=0.4332975 A_b=0.1131175 mu_b=0.2378125 sigma1_b=0.0622400 sigma2_b=0.0220350
      b_f=0.4280075 A_f=0.2724750 mu_f=0.2054925 sigma1_f=0.0522675 sigma2_f=0.0597250
      b_i=0.4614125 A_i=0.1606050 mu_i=0.2307800 sigma1_i=0.0380875 sigma2_i=0.0423100
      b_s=0.4159225 A_s=0.0121700 mu_s=0.2080700 sigma1_s=0.0137675 sigma2_s=0.000532500
      /*s2e=0.0046333*/; 
B=(B_f*zf) + (B_i*zi) + (B_b*zb) + (B_s*zs) + u_b;
A=(A_f*zf) + (A_i*zi) + (A_b*zb) + (A_s*zs) + u_A;
mu=(mu_f*zf) + (mu_i*zi) + (mu_b*zb) + (mu_s*zs) + u_mu;
sigma1=(sigma1_f*zf) + (sigma1_i*zi) + (sigma1_b*zb) + (sigma1_s*zs) + u_sigma1;
sigma2=(sigma2_f*zf) + (sigma2_i*zi) + (sigma2_b*zb) + (sigma2_s*zs) + u_sigma2;
pred =b + A *(DOY<1000*mu)*exp(-((DOY-1000*mu)/(1000*sigma1))**2/2)+A *(DOY>1000*mu)*exp(-((DOY-1000*mu)/(1000*sigma2))**2/2);
model NDVI ~ normal(pred, 0.0053431);
random u_b u_a u_mu u_sigma1 u_sigma2 ~normal([0,0,0,0,0],[0.0096209313,
-.0096366116,0.0105921432,
0.0002331833,-.0000602506,0.0005564113,
-.0026220894,0.0028251516,0.0003658789,0.0013909633,
-.0038631723,0.0038258326,-.0003862856,0.0009869461,0.0019906123]) 
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

contrast 'sigma F VS I' sigma1_f, sigma1_i;
contrast 'sigma F VS B' sigma1_f, sigma1_b;
contrast 'sigma F VS S' sigma1_f, sigma1_s;
contrast 'sigma I VS B' sigma1_i, sigma1_b;
contrast 'sigma I VS S' sigma1_i, sigma1_s;
contrast 'sigma B VS S' sigma1_b, sigma1_s;

predict pred out=out_G;
run;

proc sort data=out_G;
by  marshtype location;
run;
proc print data=out_G;
run;
 
proc sort data=out_G;
by year marshtype doy;
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

