options nocenter;
/*IMPORT DATA*/
PROC IMPORT DATAFILE= "g:\data\LANDSAT.XLS" 
OUT=data_1 
DBMS=XLS
replace;
SHEET='1999';
GETNAMES=YES;
RUN;
PROC PRINT DATA=DATA_1;
RUN;
data data; 
set DATA_1;
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
proc print data=data;
run;
/*end of import data*/

/*GAUSS distribution*/
ods select ConvergenceStatus Anova ParameterEstimates;
proc sort data=data;
by unit;
run;
proc nlin data=data method=newton outest=G_out;
parms b=0.25 A=0.4 mu=0.22 sigma=0.03; 
model NDVI = b + A *exp(-((DOY-1000*mu)/(1000*sigma))**2/2);
by unit;
run;
proc print data=g_out;
run;

data byunit; 
input unit marshtype$ location$ b A mu sigma MSe;
datalines;
1 Brackish Baratari 0.42688 0.10692 0.22000 0.03000 0.00631
2 Brackish BretonSo 0.49818 0.07228 0.22000 0.03000 0.00622
3 Brackish Pontchar 0.44873 0.09671 0.22000 0.03000 0.00480
4 Brackish Terrebon 0.33341 0.22897 0.23978 0.07386 0.000765
5 Freshwat Baratari 0.52411 0.22351 0.22000 0.03000 0.0146
6 Freshwat BretonSo 0.47551 0.18389 0.22000 0.03000 0.00933
7 Freshwat Pontchar 0.01117 0.66452 0.19517 0.14022 0.00142
8 Freshwat Terrebon 0.54834 0.21742 0.22000 0.03000 0.0135
9 Intermed Baratari -0.14921 0.80310 0.20071 0.17836 0.00154
10 Intermed BretonSo 0.39583 0.23483 0.24878 0.05230 0.00125
11 Intermed Pontchar 0.22311 0.42946 0.20915 0.10697 0.00137
12 Intermed Terrebon 0.31808 0.31602 0.21352 0.08272 0.00140
13 SalineMa Baratari 0.38125 0.00996 0.22000 0.03000 0.00413
14 SalineMa BretonSo 0.45114 0.01843 0.22000 0.03000 0.00609
15 SalineMa Pontchar 0.41380 0.01702 0.22000 0.03000 0.00527
16 SalineMa Terrebon 0.36175 0.04160 0.22000 0.03000 0.00271
RUN;
proc print data=byunit;
run;
PROC MEANS DATA = byunit MEAN VAR;
VAR b A mu sigma MSe;
RUN;
PROC MEANS DATA = byunit MEAN VAR;
VAR b A mu sigma MSe;
BY marshtype;
RUN; 
proc discrim data= byunit pcov;
class marshtype;
var b a mu sigma;
run;

proc sort data=data;
by unit;
run;
Proc nlmixed data=data method=firo;
if (marshtype='Freshwat') then do;zf=1;zi=0;zb=0;zs=0;end;
if (marshtype='Intermed') then do;zf=0;zi=1;zb=0;zs=0;end;
if (marshtype='Brackish') then do;zf=0;zi=0;zb=1;zs=0;end;
if (marshtype='SalineMa') then do;zf=0;zi=0;zb=0;zs=1;end;
parms b_b=0.4268000 A_b=0.1262200 mu_b=0.2249450 sigma_b=0.0409650
      b_f=0.3897825 A_f=0.3223350 mu_f=0.2137925 sigma_f=0.0575550
      b_i=0.1969525 A_i=0.4458525 mu_i=0.2180400 sigma_i=0.1050875
      b_s=0.4019850 A_s=0.0217525 mu_s=0.2200000 sigma_s=0.0300000
      /*s2e=0.0046333*/; 
B=(B_f*zf) + (B_i*zi) + (B_b*zb) + (B_s*zs) + u_b;
A=(A_f*zf) + (A_i*zi) + (A_b*zb) + (A_s*zs) + u_A;
mu=(mu_f*zf) + (mu_i*zi) + (mu_b*zb) + (mu_s*zs) + u_mu;
sigma=(sigma_f*zf) + (sigma_i*zi) + (sigma_b*zb) + (sigma_s*zs) + u_sigma;
pred =b + A *exp(-((DOY-1000*mu)/(1000*sigma))**2/2);
model NDVI ~ normal(pred, 0.0050441);
random u_b u_a u_mu u_sigma ~normal([0,0,0,0],[0.0322933451,
-.0306658718,0.0301324527,
0.0015738457,-.0015341181,0.0001750477,
-.0070291059,0.0068675986,-.0003508369,0.0016011116]) 
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

contrast 'sigma F VS I' sigma_f, sigma_i;
contrast 'sigma F VS B' sigma_f, sigma_b;
contrast 'sigma F VS S' sigma_f, sigma_s;
contrast 'sigma I VS B' sigma_i, sigma_b;
contrast 'sigma I VS S' sigma_i, sigma_s;
contrast 'sigma B VS S' sigma_b, sigma_s;

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


