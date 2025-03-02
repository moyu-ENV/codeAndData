options nocenter;
/*IMPORT DATA*/
PROC IMPORT DATAFILE= "g:\landsat\data\LANDSAT.XLS" 
OUT=data_1 
DBMS=XLS
replace;
SHEET='2002';
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
parms b=0.2 A=0.5 mu=0.24 sigma=0.016; 
model NDVI = b + A *exp(-((DOY-1000*mu)/(1000*sigma))**2/2);
by unit;
run;
proc print data=g_out;
run;

data byunit; 
input unit marshtype$ location$ b A mu sigma MSe;
datalines;
1 Brackish Baratari 0.40289 0.09962 0.24000 0.01600 0.00197
2 Brackish BretonSo 0.49746 0.09691 0.24000 0.01600 0.00290
3 Brackish Pontchar 0.45467 0.09258 0.24000 0.01600 0.00228
4 Brackish Terrebon 0.44878 0.08048 0.24000 0.01600 0.00389
5 Freshwat Baratari 0.55403 0.18115 0.24000 0.01600 0.0179
6 Freshwat BretonSo 0.50408 0.18578 0.24000 0.01600 0.00865
7 Freshwat Pontchar 0.22907 0.44514 0.17473 0.11196 0.000658
8 Freshwat Terrebon 0.58940 0.15878 0.24000 0.01600 0.0166
9 Intermed Baratari 0.48933 0.16318 0.24000 0.01600 0.00782
10 Intermed BretonSo 0.50041 0.15706 0.24000 0.01600 0.00523
11 Intermed Pontchar 0.27625 0.37084 0.18975 0.10894 0.00123
12 Intermed Terrebon 0.47169 0.12548 0.24000 0.01600 0.00796
13 SalineMa Baratari 0.36696 0.05517 0.24000 0.01600 0.00174
14 SalineMa BretonSo 0.44040 0.04215 0.24000 0.01600 0.00243
15 SalineMa Pontchar 0.41574 0.03601 0.24000 0.01600 0.00136
16 SalineMa Terrebon 0.34863 0.00941 0.24000 0.01600 0.00142
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
parms b_b=0.4509500 A_b=0.0923975 mu_b=0.2400000 sigma_b=0.0160000
      b_f=0.4691450 A_f=0.2427125 mu_f=0.2236825 sigma_f=0.0399900
      b_i=0.4344200 A_i=0.2041400 mu_i=0.2041400 sigma_i=0.0392350
      b_s=0.3929325 A_s=0.0356850 mu_s=0.2400000 sigma_s=0.0160000
      /*s2e=0.0046333*/; 
B=(B_f*zf) + (B_i*zi) + (B_b*zb) + (B_s*zs) + u_b;
A=(A_f*zf) + (A_i*zi) + (A_b*zb) + (A_s*zs) + u_A;
mu=(mu_f*zf) + (mu_i*zi) + (mu_b*zb) + (mu_s*zs) + u_mu;
sigma=(sigma_f*zf) + (sigma_i*zi) + (sigma_b*zb) + (sigma_s*zs) + u_sigma;
pred =b + A *exp(-((DOY-1000*mu)/(1000*sigma))**2/2);
model NDVI ~ normal(pred,0.0052524);
random u_b u_a u_mu u_sigma ~normal([0,0,0,0],[0.0103501581,
-.0083082257,0.0078541560,
0.0019681448,-.0017990932,0.0004240772,
-.0031448264,0.0029098367,-.0006833465,0.0011153853]) 
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


