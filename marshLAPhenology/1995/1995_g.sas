options nocenter;
/*IMPORT DATA*/
PROC IMPORT DATAFILE= "g:\data\LANDSAT.XLS" 
OUT=data_1 
DBMS=XLS
replace;
SHEET='1995';
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
parms b=0.2 A=0.45 mu=0.20 sigma=0.015; 
model NDVI = b + A *exp(-((DOY-1000*mu)/(1000*sigma))**2/2);
by unit;
run;
proc print data=g_out;
run;

data byunit; 
input unit marshtype$ location$ b A mu sigma MSe;
datalines;
1 Brackish Baratari 0.43622 0.10300 0.2 0.015 0.00417
2 Brackish BretonSo 0.49630 0.12019 0.2 0.015 0.00416
3 Brackish Pontchar 0.47622 0.11913 0.2 0.015 0.00441
4 Brackish Terrebon 0.46957 0.07595 0.2 0.015 0.00448
5 Freshwat Baratari 0.58806 0.15151 0.2 0.015 0.0145
6 Freshwat BretonSo 0.45689 0.10824 0.2 0.015 0.00731
7 Freshwat Pontchar 0.52609 0.15282 0.2 0.015 0.0144
8 Freshwat Terrebon 0.62007 0.13171 0.2 0.015 0.0125
9 Intermed Baratari 0.53643 0.15980 0.2 0.015 0.0117
10 Intermed BretonSo 0.50397 0.12291 0.2 0.015 0.00518
11 Intermed Pontchar 0.56226 0.14691 0.2 0.015 0.0118
12 Intermed Terrebon 0.53027 0.12949 0.2 0.015 0.00752
13 SalineMa Baratari 0.36837 0.08600 0.2 0.015 0.00191
14 SalineMa BretonSo 0.41013 0.03079 0.2 0.015 0.00184
15 SalineMa Pontchar 0.39413 0.06600 0.2 0.015 0.00170
16 SalineMa Terrebon 0.35673 0.04140 0.2 0.015 0.00157
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
parms b_b=0.4695775 A_b=0.1045675 mu_b=0.2000000 sigma_b=0.0150000
      b_f=0.5477775 A_f=0.1360700 mu_f=0.2000000 sigma_f=0.0150000
      b_i=0.5332325 A_i=0.1397775 mu_i=0.2000000 sigma_i=0.0150000
      b_s=0.3823400 A_s=0.0560475 mu_s=0.2000000 sigma_s=0.0150000
      /*s2e=0.0046333*/; 
B=(B_f*zf) + (B_i*zi) + (B_b*zb) + (B_s*zs) + u_b;
A=(A_f*zf) + (A_i*zi) + (A_b*zb) + (A_s*zs) + u_A;
mu=(mu_f*zf) + (mu_i*zi) + (mu_b*zb) + (mu_s*zs) + u_mu;
sigma=(sigma_f*zf) + (sigma_i*zi) + (sigma_b*zb) + (sigma_s*zs) + u_sigma;
pred =b + A *exp(-((DOY-1000*mu)/(1000*sigma))**2/2);
model NDVI ~ normal(pred,0.0068219);
random u_b u_a u_mu u_sigma ~normal([0,0,0,0],[0.0017442433,
0.0002672374,0.0068522468,
-.0003070388,0.0004400696,0.0000000000,
0.0000000000,0.0000000000,0.0000000000,0.0000000000]) 
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

