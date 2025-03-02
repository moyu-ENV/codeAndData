options nocenter;
/*IMPORT DATA*/
PROC IMPORT DATAFILE= "g:\data\LANDSAT.XLS" 
OUT=data_1 
DBMS=XLS
replace;
SHEET='19902';
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
parms b=0.1 A=0.5 mu=0.23 sigma=0.024; 
model NDVI = b + A *exp(-((DOY-1000*mu)/(1000*sigma))**2/2);
by unit;
run;
proc print data=g_out;
run;

data byunit; 
input unit marshtype$ location$ b A mu sigma MSe;
datalines;
1 Brackish Baratari 0.41384 0.03333 0.23000 0.02400 0.00204
2 Brackish BretonSo 0.40817 0.11953 0.23000 0.02400 0.00965
3 Brackish Pontchar 0.43155 0.13774 0.23000 0.02400 0.00993
4 Brackish Terrebon 0.42011 0.10692 0.23000 0.02400 0.00368
5 Freshwat Baratari 0.54177 0.15900 0.23000 0.02400 0.0207
6 Freshwat Pontchar 0.56517 0.09076 0.23000 0.02400 0.00360
7 Freshwat Terrebon 0.57130 0.15129 0.23000 0.02400 0.00640
8 Intermed Baratari 0.39874 0.20872 0.23020 0.05984 0.00105
9 Intermed BretonSo 0.23488 0.29946 0.25283 0.06042 0.000721
10 Intermed Pontchar 0.18419 0.46623 0.21228 0.10614 0.000314
11 Intermed Terrebon 0.35910 0.29601 0.26260 0.01258 0.00613
12 SalineMa Baratari 0.35910 0.29601 0.26260 0.01258 0.00127
13 SalineMa BretonSo 0.39042 0.05581 0.23000 0.02400 0.00802
14 SalineMa Pontchar 0.40824 0.02047 0.23000 0.02400 0.00102
15 SalineMa Terrebon 0.36015 0.06366 0.23000 0.02400 0.00238
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
parms b_b=0.4184175 A_b=0.0993800 mu_b=0.2300000 sigma_b=0.0240000
      b_f=0.5594133 A_f=0.1336833 mu_f=0.2300000 sigma_f=0.0240000
      b_i=0.2942275 A_i=0.3176050 mu_i=0.2394775 sigma_i=0.0597450
      b_s=0.3794775 A_s=0.1089875 mu_s=0.2381500 sigma_s=0.0211450
      /*s2e=0.0046333*/; 
B=(B_f*zf) + (B_i*zi) + (B_b*zb) + (B_s*zs) + u_b;
A=(A_f*zf) + (A_i*zi) + (A_b*zb) + (A_s*zs) + u_A;
mu=(mu_f*zf) + (mu_i*zi) + (mu_b*zb) + (mu_s*zs) + u_mu;
sigma=(sigma_f*zf) + (sigma_i*zi) + (sigma_b*zb) + (sigma_s*zs) + u_sigma;
pred =b + A *exp(-((DOY-1000*mu)/(1000*sigma))**2/2);
model NDVI ~ normal(pred,0.0051270);
random u_b u_a u_mu u_sigma ~normal([0,0,0,0],[0.0030258642,
-.0030902901,0.0083205835,
0.0001878546,0.0002112067,0.0002123441,
-.0007238474,0.0005232360,-.0002384989,0.0004068464]) 
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
