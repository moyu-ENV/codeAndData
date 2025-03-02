options nocenter;
/*IMPORT DATA*/
PROC IMPORT DATAFILE= "g:\data\LANDSAT.XLS" 
OUT=data_1 
DBMS=XLS
replace;
SHEET='2012';
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
parms b=0.2 A=0.44 mu=0.2 sigma1=0.023 sigma2=0.02; 
model NDVI = b + A *(DOY<1000*mu)*exp(-((DOY-1000*mu)/(1000*sigma1))**2/2)+A *(DOY>1000*mu)*exp(-((DOY-1000*mu)/(1000*sigma2))**2/2);
by unit;
run;
proc print data=g_out;
run;  

data byunit; 
input unit marshtype$ location$ b A mu sigma1 sigma2 MSe;
datalines;
1 Brackish Baratari 0.35490 0.13644 0.21871 0.11210 0.03057 0.000804
2 Brackish BretonSo 0.42790 0.15773 0.20000 0.02300 0.02079 0.00960
3 Brackish Pontchar 0.30088 0.24274 0.20345 0.13358 0.03336 0.000902
4 Brackish Terrebon 0.40081 0.12931 0.20000 0.06672 0.02000 0.00232
5 Freshwat Baratari 0.51775 0.23660 0.20000 0.06509 0.02000 0.00711
6 Freshwat BretonSo 0.34325 0.37675 0.20936 0.09752 0.02469 0.00248
7 Freshwat Pontchar 0.35717 0.33607 0.15102 0.05113 0.09442 0.000605
8 Freshwat Terrebon 0.53213 0.23515 0.20000 0.07256 0.02000 0.00720
9 Intermed Baratari 0.44651 0.17588 0.20000 0.05429 0.02000 0.00435
10 Intermed BretonSo 0.31275 0.28828 0.20000 0.10950 0.00734 0.00566
11 Intermed Pontchar 0.38256 0.24102 0.19705 0.09054 0.03798 0.000981
12 Intermed Terrebon 0.44478 0.20662 0.20000 0.07159 0.02000 0.00476
13 SalineMa Baratari 0.37287 0.05487 0.20000 0.02300 0.01550 0.00156
14 SalineMa BretonSo 0.41340 0.08036 0.20000 0.02300 0.01994 0.00289
15 SalineMa Pontchar 0.30416 0.16149 0.22060 0.17160 0.02007 0.000580
16 SalineMa Terrebon 0.38480 0.01733 0.20000 0.02300 0.02000 0.00131
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
parms b_b=0.3711225 A_b=0.1665550 mu_b=0.2055400 sigma1_b=0.0838500 sigma2_b=0.0261800
      b_f=0.4375750 A_f=0.2961425 mu_f=0.1900950 sigma1_f=0.0715750 sigma2_f=0.0397775
      b_i=0.3966500 A_i=0.2279500 mu_i=0.1992625 sigma1_i=0.0814800 sigma2_i=0.0213300
      b_s=0.3688075 A_s=0.0785125 mu_s=0.2051500 sigma1_s=0.0601500 sigma2_s=0.0188775
      /*s2e=0.0046333*/; 
B=(B_f*zf) + (B_i*zi) + (B_b*zb) + (B_s*zs) + u_b;
A=(A_f*zf) + (A_i*zi) + (A_b*zb) + (A_s*zs) + u_A;
mu=(mu_f*zf) + (mu_i*zi) + (mu_b*zb) + (mu_s*zs) + u_mu;
sigma1=(sigma1_f*zf) + (sigma1_i*zi) + (sigma1_b*zb) + (sigma1_s*zs) + u_sigma1;
sigma2=(sigma2_f*zf) + (sigma2_i*zi) + (sigma2_b*zb) + (sigma2_s*zs) + u_sigma2;
pred =b + A *(DOY<1000*mu)*exp(-((DOY-1000*mu)/(1000*sigma1))**2/2)+A *(DOY>1000*mu)*exp(-((DOY-1000*mu)/(1000*sigma2))**2/2);
model NDVI ~ normal(pred,0.0033195);
random u_b u_a u_mu u_sigma1 u_sigma2 ~normal([0,0,0,0,0],[0.0048761140,
-.0035874478,0.0034750866,
0.0001016102,0.0000140837,0.0002215176,
-.0019218278,0.0017900078,0.0003511259,0.0022222703,
-.0005589857,0.0003021090,-.0002279377,-.0000416283,0.0003854585]) 
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

