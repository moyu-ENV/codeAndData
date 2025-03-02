options nocenter;
/*IMPORT DATA*/
PROC IMPORT DATAFILE= "g:\data\LANDSAT.XLS" 
OUT=data_1 
DBMS=XLS
replace;
SHEET='20072';
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
parms b=0.2 A=0.4 mu=0.23 sigma=0.02; 
model NDVI = b + A *exp(-((DOY-1000*mu)/(1000*sigma))**2/2);
by unit;
run;
proc print data=g_out;
run;

data byunit; 
input unit marshtype$ location$ b A mu sigma MSe;
datalines;
1 Brackish Baratari 0.41653 0.11086 0.23538 0.04536 0.00199
2 Brackish BretonSo 0.46906 0.16476 0.21605 0.01654 0.00469
3 Brackish Pontchar 0.45481 0.14842 0.21455 0.01671 0.00415
4 Brackish Terrebon 0.23540 0.29034 0.20400 0.14904 0.00174
5 Freshwat Baratari 0.51757 0.20653 0.20379 0.02855 0.0158
6 Freshwat BretonSo 0.53893 0.37526 0.24852 0.01440 0.00813
7 Freshwat Pontchar 0.25332 0.43812 0.19494 0.09807 0.00281
8 Freshwat Terrebon 0.02833 0.67730 0.20527 0.16611 0.00214
9 Intermed Baratari 0.22632 0.36586 0.20397 0.11940 0.00178
10 Intermed BretonSo 0.46178 0.17643 0.20896 0.05075 0.00577
11 Intermed Pontchar 0.46470 0.16881 0.23432 0.03830 0.00543
12 Intermed Terrebon 0.26152 0.34474 0.20254 0.11637 0.00197
13 SalineMa Baratari 0.39454 0.38028 0.24912 0.00933 0.00163
14 SalineMa BretonSo 0.44753 0.08638 0.24541 0.02169 0.00269
15 SalineMa Pontchar 0.41047 0.29930 0.24925 0.01259 0.00211
16 SalineMa Terrebon 0.37476 0.03273 0.23086 0.04805  0.00139
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
parms b_b=0.3939500 A_b=0.1785950 mu_b=0.2174950 sigma_b=0.0569125
      b_f=0.3345375 A_f=0.4243025 mu_f=0.2131300 sigma_f=0.0767825
      b_i=0.3535800 A_i=0.2639600 mu_i=0.2124475 sigma_i=0.0812050
      b_s=0.4068250 A_s=0.1996725 mu_s=0.2436600 sigma_s=0.0229150
      /*s2e=0.0046333*/; 
B=(B_f*zf) + (B_i*zi) + (B_b*zb) + (B_s*zs) + u_b;
A=(A_f*zf) + (A_i*zi) + (A_b*zb) + (A_s*zs) + u_A;
mu=(mu_f*zf) + (mu_i*zi) + (mu_b*zb) + (mu_s*zs) + u_mu;
sigma=(sigma_f*zf) + (sigma_i*zi) + (sigma_b*zb) + (sigma_s*zs) + u_sigma;
pred =b + A *exp(-((DOY-1000*mu)/(1000*sigma))**2/2);
model NDVI ~ normal(pred,0.0040137);
random u_b u_a u_mu u_sigma ~normal([0,0,0,0],[0.0218503525,
-.0160133282,0.0207566115,
0.0013452329,-.0003758798,0.0002611375,
-.0073245377,0.0046039294,-.0005066573,0.0027421529]) 
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

