options nocenter;
/*IMPORT DATA*/
PROC IMPORT DATAFILE= "g:\landsat\data\LANDSAT.XLS" 
OUT=data_1 
DBMS=XLS
replace;
SHEET='2014';
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
parms b=0.15 A=0.5 mu=0.21 sigma1=0.1 sigma2=0.1; 
model NDVI = b + A *(DOY<1000*mu)*exp(-((DOY-1000*mu)/(1000*sigma1))**2/2)+A *(DOY>1000*mu)*exp(-((DOY-1000*mu)/(1000*sigma2))**2/2);
by unit;
run;
proc print data=g_out;
run;  

data byunit; 
input unit marshtype$ location$ b A mu sigma1 sigma2 MSe;
datalines;
1 Brackish Baratari 0.45694 0.10909 0.22 0.023 0.025000 0.00729
2 Brackish BretonSo 0.45026 0.16675 0.22 0.023 0.025000 0.00754
3 Brackish Pontchar 0.42259 0.16053 0.22 0.023 0.059622 0.00662
4 Brackish Terrebon 0.43833 0.12169 0.22 0.023 0.042780 0.0102
5 Freshwat Baratari 0.51737 0.21978 0.22 0.023 0.025000 0.0271
6 Freshwat BretonSo 0.54437 0.19857 0.22 0.023 0.025000 0.0153
7 Freshwat Pontchar 0.49729 0.25195 0.22 0.023 0.035588 0.0166
8 Freshwat Terrebon 0.53814 0.20291 0.22 0.023 0.047316 0.0201
9 Intermed Baratari 0.48720 0.19213 0.22 0.023 0.025000 0.0150
10 Intermed BretonSo 0.48626 0.19136 0.22 0.023 0.025000 0.0110
11 Intermed Pontchar 0.47601 0.23726 0.22 0.023 0.042879 0.0117
12 Intermed Terrebon 0.47874 0.17331 0.22 0.023 0.044410 0.0145
13 SalineMa Baratari 0.40675 0.09244 0.22 0.023 0.025000 0.00402
14 SalineMa BretonSo 0.44354 0.11839 0.22 0.023 0.025000 0.00412
15 SalineMa Pontchar 0.41315 0.10560 0.22 0.023 0.025000 0.00359
16 SalineMa Terrebon 0.37527 0.09940 0.22 0.023 0.061371 0.00446
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
parms b_b=0.4420300 A_b=0.1395150 mu_b=0.2200000 sigma1_b=0.0230000 sigma2_b=0.0381005
      b_f=0.5242925 A_f=0.2183025 mu_f=0.2200000 sigma1_f=0.0230000 sigma2_f=0.0332260
      b_i=0.4820525 A_i=0.1985150 mu_i=0.2200000 sigma1_i=0.0230000 sigma2_i=0.0343223
      b_s=0.4096775 A_s=0.1039575 mu_s=0.2200000 sigma1_s=0.0230000 sigma2_s=0.0340928
      /*s2e=0.0046333*/; 
B=(B_f*zf) + (B_i*zi) + (B_b*zb) + (B_s*zs) + u_b;
A=(A_f*zf) + (A_i*zi) + (A_b*zb) + (A_s*zs) + u_A;
mu=(mu_f*zf) + (mu_i*zi) + (mu_b*zb) + (mu_s*zs) + u_mu;
sigma1=(sigma1_f*zf) + (sigma1_i*zi) + (sigma1_b*zb) + (sigma1_s*zs) + u_sigma1;
sigma2=(sigma2_f*zf) + (sigma2_i*zi) + (sigma2_b*zb) + (sigma2_s*zs) + u_sigma2;
pred =b + A *(DOY<1000*mu)*exp(-((DOY-1000*mu)/(1000*sigma1))**2/2)+A *(DOY>1000*mu)*exp(-((DOY-1000*mu)/(1000*sigma2))**2/2);
model NDVI ~ normal(pred,0.0111963);
random u_b u_a u_mu u_sigma1 u_sigma2 ~normal([0,0,0,0,0],[0.0003747015,
-.0001357363,0.0005650227,
0.0000000000,0.0000000000,0.0000000000,
0.0000000000,0.0000000000,0.0000000000,0.0000000000,
-.0001782903,0.0000384288,0.0000000000,0.0000000000,0.0002090572]) 
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

