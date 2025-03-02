options nocenter;
/*IMPORT DATA*/
PROC IMPORT DATAFILE= "g:\data\LANDSAT.XLS" 
OUT=data_1 
DBMS=XLS
replace;
SHEET='19862';
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
parms b=0.02 A=0.3 mu=0.21 sigma1=0.018 sigma2=0.018; 
model NDVI = b + A *(DOY<1000*mu)*exp(-((DOY-1000*mu)/(1000*sigma1))**2/2)+A *(DOY>1000*mu)*exp(-((DOY-1000*mu)/(1000*sigma2))**2/2);
by unit;
run;
proc print data=g_out;
run;  

data byunit; 
input unit marshtype$ location$ b A mu sigma1 sigma2 MSe;
datalines;
1 Brackish Baratari 0.40806 0.04223 0.21000 0.02 0.018000 0.00596
2 Brackish BretonSo 0.47504 0.07513 0.21000 0.02 0.018000 0.00551
3 Brackish Pontchar 0.47946 0.08886 0.21000 0.02 0.018000 0.00668
4 Brackish Terrebon 0.44126 0.07411 0.21000 0.02 0.018000 0.00834
5 Freshwat Baratari 0.52344 0.17585 0.21173 0.02 0.018000 0.0118
6 Freshwat Pontchar 0.49490 0.14665 0.21041 0.02 0.018000 0.00893
7 Freshwat Terrebon 0.53088 0.17925 0.21167 0.02 0.018000 0.0169
8 Intermed Baratari 0.47232 0.13141 0.21000 0.02 0.018000 0.0107
9 Intermed BretonSo 0.45360 0.09211 0.21000 0.02 0.018000 0.00759
10 Intermed Pontchar 0.42007 0.21528 0.21152 0.02 0.047167 0.00236
11 Intermed Terrebon 0.39917 0.22201 0.21248 0.02 0.057757 0.00240
12 SalineMa Baratari 0.36230 0.03141 0.21000 0.02 0.018000 0.00377
13 SalineMa BretonSo 0.43820 0.05454 0.21000 0.02 0.018000 0.00743
14 SalineMa Pontchar 0.44114 0.02005 0.21000 0.02 0.018000 0.00652
15 SalineMa Terrebon 0.39850 0.03338 0.21000 0.02 0.018000 0.00353
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
parms b_b=0.4509550 A_b=0.0700825 mu_b=0.2100000 sigma1_b=0.0200000 sigma2_b=0.0180000
      b_f=0.5164067 A_f=0.1672500 mu_f=0.2112700 sigma1_f=0.0200000 sigma2_f=0.0180000
      b_i=0.4362900 A_i=0.1652025 mu_i=0.2110000 sigma1_i=0.0200000 sigma2_i=0.0352310
      b_s=0.4100350 A_s=0.0348450 mu_s=0.2100000 sigma1_s=0.0200000 sigma2_s=0.0180000
      /*s2e=0.0046333*/; 
B=(B_f*zf) + (B_i*zi) + (B_b*zb) + (B_s*zs) + u_b;
A=(A_f*zf) + (A_i*zi) + (A_b*zb) + (A_s*zs) + u_A;
mu=(mu_f*zf) + (mu_i*zi) + (mu_b*zb) + (mu_s*zs) + u_mu;
sigma1=(sigma1_f*zf) + (sigma1_i*zi) + (sigma1_b*zb) + (sigma1_s*zs) + u_sigma1;
sigma2=(sigma2_f*zf) + (sigma2_i*zi) + (sigma2_b*zb) + (sigma2_s*zs) + u_sigma2;
pred =b + A *(DOY<1000*mu)*exp(-((DOY-1000*mu)/(1000*sigma1))**2/2)+A *(DOY>1000*mu)*exp(-((DOY-1000*mu)/(1000*sigma2))**2/2);
model NDVI ~ normal(pred,0.0072280);
random u_b u_a u_mu u_sigma1 u_sigma2 ~normal([0,0,0,0,0],[0.0010417575,
-.0002398759,0.0013320363,
-.0000081083,0.0000221339,0.0000005065,
0.0000000000,0.0000000000,0.0000000000,0.0000000000,
-.0001771699,0.0003381006,0.0000067279,0.0000000000,0.0001130640]) 
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

