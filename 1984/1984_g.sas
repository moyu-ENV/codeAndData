options nocenter;
/*IMPORT DATA*/
PROC IMPORT DATAFILE= "g:\data\LANDSAT.XLS" 
OUT=data_1 
DBMS=XLS
replace;
SHEET='1984';
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
parms b=0.3 A=0.4 mu=0.24 sigma=0.018; 
model NDVI = b + A *exp(-((DOY-1000*mu)/(1000*sigma))**2/2);
by unit;
run;
proc print data=g_out;
run;

data byunit; 
input unit marshtype$ location$ b A mu sigma MSe;
datalines;
1 Brackish Baratari 0.37628 0.06749 0.2400 0.0180 0.00215
2 Brackish BretonSo 0.45007 0.08947 0.2400 0.0180 0.00252
3 Brackish Pontchar 0.47455 0.04888 0.2400 0.0180 0.00314
4 Brackish Terrebon 0.42249 0.06635 0.2400 0.0180 0.00283
5 Freshwat Baratari 0.49824 0.16746 0.2400 0.0180 0.0165
6 Freshwat Pontchar 0.55108 0.06659 0.2400 0.0180 0.00815
7 Freshwat Terrebon 0.53462 0.14240 0.2400 0.0180 0.0102
8 Intermed Baratari 0.45907 0.14238 0.2400 0.0180 0.00736
9 Intermed BretonSo 0.16211 0.35973 0.2495 0.0964 0.00143
10 Intermed Pontchar 0.51819 0.03982 0.2400 0.0180 0.00629
11 Intermed Terrebon 0.48430 0.10150 0.2400 0.0180 0.00466
12 SalineMa Baratari 0.36246 0.12924 0.2374 0.0029 0.00213
13 SalineMa BretonSo 0.43675 -0.03668 0.2368 0.0032 0.00206
14 SalineMa Pontchar 0.40764 0.01111 0.2400 0.0180 0.00113
15 SalineMa Terrebon 0.39566 0.03739 0.2400 0.0180 0.00168
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
parms b_b=0.4308475 A_b=0.0680475 mu_b=0.2400000 sigma_b=0.0180000
      b_f=0.5279800 A_f=0.1254833 mu_f=0.2400000 sigma_f=0.0180000
      b_i=0.4059175 A_i=0.1608575 mu_i=0.2423750 sigma_i=0.0376000
      b_s=0.4006275 A_s=0.0352650 mu_s=0.2385500 sigma_s=0.0105250
      /*s2e=0.0046333*/; 
B=(B_f*zf) + (B_i*zi) + (B_b*zb) + (B_s*zs) + u_b;
A=(A_f*zf) + (A_i*zi) + (A_b*zb) + (A_s*zs) + u_A;
mu=(mu_f*zf) + (mu_i*zi) + (mu_b*zb) + (mu_s*zs) + u_mu;
sigma=(sigma_f*zf) + (sigma_i*zi) + (sigma_b*zb) + (sigma_s*zs) + u_sigma;
pred =b + A *exp(-((DOY-1000*mu)/(1000*sigma))**2/2);
model NDVI ~ normal(pred,0.0048153);
random u_b u_a u_mu u_sigma ~normal([0,0,0,0],[0.0082400637,
-.0069958029,0.0071824172,
-.0002120480,0.0001704707,0.0000069343,
-.0017338902,0.0013852152,0.0000547150,0.0004394061]) 
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
 
PROC MEANS DATA = out_G MEAN;
VAR pred;
by marshtype notsorted;
class doy;
RUN;
PROC MEANS DATA = out_G MEAN;
VAR stderrpred;
by marshtype notsorted;
class doy;
RUN;
PROC MEANS DATA = out_G MEAN;
VAR lower;
by marshtype notsorted;
class doy;
RUN;
PROC MEANS DATA = out_G MEAN;
VAR upper;
by marshtype notsorted;
class doy;
RUN;


proc means data=out_G;
var pred;
by  marshtype DOY notsorted;
output out=out_G_mean mean=mean stderr=stderr;
run;
proc print data=out_G_mean;
run;
data G;
merge out_G out_G_mean;
by doy marshtype;
run;
proc print data=G;
run;
/*GAUSS distribution*/


/*comparation of different model*/
%macro SSReductionTest;
data aovall; merge aov1 aov3;
if (Source='Error') then do;
Fstat = (SS1-SS3)/(df1-df3)/ms3;
pvalue= 1- probf(Fstat, df1-df3, df3);
output;
end;
run;
proc print data=aovall label noobs;
label Fstat = 'F Value'
      pValue = 'Prob>F';
format pvalue pvalue8.;
var Fstat pValue;
run;
%mend;
%SSReductionTest;

/*comparation of different model*/

/*checking ditrubution of residule*/
proc univariate data=p1;
var resid1;
/*class marshtype location;*/
histogram resid1/kernel(color=red) cfill=lrgray normal;
inset n='observations'/position=ne;
run;

proc univariate data=p1 normaltest;
var resid1;
probplot resid1/normal(mu=est sigma=est) square;
qqplot resid1/ normal(mu=est sigma=est) square;
run;
/*end of checking residule distribution*/
