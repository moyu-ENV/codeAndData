options nocenter;
/*IMPORT DATA*/
PROC IMPORT DATAFILE= "g:\landsat\data\LANDSAT.XLS" 
OUT=data_1 
DBMS=XLS
replace;
SHEET='2008';
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
proc nlin data=data maxiter=2000 method=newton outest=G_out;
parms b=0.1 A=0.45 mu=0.24 sigma=0.018; 
model NDVI= b + A *exp(-((DOY-1000*mu)/(1000*sigma))**2/2);
by unit;
run;
proc print data=g_out;
run;

data byunit; 
input unit marshtype$ location$ b A mu sigma MSe;
datalines;
1 Brackish Baratari 0.38332 0.12140 0.21036 0.031575 0.00126
2 Brackish BretonSo 0.43908 0.17792 0.19538 0.018000 0.00466
3 Brackish Pontchar 0.40732 0.12913 0.21303 0.041413 0.000952
4 Brackish Terrebon 0.34564 0.16841 0.18619 0.083997 0.00123
5 Freshwat Baratari 0.55203 0.10010 0.24000 0.018000 0.0114
6 Freshwat BretonSo 0.40922 0.29602 0.19562 0.052239 0.00425
7 Freshwat Pontchar 0.34355 0.33591 0.19000 0.070793 0.00346
8 Freshwat Terrebon 0.56638 0.15398 0.19269 0.018000 0.00637
9 Intermed Baratari 0.45777 0.15959 0.19394 0.018000 0.00527
10 Intermed BretonSo 0.42761 0.33511 0.21363 0.018000 0.0145
11 Intermed Pontchar 0.49720 0.06571 0.17082 0.020198 0.00724
12 Intermed Terrebon 0.48247 0.14245 0.19935 0.018000 0.00500
13 SalineMa Baratari 0.35999 0.03898 0.21502 0.027028 0.000718
14 SalineMa BretonSo 0.40650 0.03221 0.21729 0.039845 0.000625
15 SalineMa Pontchar 0.36623 0.07242 0.22847 0.036804 0.000739
16 SalineMa Terrebon 0.32179 0.09959 0.20737 0.075120 0.000730
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
parms b_b=0.3938400 A_b=0.1492150 mu_b=0.2012400 sigma_b=0.0437463
      b_f=0.4677950 A_f=0.2215025 mu_f=0.2045775 sigma_f=0.0397580
      b_i=0.4662625 A_i=0.1757150 mu_i=0.1944350 sigma_i=0.0185495
      b_s=0.3636275 A_s=0.0608000 mu_s=0.2170375 sigma_s=0.0446993
      /*s2e=0.0046333*/; 
B=(B_f*zf) + (B_i*zi) + (B_b*zb) + (B_s*zs) + u_b;
A=(A_f*zf) + (A_i*zi) + (A_b*zb) + (A_s*zs) + u_A;
mu=(mu_f*zf) + (mu_i*zi) + (mu_b*zb) + (mu_s*zs) + u_mu;
sigma=(sigma_f*zf) + (sigma_i*zi) + (sigma_b*zb) + (sigma_s*zs) + u_sigma;
pred =b + A *exp(-((DOY-1000*mu)/(1000*sigma))**2/2);
model NDVI ~ normal(pred,0.0042753);
random u_b u_a u_mu u_sigma ~normal([0,0,0,0],[0.0038949207,
-.0039810559,0.0068430772,
0.0003112303,-.0001236715,0.0002792202,
-.0010785152,0.0008650959,-.0001726277,0.0004857967]) 
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

