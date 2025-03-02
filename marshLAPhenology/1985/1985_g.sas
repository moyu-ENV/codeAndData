options nocenter;
/*IMPORT DATA*/
PROC IMPORT DATAFILE= "g:\data\LANDSAT.XLS" 
OUT=data_1 
DBMS=XLS
replace;
SHEET='19852';
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
parms b=0.2 A=0.3 mu=0.2 sigma=0.018; 
model NDVI = b + A *exp(-((DOY-1000*mu)/(1000*sigma))**2/2);
by unit;
run;
proc print data=g_out;
run;

data byunit; 
input unit marshtype$ location$ b A mu sigma MSe;
datalines;
1 Brackish Baratari 0.36979 0.1501 0.2000 0.0180 0.00397
2 Brackish BretonSo 0.41018 0.1133 0.2000 0.0180 0.00398
3 Brackish Pontchar 0.40897 0.1114 0.2000 0.0180 0.00418
4 Brackish Terrebon 0.34706 0.2222 0.1955 0.0603 0.000474
5 Freshwat Baratari 0.48923 0.6210 0.1822 0.0180 0.0155
6 Freshwat Pontchar 0.48338 0.8690 0.1779 0.0180 0.0114
7 Freshwat Terrebon 0.46150 0.2678 0.2107 0.0451 0.00368
8 Intermed Baratari 0.42635 0.2576 0.2008 0.0180 0.0110
9 Intermed BretonSo 0.36144 0.2579 0.2049 0.0180 0.00884
10 Intermed Pontchar 0.42778 0.2743 0.2055 0.0180 0.00761
11 Intermed Terrebon 0.42058 0.2389 0.2191 0.0338 0.00322
12 SalineMa Baratari 0.33969 0.0622 0.2000 0.0180 0.00162
13 SalineMa BretonSo 0.38833 0.0696 0.2062 0.0175 0.00180
14 SalineMa Pontchar 0.34874 0.0189 0.2091 0.0184 0.000519
15 SalineMa Terrebon 0.35801 0.1431 0.2000 0.0180 0.000768
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
parms b_b=0.3840000 A_b=0.1492500 mu_b=0.1988750 sigma_b=0.0285750
      b_f=0.4780367 A_f=0.5859333 mu_f=0.1902667 sigma_f=0.0270333
      b_i=0.4090375 A_i=0.4090375 mu_i=0.2075750 sigma_i=0.0219500
      b_s=0.3586925 A_s=0.0734500 mu_s=0.2038250 sigma_s=0.0179750
      /*s2e=0.0046333*/; 
B=(B_f*zf) + (B_i*zi) + (B_b*zb) + (B_s*zs) + u_b;
A=(A_f*zf) + (A_i*zi) + (A_b*zb) + (A_s*zs) + u_A;
mu=(mu_f*zf) + (mu_i*zi) + (mu_b*zb) + (mu_s*zs) + u_mu;
sigma=(sigma_f*zf) + (sigma_i*zi) + (sigma_b*zb) + (sigma_s*zs) + u_sigma;
pred =b + A *exp(-((DOY-1000*mu)/(1000*sigma))**2/2);
model NDVI ~ normal(pred,0.0052374);
random u_b u_a u_mu u_sigma ~normal([0,0,0,0],[0.0006992213,
0.0002955166,0.0181098940,
-.0000118808,-.0010348645,0.0000821481,
-.0001679213,-.0005312971,0.0000539998,0.0001835647]) 
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
