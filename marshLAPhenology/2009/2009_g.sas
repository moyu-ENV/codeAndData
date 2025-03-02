options nocenter;
/*IMPORT DATA*/
PROC IMPORT DATAFILE= "g:\landsat\data\LANDSAT.XLS" 
OUT=data_1 
DBMS=XLS
replace;
SHEET='2009';
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
parms b=0.1 A=0.45 mu=0.23 sigma=0.018; 
model NDVI= b + A *exp(-((DOY-1000*mu)/(1000*sigma))**2/2);
by unit;
run;
proc print data=g_out;
run;

data byunit; 
input unit marshtype$ location$ b A mu sigma MSe;
datalines;
1 Brackish Baratari 0.42506 0.14087 0.23584 0.018000 0.00452
2 Brackish BretonSo 0.43274 0.13913 0.24342 0.018000 0.00381
3 Brackish Pontchar 0.43668 0.13228 0.24510 0.018000 0.00315
4 Brackish Terrebon 0.42519 0.12176 0.23783 0.018000 0.00406
5 Freshwat Baratari 0.53053 0.17046 0.22868 0.018000 0.0188
6 Freshwat BretonSo 0.50640 0.18441 0.23108 0.018000 0.00895
7 Freshwat Pontchar 0.48155 0.26493 0.22772 0.018000 0.00819
8 Freshwat Terrebon 0.55688 0.18365 0.23142 0.018000 0.0121
9 Intermed Baratari 0.46907 0.15997 0.23410 0.018000 0.00902
10 Intermed BretonSo 0.43212 0.20406 0.23181 0.018000 0.00893
11 Intermed Pontchar 0.49117 0.17816 0.23158 0.018000 0.00497
12 Intermed Terrebon 0.48599 0.15228 0.23140 0.018000 0.00716
13 SalineMa Baratari 0.37184 0.09838 0.24430 0.018000 0.00371
14 SalineMa BretonSo 0.42532 0.02745 0.23609 0.028224 0.00262
15 SalineMa Pontchar 0.38329 0.04914 0.23599 0.035330 0.00169
16 SalineMa Terrebon 0.34775 0.08907 0.24687 0.018000 0.00299
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
parms b_b=0.4299175 A_b=0.1335100 mu_b=0.2405475 sigma_b=0.0180000
      b_f=0.5188400 A_f=0.2008625 mu_f=0.2297250 sigma_f=0.0180000
      b_i=0.4695875 A_i=0.1736175 mu_i=0.2322225 sigma_i=0.0180000
      b_s=0.3820500 A_s=0.0660100 mu_s=0.2408125 sigma_s=0.0248885
      /*s2e=0.0046333*/; 
B=(B_f*zf) + (B_i*zi) + (B_b*zb) + (B_s*zs) + u_b;
A=(A_f*zf) + (A_i*zi) + (A_b*zb) + (A_s*zs) + u_A;
mu=(mu_f*zf) + (mu_i*zi) + (mu_b*zb) + (mu_s*zs) + u_mu;
sigma=(sigma_f*zf) + (sigma_i*zi) + (sigma_b*zb) + (sigma_s*zs) + u_sigma;
pred =b + A *exp(-((DOY-1000*mu)/(1000*sigma))**2/2);
model NDVI ~ normal(pred,0.0065419);
random u_b u_a u_mu u_sigma ~normal([0,0,0,0],[0.0007101775,
-.0006136327,0.0008967679,
-.0000234831,0.0000293464,0.0000139618,
0.0000386568,-.0000572162,-.0000109881,0.0000179211]) 
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

