options nocenter;
/*IMPORT DATA*/
PROC IMPORT DATAFILE= "g:\data\LANDSAT.XLS" 
OUT=data2007_1 
DBMS=XLS
replace;
SHEET='2007';
GETNAMES=YES;
RUN;
PROC PRINT DATA=DATA2007_1;
RUN;
data data2007; 
set DATA2007_1;
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
proc print data=data2007;
run;
/*end of impoit data*/

/*logistic function*/
proc sort data=data2007;
by unit;
run;
Proc nlin data=data2007 maxiter=2000 method=Newton outest=L_out;
parms B =0.15 A =0.2  C_s =0.1  C_e =0.3 S_s = 0.5  S_e =0.5 R=0.220;
model NDVI = B + A/(1+(DOY<(1000*R))*exp(0.1*S_s*(1000*R-DOY-1000*C_s))+(DOY>(1000*R))*exp(0.1*S_e*(DOY-1000*R-1000*C_e)));
by unit;
run;
proc print data=L_out;
run;

data byunit; 
input unit marshtype$ location$ B A C_s C_e S_s S_e R MSe;
datalines;
1 Brackish Baratari 0.38546 0.10335 0.1 0.27846 0.32499 0.47327 0.22000 0.00225
2 Brackish BretonSo 0.42067 0.12132 0.1 0.29057 0.38724 0.47977 0.22000 0.00334
3 Brackish Pontchar 0.40463 0.12197 0.1 0.28515 0.47202 0.47968 0.22000 0.00244
4 Brackish Terrebon 0.41194 0.08785 0.1 0.28094 0.34713 0.47209 0.22000 0.00305
5 Freshwat Baratari 0.45485 0.17408 0.1 0.25430 0.53822 0.48358 0.22000 0.0149
6 Freshwat BretonSo 0.44408 0.21064 0.1 0.29738 0.39047 0.49617 0.22000 0.00306
7 Freshwat Pontchar 0.43243 0.16683 0.1 0.24870 0.58682 0.47993 0.22000 0.0118
8 Freshwat Terrebon 0.47960 0.17839 0.1 0.28865 0.40037 0.48448 0.22000 0.00751
9 Intermed Baratari 0.40289 0.13878 0.1 0.25448 0.47134 0.48622 0.22000 0.00553
10 Intermed BretonSo 0.40788 0.18325 0.1 0.29360 0.34164 0.48795 0.22000 0.00463
11 Intermed Pontchar 0.42383 0.14203 0.1 0.26498 0.49869 0.48182 0.22000 0.00602
12 Intermed Terrebon 0.42062 0.13638 0.1 0.28422 0.40563 0.47155 0.22000 0.00544
13 SalineMa Baratari 0.38395 0.03119 0.1 0.27121 0.50000 0.47129 0.22000 0.00157
14 SalineMa BretonSo 0.39731 0.10393 0.1 0.29177 0.25648 0.48268 0.22000 0.00167
15 SalineMa Pontchar 0.33345 0.12564 0.1 0.29264 0.41488 0.69086 0.18091 0.00144
16 SalineMa Terrebon 0.36996 0.02364 0.1 0.30000 0.28759 0.50000 0.22000 0.00149
RUN;
proc print data=byunit;
run;
PROC MEANS DATA = byunit MEAN VAR;
VAR B A C_s C_e S_s S_e R MSe;
RUN;
PROC MEANS DATA = byunit MEAN VAR;
VAR B A C_s C_e S_s S_e R MSe;
BY marshtype;
RUN; 
proc discrim data= byunit pcov;
class marshtype;
var B A C_s C_e S_s S_e R;
run;
Proc nlmixed data=data2007 method=firo;
if (marshtype='Freshwat') then do;z1=1; z2=0; z3=0; z4=0; end;
if (marshtype='Intermed') then do;z1=0; z2=1; z3=0; z4=0; end;
if (marshtype='Brackish') then do;z1=0; z2=0; z3=1; z4=0; end;
if (marshtype='SalineMa') then do;z1=0; z2=0; z3=0; z4=1; end;

parms B_b =0.4056750 A_b =0.1086225  C_b_s =0.1000000  C_b_e =0.2837800 S_b_s = 0.3828450  S_b_e =0.4762025 R_b=0.2200000
      B_f =0.4527400 A_f =0.1824850  C_f_s =0.1000000 C_f_e =0.2722575 S_f_s =0.4789700  S_f_e =0.4860400 R_f=0.2200000
      B_i =0.4138050 A_i =0.1501100 C_i_s =0.1000000  C_i_e =0.2743200 S_i_s =0.4293250  S_i_e =0.4818850 R_i=0.2200000
      B_s =0.3711675 A_s =0.0711000  C_s_s =0.1000000  C_s_e =0.2889050 S_s_s =0.3647375  S_s_e =0.5362075  R_s=0.2102275
      /*s2e=0.0080125*/;

B=(B_f*z1) + (B_i*z2) + (B_b*z3) + (B_s*z4) + u_B;
A=(A_f*z1) + (A_i*z2) + (A_b*z3) + (A_s*z4) + u_A;
S_s=(S_f_s*z1) + (S_i_s*z2) + (S_b_s*z3) + (S_s_s*z4) +u_S_s;
S_e=(S_f_e*z1) + (S_i_e*z2) + (S_b_e*z3) + (S_s_e*z4) +u_S_e;
C_s=(C_f_s*z1) + (C_i_s*z2) + (C_b_s*z3) + (C_s_s*z4) +u_C_s;
C_e=(C_f_e*z1) + (C_i_e*z2) + (C_b_e*z3) + (C_s_e*z4) +u_C_e;
R=(R_f*z1) + (R_i*z2) + (R_b*z3) + (R_s*z4) +u_R;

pred = B + A/(1+(DOY<(1000*R))*exp(0.1*S_s*(1000*R-DOY-1000*C_s))+(DOY>(1000*R))*exp(0.1*S_e*(DOY-1000*R-1000*C_e)));

model NDVI ~ normal(pred, 0.0047588);
random u_B u_A u_S_s u_S_e u_C_s u_C_e u_R~normal([0,0,0,0,0,0,0],[0.0003716100,
-.0001499713,0.0009418319,
0.0000000000,0.0000000000,0.0000000000,
0.0000520029,0.0002193448,0.0000000000,0.0002725492,
-.0003996736,-.0006863762,0.0000000000,-.0011111758,0.0079212639,
-.0006750183,0.0009901281,0.0000000000,0.0001311483,0.0005430209,0.0027224178,
0.0001228648,-.0001776641,0.0000000000,-.0000121668,-.0001633392,-.0005037805,0.0000955018]) subject=unit;

contrast 'B F VS I' b_f, b_i;
contrast 'B F VS B' b_f, b_b;
contrast 'B F VS S' b_f, b_s;
contrast 'B I VS B' b_i, b_b;
contrast 'B I VS S' b_i, b_s;
contrast 'B B VS S' b_b, b_s;

contrast 'A F VS I' A_f, A_i;
contrast 'A F VS B' A_f, A_b;
contrast 'A F VS S' A_f, A_s;
contrast 'A I VS B' A_i, A_b;
contrast 'A I VS S' A_i, A_s;
contrast 'A B VS S' A_b, A_s;

contrast 'S F Start VS End' S_f_s, S_f_e;
contrast 'S I Start VS End' S_i_s, S_i_e;
contrast 'S S Start VS End' S_b_s, S_b_e;
contrast 'S C Start VS End' S_s_s, S_s_e;

contrast 'C_s F VS B' C_f_s, C_b_s;
contrast 'C_s F VS S' C_f_s, c_s_s;
contrast 'C_s I VS B' C_i_s, C_b_s;
contrast 'C_s I VS S' C_i_s, C_s_s;
contrast 'C_s B VS S' C_b_s, C_s_s;

contrast 'C_e F VS B' C_f_e, C_b_e;
contrast 'C_e F VS S' C_f_e, c_s_e;
contrast 'C_e I VS B' C_i_e, C_b_e;
contrast 'C_e I VS S' C_i_e, C_s_e;
contrast 'C_e B VS S' C_b_e, C_s_e;

contrast 'R F VS B' R_f, R_b;
contrast 'R F VS S' R_f, R_s;
contrast 'R I VS B' R_i, R_b;
contrast 'R I VS S' R_i, R_s;
contrast 'R B VS S' R_b, R_s;

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

