/*======================================================================
*
* STUDY                : Code snippet: Mutation Overview Plot
*
* CREATION DATE        : 29Jun2016
*
* PROGRAM              : heatmap_macro.sas
*
* CURRENT VERSION      : 1
*
* PROGRAM LOCATION     :
*
* PROGRAMMER           : EXT Jari Ahvenainen
*
* DESCRIPTION          : Creates a mutation overview plot with SAS GTL
*
* USER REQUIREMENTS    :
*
* SAS VERSION
*   in which validated :
*
* LIMITATIONS          :
*
* OPERATING SYSTEM
*   which is used locally: WINDOWS NT
*   which is used remotely:
*
* FILES USED           :
*
*
* FILES CREATED        :
*
* EXTERNAL PROGRAMS
*   OR MACROS USED     :
*
* COMMENTS             :
*
*-----------------------------------------------------------------------
*
* REVISION HISTORY (COPY FOR EACH REVISION):
*
* DATE                 :
* PROGRAMMER           :
* REASON FOR CHANGE    :
*=======================================================================*/;


/* Creating an example data */
%macro RandBetween(min, max);
   (&min + floor((1+&max-&min)*rand("uniform")))
%mend;
/* X-axis variable */
data random;
array pat[25] pat1-pat25;
  do j = 1 to 16;
    do i = 1 to 25;
     pat[i] = %RandBetween(0, 1);
if j in (10,11,12) then pat[i]=%RandBetween(0, 2);
if pat[i]=2 then pat[i]=0.5;
    end;
  output;
  end;
run;
data categories;
input j catn;
datalines;
1 1
2 1
3 1
4 1
5 2
6 2
7 2
8 3
9 3
10 3
11 3
12 3
13 3
14 3
15 3
16 3
;
run;
proc format;
  value groupf
  1="Drug A"
  2="Drug B"
  ;
  value catf
  1="Baseline"
  2="SCNA"
  3="SNV"
  ;
  value subcatf
  1="T790M"
  2="Ex19del"
  3="L959R"
  4="Ex20Ins"
  5="MET"
  6="ERBB2"
  7="EGFR"
  8=" EGFR"
  9="PIK3CA"
  10="KRAS"
  11="CDKN2"
  12="RB1"
  13="ALK"
  14="KIT"
  15=" MET"
  16="Other"
  ;
run;
data testdata (keep=subcatn subcat catn cat pat1--patc25);
  merge categories random;
  by j;
  subcatn=j;
  cat=put(catn,catf.);
  subcat=put(subcatn, subcatf.);
run;
proc sort data=testdata;
  by catn subcatn;
run;

/* Format of input data is defined: */
/* Variable: CATN --> Numeric presentation of main category for variables shown on Y-axis (1,2,...n) */
/* Variable: CAT --> Charecter presentation of main category for variables shown on Y-axis (1:1 with CATN) */
/* Variable: SUBCATN --> Numeric presentation of sub category for variables shown on Y-axis (1,2,...n) */
/* Variable: SUBCAT --> Charecter presentation of sub category for variables shown on Y-axis (1:1 with CATN) */
/* Other variables: Variables that are shown in X-axis, i.e. Patients */
/*   These variables will be populated with reported values (0 or 0.5 or 1) --> 0 missing, 0.5 absence, 1 presence */
/* */
/* In case there is no need for subcategory, please populate variables CAT and CATN with number 1 + */
/* use SUBCAT and SUBCATN as a category variables */


/* MACRO DATAPREP: */
/*  __indata: SAS dataset name used for the graph (Including libname: i.e. __indata=WORK.TESTDATA) */
/*  __patvars: Variable names that contain reported values (i.e. __patvars=pat101 pat102 pat103 pat104) */

%MACRO DATAPREP (__indata=, __patvars=, marker0=, markerh=, marker1=);
%global CATCNT SUBCATCNT;
ods listing close;
proc sort data=&__indata.;
  by catn subcatn;
run;

/* Creating a new variable to measure how many rows there are per category */
data &__indata.;
  retain catwth;
  set &__indata.;
  by catn subcatn;
  if first.catn then catwth=0;
  catwth=catwth+1;
run;

proc transpose data=&__indata. out=exaxis (rename=(_NAME_=patno col1=aval));
var &__patvars.;
by catn subcatn cat subcat catwth;
run;

/* Calculating the percentage of "Presence" to be able to create barcharts on the right side of the graph */
ods output crosstabfreqs=tabs (where=(aval=1 and frequency ge 1 and rowpercent >.z ) keep=catn subcatn cat subcat catwth rowpercent aval frequency);
proc freq data=exaxis;
  table catn*subcatn*cat*subcat*catwth*aval;
run;
proc transpose data=tabs out=perc (rename=(_NAME_=rowper col1=perc));
var rowpercent;
by catn subcatn cat subcat catwth;
run;
data indata;
  length patno $200;
  set exaxis perc;
run;
proc sort data=indata;
by catn subcatn cat subcat catwth;
run;

/* Number of categories */
proc sort data=indata out=catego (keep=catn) nodupkey;
  by catn;
run;
proc sql noprint; select count(*) into :CATCNT from catego;quit;
%PUT &CATCNT.;
/* Number of sub-categories */
proc sort data=indata out=scatego (keep=catn subcatn catwth cat) nodupkey;
  by catn subcatn;
run;
proc sql noprint; select count(*) into :SUBCATCNT from scatego;quit;
%PUT &SUBCATCNT.;

/* Data manipulation */
/* To be able to use different colors within same graph */
/*  Reported values are scaled between 0-1 for category1, 2-3 for cat2, 4-5 for cat3 and 6-7 for cat4 */
data tograph;
  length marker $2.;
  set indata;
  if aval=0 then marker="&marker0.";
  if aval=0.5 then marker="&markerh.";
  if aval=1 then marker="&marker1.";
  aval=aval+(2*catn)-2;
  perc=perc/100;
  if catn=1 then perc1=perc;
  if catn=2 then perc2=perc;
  if catn=3 then perc3=perc;
  if catn=4 then perc4=perc;
  if subcatn=16 then do;
    pchgb = rand("normal", 0,30);
if marker="AA" then group=1;
else group=2;
  end;
  format group groupf.;
run;
proc sort data=TOGRAPH;
  by descending subcatn;
run;
%MEND DATAPREP;
%DATAPREP(__indata=testdata, __patvars=pat1--pat25, marker0=AA, markerh=AC, marker1=CC);


options mprint source source2;
%MACRO HEATGRA (ystart=, yend=, subcat=, barch=, colors=, symbols=);
  %global len line1 line2 line3 line4 newend __line1 __line2 __line3 __line4 __text1 __text2 __text3 __text4 label1 label2 label3 label4;
  %let newend=%STR(&yend.);
    %IF &SUBCAT.=1 %THEN %DO;
    %do i=1 %to &catcnt.;
/* Calculating correct start and end point for lines of Y-axis */
    data _null_;
      set scatego (where=(catn=&i.));
 by catn subcatn;
 a=floor(catwth*(&yend.-&ystart.)/&subcatcnt.);
 if last.subcatn then do;
        call symput("LINE&i.", a);
call symput("LABEL&i.",trim(left(cat)));
 end;
    run;
    /*  Creating macro variables for Ctaegory labels and lines */
data _null_;
 length __line __text $200.;
 start=min(&newend.,100);
 end=max(start-&&line&i.+1,1);
 call symput('newend', end-1);
 __line="drawline x1=70 y1="||compress(start)||" x2=70 y2="||compress(end)||";";
 __text="drawtext  textattrs=(color=Black size=6pt weight=bold) '&&label&i' / x=0 y="||compress(start-(start-end)/2)||" anchor=center justify=center;";
 call symput("__line&i.", __line);
 call symput("__text&i.", __text);
 output;
run;    
%end;
%END;
  %IF &BARCH.=1 %THEN %DO;
    %LET col2=0.8;
%LET col3=0.15;
  %END;
  %ELSE %DO;
    %LET col2=0.94;
%LET col3=0.01;
  %END;
proc template;
  define statgraph heatmap;
    begingraph / ;
rangeattrmap name="rmap";
%IF &COLORS=1 %THEN %DO;
     range 0-1 / rangecolormodel=(white LIBRGR   lightgreen);
 range 2-3 / rangecolormodel=(white LIBRGR   lightred);
 range 4-5 / rangecolormodel=(white LIBRGR   lightblue);
 range 6-7 / rangecolormodel=(white LIBRGR   lightbrown);
%END;
%ELSE %DO;
     range 0-1 / rangecolormodel=(white white white);
 range 2-3 / rangecolormodel=(white white white);
 range 4-5 / rangecolormodel=(white white white);
 range 6-7 / rangecolormodel=(white white white);
%END;
   endrangeattrmap;
      discreteattrmap name='colors' / ignorecase=true;
         value "BI IMP" / fillattrs=(color=lightgreen);
         value "Placebo" / fillattrs=(color=lightblue);
      enddiscreteattrmap;

      /* Associate the attribute map with input data column DEPT */
      /* and assign the name DEPTCOLORS to the named association */
      discreteattrvar attrvar=grpcolors var=group attrmap='colors';

   rangeattrvar attrmap="rmap" var=aval attrvar=cvar;
layout lattice / columns=3 rows=2 rowgutter=0px columnweights=(.05 &col2. &col3.) rowweights=(0.3 0.7) border=false;
layout overlay  / walldisplay=none;
        drawtext "";
endlayout;
layout overlay / xaxisopts=(display=none)
    yaxisopts =(label = "Change from baseline (%)" labelattrs=(size=8pt) LINEAROPTS=( viewmin = -60 viewmax = 60 ));
    BarChart X=patno Y=pchgb / primary=true Group=grpcolors includemissinggroup=false barlabel=true Display=( Fill ) NAME="VBAR"
BARLABELFORMAT=5.1 ;
    endlayout;
layout overlay  / walldisplay=none;
        DiscreteLegend "VBAR"/ title="Treatment";
endlayout;
  layout overlay  / walldisplay=none;
        drawtext "";
 %IF &SUBCAT.=1 %THEN %DO;
        %IF &CATCNT ge 1 %THEN &__line1.;
        %IF &CATCNT ge 2 %THEN &__line2.;
%IF &CATCNT ge 3 %THEN &__line3.;
%IF &CATCNT ge 4 %THEN &__line4.;
        %IF &CATCNT ge 1 %THEN &__text1.;
        %IF &CATCNT ge 2 %THEN &__text2.;
%IF &CATCNT ge 3 %THEN &__text3.;
        %IF &CATCNT ge 4 %THEN &__text4.;
 %END;
 endlayout;
 layout overlay  / walldisplay=none
yaxisopts=(display=(tickvalues) tickvalueattrs=(size=7)  discreteopts=(tickvaluefitpolicy=SPLITALWAYS tickvaluesplitchar=","))
                        x2axisopts=(display=(tickvalues) tickvalueattrs=(size=6) discreteopts=(tickvaluefitpolicy=EXTRACTALWAYS));
   heatmapparm x=patno y=subcat colorresponse=cvar /  name="rmap" display=(fill) outlineattrs=(color=black) xaxis=x2 xgap=10 ygap=10;
%IF &SYMBOLS.=1 %THEN %DO; scatterplot x=patno y=subcat / markercharacter=marker markercharacterattrs=(size=6) xaxis=x2; %END;

 endlayout;
      layout overlay / walldisplay=none
  yaxisopts=(display=none)
                       xaxisopts=(display=none);
        drawtext "";
   %IF &BARCH.=1 %THEN %DO;
          %IF &CATCNT ge 1 %THEN barchart category=subcat  response=perc1 /  display=(fill) fillattrs=(color=lightgreen) primary=true orient=horizontal barlabel=true barlabelformat=percent7.0;;
 %IF &CATCNT ge 2 %THEN barchart category=subcat  response=perc2 / display=(fill) fillattrs=(color=lightred) primary=true orient=horizontal barlabel=true barlabelformat=percent7.0;;
 %IF &CATCNT ge 3 %THEN barchart category=subcat  response=perc3 / display=(fill) fillattrs=(color=lightblue) primary=true orient=horizontal barlabel=true barlabelformat=percent7.0;;
 %IF &CATCNT ge 4 %THEN barchart category=subcat  response=perc4 / display=(fill) fillattrs=(color=black) primary=true orient=horizontal barlabel=true barlabelformat=percent7.0;;
%END;
      endlayout;

endlayout;
endgraph;
  end;
run;

ods listing  style=listing_bi image_dpi=400 gpath="Z:\Temp" ;
ods graphics on / reset=index imagename="Heat"  imagefmt=png;
  proc sgrender data=tograph template=heatmap; run;
ods graphics off;
%MEND HEATGRA;

%HEATGRA(ystart=0, yend=92, subcat=1, barch=1, colors=1, symbols=1);