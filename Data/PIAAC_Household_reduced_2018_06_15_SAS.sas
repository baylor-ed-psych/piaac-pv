

libname library 'C:\Users\noah_padgett1\Desktop\PIAAC_PV\Data\' ;

proc format library = library ;
   value CNTRYID
      36 = 'Australia'  
      40 = 'Austria'  
      56 = 'Belgium'  
      124 = 'Canada'  
      196 = 'Cyprus'  
      203 = 'Czech Republic'  
      208 = 'Denmark'  
      233 = 'Estonia'  
      246 = 'Finland'  
      250 = 'France'  
      276 = 'Germany'  
      372 = 'Ireland'  
      380 = 'Italy'  
      392 = 'Japan'  
      410 = 'Korea'  
      528 = 'Netherlands'  
      578 = 'Norway'  
      616 = 'Poland'  
      643 = 'Russian Federation'  
      703 = 'Slovak Republic'  
      724 = 'Spain'  
      752 = 'Sweden'  
      826 = 'United Kingdom'  
      840 = 'United States'  
      84091 = 'United States (16-74 y.o. sample)'  
      84092 = 'United States (prison sample)' ;
   value GENDER_R
      1 = 'Male'  
      2 = 'Female'  
      9 = 'Not stated or inferred' ;
   value PVNUM1F
      9999 = 'Not stated or inferred' ;
   value PVNUM2F
      9999 = 'Not stated or inferred' ;
   value PVNUM3F
      9999 = 'Not stated or inferred' ;
   value PVNUM4F
      9999 = 'Not stated or inferred' ;
   value PVNUM5F
      9999 = 'Not stated or inferred' ;
   value PVNUM6F
      9999 = 'Not stated or inferred' ;
   value PVNUM7F
      9999 = 'Not stated or inferred' ;
   value PVNUM8F
      9999 = 'Not stated or inferred' ;
   value PVNUM9F
      9999 = 'Not stated or inferred' ;
   value PVNUM10F
      9999 = 'Not stated or inferred' ;
   value $VEMETHOD
     'JK1' = 'Jackknife 1'  
     'JK2' = 'Jackknife 2'  
     'BRR' = 'Balanced Repeated Replication'  
     'FAY' = 'Balanced Repeated Replication w Fay''s adjustment' ;
   value VEMETHODN
      1 = 'JK1 - Jackknife 1'  
      2 = 'JK2 - Jackknife 2'  
      3 = 'BRR - Balanced Repeated Replication'  
      4 = 'FAY - Balanced Repeated Replication w Fay''s adjustment' ;

proc datasets library = library ;
modify piaac_household_reduced_2018_06_15_sas;
   format   CNTRYID CNTRYID.;
   format  GENDER_R GENDER_R.;
   format    PVNUM1 PVNUM1F.;
   format    PVNUM2 PVNUM2F.;
   format    PVNUM3 PVNUM3F.;
   format    PVNUM4 PVNUM4F.;
   format    PVNUM5 PVNUM5F.;
   format    PVNUM6 PVNUM6F.;
   format    PVNUM7 PVNUM7F.;
   format    PVNUM8 PVNUM8F.;
   format    PVNUM9 PVNUM9F.;
   format   PVNUM10 PVNUM10F.;
   format  VEMETHOD $VEMETHOD.;
   format VEMETHODN VEMETHODN.;
quit;
