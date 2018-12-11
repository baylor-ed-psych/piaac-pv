PROC IMPORT OUT= WORK.PIAAC_data 
            DATAFILE= "C:\Users\noah_padgett1\Desktop\PIAAC_PV\Data\PIAA
C_household_reduced_2018_06_15.sav" 
            DBMS=SPSS REPLACE;

RUN;
