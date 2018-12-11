/* version 2., updated 25-07-2014 */
/***********************************/


%macro PIAAC_TOOL(method= mean,
					table= Prgusap1, 
					wgt=SPFWT0 , 
					rwgt=SPFWT , 
					byvar= CNTRYID,
					nrep=80 , nb_pv=1 , 
					dvar= PVNUM, dvarpv=1 , 
				  	vemethod=VEMETHOD,
				  	ficout= output_attempt.xls ,
					path_out= C:\Users\noah_padgett1\Desktop\SAS Macro Attempts\, 
					excel_sheet=sas_output, 
					dbms=excel5);


	


/******************************************************************************************************************/
/******************************************************************************************************************/
/**                                    0-  automatic macro variables                                             **/
/******************************************************************************************************************/
/******************************************************************************************************************/
	

/* automatic macro variables*/
/****************************/

* number of variables defined in BYVAR list;
%let nb_byvar=0;
%IF &byvar ne %THEN %DO ; 
	%let nb_byvar=%sysfunc(countw(&byvar));
	
%END;

*number byvar with plausible values;
%let nb_byvarpv=0;
%IF &byvarpv ne %THEN %DO ; 
%let nb_byvarpv=%sysfunc(countw(&byvarpv));
%END;

*number BYVAR including PV;
%LET nb_byvartot=%EVAL(&nb_byvar.+&nb_byvarpv);

*List byvar and byvarpv;
%LET byvartot=&byvar. &byvarpv.; 

* total number of variables (dvar et byvar);

	%let nb_dvar=0;
	%let iter=1;
	%DO %WHILE (%SCAN(&dvar,&iter) ne );
		%let nb_dvar=%EVAL(&nb_dvar+1);
	  	%let iter=%EVAL(&iter+1);
	%END;

	%let nb_all_var= %EVAL(&nb_byvar + &nb_dvar);

	%let list_all_var=&dvar &byvar;

	%put &nb_all_var &list_all_var;

 * Coefficient PV;
	%IF &nb_pv. ne 1 %THEN %DO;
	%let coef_pv=%sysevalf((&nb_pv.+1)/((&nb_pv.-1)*&nb_pv.));
	%END;

*Coefficient jk;
	/*%IF &nrep. ne %THEN %DO;
	%let jkcoeff_corr=%sysevalf((&nrep.-1)/(&nrep.));
	%END;
*/

*number percentile;

%if &percentile ne  %then %do;
%let nb_perc=%sysfunc(countw(&percentile));
%end;

*number ivar pv;
%IF &ivarpv ne %THEN %DO ; 
%let nb_ivarpv=%sysfunc(countw(&ivarpv));
%END;

*number clvar pv;
%IF &clvarpv ne %THEN %DO ; 
%let nb_clvarpv=%sysfunc(countw(&clvarpv));
%END;


/******************************************************************************************************************/
/******************************************************************************************************************/
/**                                    1-  Copy of the dataset and sort                                          **/
/******************************************************************************************************************/
/******************************************************************************************************************/

/* Copy of the input dataset in work librairy, named TEMP_(name of the input dataset)
and sort by the variables listed in the macro-variable byvar */
/*************************************************************************************/
	

DATA TEMP_&table.;
	SET &table.;

	/*Filter*/
	%IF (&where_cond) ne %THEN %DO;
		where &where_cond. ;
	%end;

	KEEP CNTRY_OUT CNTRY_E SEQID VENREPS &VEMETHOD. &wgt &RWGT.1--&RWGT.&NREP. &dvar.: &ivar. &byvar. 

	%IF (&IVARPV ne ) %THEN %DO;

		%DO i=1 %TO &nb_pv. ;

			%DO j=1 %TO &nb_ivarpv;
					%SCAN(&ivarpv,&j)&i.
			%END;
		%END;

	%END;

	%IF (&BYVARPV ne ) %THEN %DO;

		%DO i=1 %TO &nb_pv. ;

			%DO j=1 %TO &nb_byvarpv;
					%SCAN(&byvarpv,&j)&i.
			%END;
		%END;

	%END;

	;RUN;	

	%IF (&byvar ne ) and (&byvarpv.= )  %THEN %DO;	
		PROC SORT DATA=TEMP_&table. out=TEMP_&table. ;
			BY &byvar. ;
		RUN;
	%END;




/******************************************************************************************************************/
/******************************************************************************************************************/
/**                         2.1- COMPUTATION : CASE WITH PLAUSIBLE VARIABLES                                     **/
/******************************************************************************************************************/
/******************************************************************************************************************/

	%IF (&DVARPV=1) or (&IVARPV ne ) or (&BYVARPV ne ) %THEN %DO;

		%DO i=1 %TO &nb_pv. ;

		%IF &byvarpv. ne %THEN %DO;	
		PROC SORT DATA=TEMP_&table. out=TEMP_&table.;
			BY &byvar. %DO j=1 %TO &nb_byvarpv;
								%SCAN(&byvarpv,&j)&i.;
						%END;;
		RUN;
		%END;


			%IF %UPCASE(&method)=MEAN %THEN %DO;
				PROC SURVEYMEANS data=TEMP_&table. varmethod=jackknife mean nobs SUMWGT 
												%IF &percentile ne  %THEN %DO; 
												percentile=(&percentile.) 
												%end;;
				VAR 

				%IF (&DVARPV=1)%THEN %DO;
				&dvar.&i.
				%END;
				%IF (&DVARPV=0)%THEN %DO;
				&dvar.
				%END;;
								
			%END;

			%ELSE %IF %UPCASE(&method)=FREQ %THEN %DO;
				PROC SURVEYFREQ data=TEMP_&table. varmethod=jackknife ;
				TABLE 

				%IF (&DVARPV=1)%THEN %DO;
				&dvar.&i.
				%END;
				%IF (&DVARPV=0)%THEN %DO;
				&dvar.
				%END;;


			%END;

			%ELSE %IF %UPCASE(&method)=REG %THEN %DO;
				PROC SURVEYREG DATA=TEMP_&table. varmethod=jackknife;
				MODEL 


				%IF (&DVARPV=1)%THEN %DO;
				&dvar.&i.
				%END;
				%IF (&DVARPV=0)%THEN %DO;
				&dvar.
				%END;

				= &Ivar. %IF &ivarpv ne %THEN %DO ;
											%DO j=1 %TO &nb_ivarpv;
											%SCAN(&ivarpv,&j)&i.
											%END;;
						%END;
										;
			%END;

			%ELSE %IF %UPCASE(&method)=LOGISTIC %THEN %DO;
				PROC SURVEYLOGISTIC DATA=TEMP_&table. varmethod=jackknife ;

				%if %SCAN(&CLVAR,1) ne  or %scan(&clvarpv,1) ne  %then %do;
					CLASS &clvar.  
							%IF &clvarpv ne %THEN %DO ;
								%DO j=1 %TO &nb_clvarpv;
								%SCAN(&clvarpv,&j)&i.(%SCAN(&refclvarpv,&j)) 
								%END;
							%END;
					/ param=ref;
				%end;				
				MODEL 
					%IF (&DVARPV=1)%THEN %DO;
					&dvar.&i.
					%END;
					%IF (&DVARPV=0)%THEN %DO;
					&dvar.
					%END;
				(EVENT="&event.") = &Ivar.  %IF &ivarpv ne %THEN %DO ;
																%DO j=1 %TO &nb_ivarpv;
																%SCAN(&ivarpv,&j)&i.
																%END;
															 %END;


					%IF (&PROBIT=1) %THEN %DO;
						/ link=probit 
					%END;
										;
			%END;

				weight &wgt.;

			%if (&byvar ne ) or (&byvarpv ne ) %then %do;
				by 
				%IF &byvar ne %THEN %DO ;
				&byvar. %END;  %IF &byvarpv ne %THEN %DO ;
											%DO j=1 %TO &nb_byvarpv;
											%SCAN(&byvarpv,&j)&i.
											%END;
						%END;

					&vemethod. VENREPS
							;

						
			%end;



				repweight &RWGT.1--&RWGT.&NREP. /  jkcoefs=1;

		
			%IF %UPCASE(&method)=MEAN %THEN %DO;
				ods output statistics=temp&i.(rename= mean=mean_&i.  drop= VarName );
			%END;
			%IF %UPCASE(&method)=FREQ and (&DVARPV=1) %THEN %DO;
				ods output Oneway=temp&i.(rename= (Percent=Percent_&i. Frequency=Frequency_&i. WgtFreq=WgtFreq_&i. StdErr=StdErr_&i. &dvar.&i.=&dvar.)  drop= table StdDev _SkipLine );
			%END;

			%IF %UPCASE(&method)=FREQ and (&DVARPV=0) %THEN %DO;
				ods output Oneway=temp&i.(rename= (Percent=Percent_&i. Frequency=Frequency_&i. WgtFreq=WgtFreq_&i. StdErr=StdErr_&i.)  drop= table StdDev _SkipLine );
			%END;

			%IF %UPCASE(&method)=REG or %UPCASE(&method)=LOGISTIC %THEN %DO;
				ods output ParameterEstimates= temp&i.(rename= (Estimate=Estimate_&i. StdErr=StdErr_&i.  ) )

				%IF &R2=1 %THEN %DO; /*compute R2 */

				FitStatistics=R2_&i.

				%END;;
			
			%END;


			%IF %UPCASE(&PREDICT)=1 %THEN %DO;
				output out=&table_pred._&i. predicted=predicted residual=residual std=se_pred;
			
			%END;

			
			run;

		%IF %UPCASE(&method)=MEAN %THEN %DO;

				DATA temp&i.; SET temp&i.;
					RENAME
					StdErr=StdErr_&i.

			%IF &percentile ne  %THEN %DO;

				
					%DO p=1 %TO &nb_perc;
						%let perc=%SCAN(&percentile,&p);

						 Pctl_&perc.=Pctl_&perc._&i. 
						 StdErr_Pctl_&perc.=StdErr_Pctl_&perc._&i.

					%END; 

					;
					DROP  LCL: UCL: 
			%END;
					
				;RUN;
			%END;

			%IF &ivarpv. ne  %THEN %DO;

				DATA temp&i.; SET temp&i.;
					%DO j=1 %TO &nb_ivarpv;
						%LET temp_ivar=%SCAN(&ivarpv,&j);
						%IF %UPCASE(&method)=LOGISTIC %THEN %DO;
						IF upcase(variable)=upcase("&temp_ivar.&i.") THEN variable="&temp_ivar.";
						%END;
						%IF  %UPCASE(&method)=REG %THEN %DO;
						IF upcase(Parameter)=upcase("&temp_ivar.&i.") THEN Parameter="&temp_ivar.";
						%END;
	
					%END;
				RUN;
					
			%END;

			%IF &byvarpv. ne  %THEN %DO;

				DATA temp&i.; SET temp&i.;
					%DO j=1 %TO &nb_byvarpv;
						%LET temp_byvar=%SCAN(&byvarpv,&j);
						
						RENAME &temp_byvar.&i.=&temp_byvar.;
						
	
					%END;
				RUN;
					
			%END;



		%END;

	
			PROC SQL;
			CREATE TABLE resul AS
			
			%IF %UPCASE(&method)=MEAN %THEN %DO;

				SELECT a1.* ,

				%do j=2 %to &nb_pv.;

					a&j..mean_&j.,
					a&j..StdErr_&j.,

					%if &percentile ne  %then %do;
						%DO p=1 %TO &nb_perc;
							%let perc=%SCAN(&percentile,&p);

							a&j..Pctl_&perc._&j., 
							a&j..StdErr_Pctl_&perc._&j., 

						%END; 
					%end;

					

				%end;	

				(mean_1

				%do k=2 %to &nb_pv.;

					+mean_&k.

				%end;

				 )/&nb_pv. as mean_m ,


				 (StdErr_1**2

				%do k=2 %to &nb_pv.;

					+StdErr_&k.**2

				%end;

				 )/&nb_pv. as StdErrsquared_m


		%IF &percentile ne  %THEN %DO;

				%DO p=1 %TO &nb_perc;
					%let perc=%SCAN(&percentile,&p);
					 

					 , (Pctl_&perc._1 

					%do k=2 %to &nb_pv.;

						+Pctl_&perc._&k.

					%end;

					 )/&nb_pv. as Pctl_&perc._m

				%end;	
				%DO p=1 %TO &nb_perc;
					%let perc=%SCAN(&percentile,&p);
					 

					 , (stderr_Pctl_&perc._1**2 

					%do k=2 %to &nb_pv.;

						+stderr_Pctl_&perc._&k.**2

					%end;

					 )/&nb_pv. as stderr_Pctl_&perc._m

				%end;
		%end;				
					



			FROM temp1 as a1

		%END;



			%IF %UPCASE(&method)=FREQ %THEN %DO;
			
			SELECT

			%IF &byvartot. ne %THEN %DO;
		
				%DO i=1 %TO &nb_byvartot.;

				%LET var=%SCAN(&byvartot.,&i.);

				coalesce(a1.&var.
				%do p=2 %to &nb_pv.;
					, a&p..&var.
				%end;

			) as &var.,

				%end;
			%END;

			

			coalesce(a1.VEMETHOD
				%do p=2 %to &nb_pv.;
					, a&p..VEMETHOD
				%end;

			) as VEMETHOD,

			coalesce(a1.VENREPS
				%do p=2 %to &nb_pv.;
					, a&p..VENREPS
				%end;

			) as VENREPS,

			coalesce(a1.&dvar.
				%do p=2 %to &nb_pv.;
					, a&p..&dvar.
				%end;

			) as &dvar.,


				%do j=1 %to &nb_pv.;

					ifn(a&j..Percent_&j. is missing,0,a&j..Percent_&j.) as Percent_&j. ,
					a&j..StdErr_&j.,
					a&j..Frequency_&j.,
					a&j..WgtFreq_&j.,

				%end;	

			 	

				 ( IFN(a1.Frequency_1 is missing,0,a1.Frequency_1)

				%do k=2 %to &nb_pv.;

					+IFN(a&k..Frequency_&k. is missing,0,a&k..Frequency_&k.)

				%end;

				 )/&nb_pv. as Frequency ,

				( IFN(a1.WgtFreq_1 is missing,0,a1.WgtFreq_1)

				%do k=2 %to &nb_pv.;

					+ IFN(a&k..WgtFreq_&k. is missing,0,a&k..WgtFreq_&k.)

				%end;

				 )/&nb_pv. as WgtFreq ,

				 (ifn(a1.Percent_1 is missing,0,a1.Percent_1)

				%do k=2 %to &nb_pv.;

					+ifn(a&k..Percent_&k. is missing,0,a&k..Percent_&k.)

				%end;

				 )/&nb_pv. as Percent_m ,


				(ifn(a1.StdErr_1 is missing,0,a1.StdErr_1)**2

				%do k=2 %to &nb_pv.;

					+ifn(a&k..StdErr_&k. is missing,0,a&k..StdErr_&k.)**2

				%end;

				 )/&nb_pv. as StdErrsquared_m 


				
			FROM temp1 as a1

			%do r=2 %to &nb_pv.;

			FULL JOIN temp&r. as a&r. on a1.&dvar.=a&r..&dvar. 

			%IF &byvartot. ne %THEN %DO;
		
				%DO i=1 %TO &nb_byvartot.;

				%LET var=%SCAN(&byvartot.,&i.);

				and a1.&var.=a&r..&var.

				%end;
			%END;
		
			%end;


	
			


			%END;


			%IF %UPCASE(&method)=REG or %UPCASE(&method)=LOGISTIC %THEN %DO;

				SELECT a1.*,

				%do j=2 %to &nb_pv.;

					a&j..Estimate_&j.,
					a&j..StdErr_&j.,
				%end;	

				(Estimate_1

				%do k=2 %to &nb_pv.;

					+Estimate_&k.

				%end;

			

				 )/&nb_pv. as Estimate_m,

				(StdErr_1**2

				%do k=2 %to &nb_pv.;

					+StdErr_&k.**2

				%end;

				 )/&nb_pv. as StdErrsquared_m



		FROM temp1 as a1

			%END;

		%IF %UPCASE(&method)^=FREQ %THEN %DO;

				%do l=2 %to &nb_pv.;

					,temp&l. as a&l.

				%end;

		%END;

		%IF %UPCASE(&method)=REG %THEN %DO;

		WHERE 
			a1.parameter
				%do k=2 %to &nb_pv.;

					=a&k..parameter

					%end;
		%END;

		%IF %UPCASE(&method)=LOGISTIC %THEN %DO;

		WHERE

			%if %SCAN(&CLVAR,1) ne or %scan(&clvarpv,1) ne   %then %do;
				a1.ClassVal0
					%do k=2 %to &nb_pv.;

						=a&k..ClassVal0

					%end;
				and
			%end;

			a1.variable
				%do k=2 %to &nb_pv.;

					=a&k..variable

				%end;
		%END;

	%IF &byvartot. ne and %UPCASE(&method)^=MEAN and %UPCASE(&method)^=FREQ %THEN %DO;
		

		%LET var=%SCAN(&byvartot.,1);

		and a1.&var.
			%do k=2 %to &nb_pv.;

				=a&k..&var.

				%end;


		%DO i=2 %TO &nb_byvartot.;

			%LET var=%SCAN(&byvartot.,&i.);

			and a1.&var.
				%do k=2 %to &nb_pv.;

					=a&k..&var.

					%end;
				

		%END;

		

	%END;

	%IF &byvartot. ne and %UPCASE(&method)=MEAN %THEN %DO;
		WHERE

		%LET var=%SCAN(&byvartot.,1);

		a1.&var.
			%do k=2 %to &nb_pv.;

				=a&k..&var.

				%end;


		%DO i=2 %TO &nb_byvartot.;

			%LET var=%SCAN(&byvartot.,&i.);

			and a1.&var.
				%do k=2 %to &nb_pv.;

					=a&k..&var.

					%end;
				

		%END;

		

	%END;
		;

		QUIT;


		DATA RESUL;
			SET RESUL;

	
	
		%IF %UPCASE(&method)=MEAN %THEN %DO;
			ERROR_PV=((mean_1-mean_m)**2
				%do k=2 %to &nb_pv.;

						+(mean_&k.-mean_m)**2

				%end;
			
			)*&coef_pv.;




			%IF &percentile ne  %THEN %DO;

				%DO p=1 %TO &nb_perc;
					%let perc=%SCAN(&percentile,&p);
					 

					ERROR_PV_&perc.=((Pctl_&perc._1-Pctl_&perc._m)**2
						%do k=2 %to &nb_pv.;

						+(Pctl_&perc._&k.-Pctl_&perc._m)**2

						%end;
			
						)*&coef_pv.;

				

				%end;	
		%end;				
					


		%END;

		%IF %UPCASE(&method)=FREQ %THEN %DO;
			ERROR_PV=((Percent_1-Percent_m)**2
				%do k=2 %to &nb_pv.;

						+(Percent_&k.-Percent_m)**2

				%end;
			
			)*&coef_pv.;

		%END;

		%IF %UPCASE(&method)=REG or %UPCASE(&method)=LOGISTIC %THEN %DO;
			ERROR_PV=((Estimate_1-Estimate_m)**2
				%do k=2 %to &nb_pv.;

						+(Estimate_&k.-Estimate_m)**2

				%end;
			
			)*&coef_pv.;

		%END;


	/* JK coeff depending on the country*/
	/************************************/



		IF &vemethod.="JK1" THEN DO; /*Australia, Austria, Canada, Denmark, Germany*/

			SE=sqrt(((venreps-1)/venreps)*StdErrsquared_m+ERROR_PV);
		

			%IF &percentile ne  %THEN %DO;

				%DO p=1 %TO &nb_perc;
					%let perc=%SCAN(&percentile,&p);
					 

				SE_&perc.=sqrt(((venreps-1)/venreps)*StdErr_Pctl_&perc._m+ERROR_PV_&perc.);

				%end;	
			%end;	
	
		END;

		ELSE DO;

			SE=sqrt(StdErrsquared_m+ERROR_PV);
		

			%IF &percentile ne  %THEN %DO;

				%DO p=1 %TO &nb_perc;
					%let perc=%SCAN(&percentile,&p);
					 

				SE_&perc.=sqrt(StdErr_Pctl_&perc._m+ERROR_PV_&perc.);

				%end;	
			%end;	
	
		END;

	%IF %UPCASE(&method)=LOGISTIC %THEN %DO;
		fm = error_pv / SE**2;
		df = 1/(fm*fm/(&nb_pv-1) + (1-fm)*(1-fm)/(&nrep-1));
		ddf = (&nrep-1);
		df = min(df,ddf);
		if se ne 0 then prob_T = 2*(1-probt(abs(estimate_m/se),(&nrep-1)));
		else prob_T=.;
		odds=exp(estimate_m);
		LOWER=exp(estimate_m-se*tinv(.975,df));
		UPPER=exp(estimate_m+se*tinv(.975,df));

	%END;


	/**added 09/07/2014 : error in the probt*/

	%IF %UPCASE(&method)=REG %THEN %DO;
		if se ne 0 then prob_T = 2*(1-probt(abs(estimate_m/se),(&nrep-1)));
		else prob_T=.;
		
	%END;


		RUN;


	%END;


/******************************************************************************************************************/
/******************************************************************************************************************/
/**                         2.2- COMPUTATION : CASE WITH NO PLAUSIBLE VARIABLE                                   **/
/******************************************************************************************************************/
/******************************************************************************************************************/

%ELSE %DO;

		%IF %UPCASE(&method)=MEAN %THEN %DO;
				PROC SURVEYMEANS data=TEMP_&table. varmethod=jackknife mean nobs SUMWGT 
												%IF &percentile ne  %THEN %DO; 
												percentile=(&percentile.) 
												%end;;
				VAR &dvar.;
		%END;

		%ELSE %IF %UPCASE(&method)=FREQ %THEN %DO;
				PROC SURVEYFREQ data=TEMP_&table. varmethod=jackknife;
				TABLE &dvar.;
		%END;

		%ELSE %IF %UPCASE(&method)=REG %THEN %DO;
				PROC SURVEYREG DATA=TEMP_&table. varmethod=jackknife;
				MODEL &dvar. = &Ivar.;
		%END;

		%ELSE %IF %UPCASE(&method)=LOGISTIC %THEN %DO;
				PROC SURVEYLOGISTIC DATA=TEMP_&table. varmethod=jackknife ;
				

				%if %SCAN(&CLVAR,1) ne  %then %do;
					CLASS &CLVAR  / param=ref;
				%end;
				MODEL &dvar.(EVENT="&event.") = &Ivar. 
				%IF (&PROBIT=1) %THEN %DO;
						/link=probit 
					%END;;
		%END;

		weight &wgt.;

		%if &byvar ne  %then %do;
				by &byvar. &vemethod. VENREPS;
		%end;

		repweight &RWGT.1--&RWGT.&NREP. /  jkcoefs=1;

		%IF %UPCASE(&method)=MEAN %THEN %DO;
				ods output statistics=Resul(rename= (mean=mean_m StdErr=SE) drop= VarName );
		%END;
		%IF %UPCASE(&method)=FREQ %THEN %DO;
				ods output Oneway=Resul(rename= (Percent=Percent_m StdErr=SE) drop= table StdDev _SkipLine );
		%END;

		%IF %UPCASE(&method)=REG or %UPCASE(&method)=LOGISTIC %THEN %DO;
				ods output ParameterEstimates= resul(rename= (Estimate=Estimate_m StdErr=SE) ) 
						%IF &R2=1 %THEN %DO; /*compute R2 if option asked*/

						FitStatistics=R2

						%END;;
		%END;

		%IF %UPCASE(&PREDICT)=1 %THEN %DO;
				output out=&table_pred. predicted=predicted residual=residual std=se_pred;
			
		%END;

		run;


			%IF &percentile ne  %THEN %DO;

				DATA resul; SET resul;
					RENAME
					%DO p=1 %TO &nb_perc;
						%let perc=%SCAN(&percentile,&p);

						 Pctl_&perc.=Pctl_&perc._m
						

					%END; 
					;

					%DO p=1 %TO &nb_perc;
						%let perc=%SCAN(&percentile,&p);

						 SE_&perc.=StdErr_Pctl_&perc.;

					%END;

					DROP  LCL: UCL: StdErr_Pctl:;
				RUN;
			%END;



			%IF %UPCASE(&method)=LOGISTIC %THEN %DO;

			data resul;set resul;
				df = (&nrep-1);
				if se ne 0 then prob_T = 2*(1-probt(abs(estimate_m/se),(&nrep-1)));
				else prob_T=.;
				odds=exp(estimate_m);
				LOWER=exp(estimate_m-se*tinv(.975,df));
				UPPER=exp(estimate_m+se*tinv(.975,df));
			run;
			%END;

/**added 09/07/2014 : error in the probt*/

				%IF %UPCASE(&method)=REG %THEN %DO;
				data resul;set resul;
					if se ne 0 then prob_T = 2*(1-probt(abs(estimate_m/se),(&nrep-1)));
					else prob_T=.;
				run;
					
				%END;
		
	/* JK coeff depending on the country*/
	/************************************/

	


		DATA RESUL;
		SET RESUL;
		
		

		IF &vemethod.="JK1" THEN DO; /*Australia, Austria, Canada, Denmark, Germany*/
			SE=sqrt(((venreps-1)/venreps)*SE*SE);
		END;
		RUN;

		
	%END;

	



/******************************************************************************************************************/
/******************************************************************************************************************/
/**                   					 3- FORMATTING AND EXPORT IN EXCEL                                       **/
/******************************************************************************************************************/
/******************************************************************************************************************/

%IF %UPCASE(&method)=MEAN %THEN %DO;
	
		data resul; set resul;
		keep &byvartot. mean_m se: n SumWgt %IF &percentile ne  %THEN %DO; 
												%DO p=1 %TO &nb_perc;
												%let perc=%SCAN(&percentile,&p);
					 							pctl_&perc._m

												%end;
												%end;;
		run;
	
	
		
%END;


%IF %UPCASE(&method)=FREQ %THEN %DO;
	
		data resul; set resul;
		if &dvar.=. then delete;
		keep &dvar. &byvartot. percent_m se frequency wgtfreq;
		run;

		
	
%END;

%IF %UPCASE(&method)=REG %THEN %DO;
	
		data resul; set resul;
		keep &byvartot. Parameter ESTIMATE_m se Prob_t;
		run;
	
		%IF &R2=1 %THEN %DO;




			%IF (&DVARPV=1) or (&IVARPV ne ) or (&BYVARPV ne ) %THEN %DO;
			
				%DO i=1 %TO &nb_pv. ;
					DATA R2_&i.; set R2_&i.;
					
					%if (&byvarpv ne )%then %do ;
					&byvarpv.=&byvarpv.&i. ;
					%end;
					R2_&i.=input(cValue1,8.);
					IF Label1="R-Square";
					keep &byvar. &byvarpv. R2_&i.   ; 

					run;

				PROC SORT DATA=R2_&i.;
				by &byvartot;
				RUN;
				%END;

				
				DATA R2;
				MERGE R2_1 %do s=2 %to &nb_pv. ; 
					R2_&s. %end;;
				by &byvartot;
				RUN;
				
 			%END;

			%ELSE %DO;

			DATA R2; set R2;
			R2=input(cValue1,8.);
			IF Label1="R-Square";
				keep &byvartot. R2;
			run;

			%END;

		%IF %BQUOTE(&ficout.) ne  %THEN %DO;

			Proc EXPORT data=R2
			outfile="&path_out.&ficout." 
			dbms=&dbms replace;sheet=&out_R2.;run;

		%END;
		%END;
	
	
%END;




%IF %UPCASE(&method)=LOGISTIC %THEN %DO;
	
		data resul; set resul;
		%if %UPCASE(&logintercept) ne YES %then %do; 
			if variable="Intercept" then delete;
			%end;
		keep &byvartot. variable 
		%if %SCAN(&CLVAR,1) ne  or %scan(&clvarpv,1) ne %then %do;
		ClassVal0 %end;
		df prob_T odds lower upper ESTIMATE_m se;
		run;
	
		proc sort data=resul;
		by &byvartot. variable %if %SCAN(&CLVAR,1) ne   or %scan(&clvarpv,1) ne %then %do; ClassVal0 %end;;
		run;
	
		
%END;

%IF %BQUOTE(&ficout.) ne  %THEN %DO;
Proc EXPORT data=resul
		outfile="&path_out.&ficout." 
		dbms=&dbms replace;sheet=&excel_sheet. ;run; 
%END;

%ELSE %DO ;
proc print data=resul;
run;
%END;

proc delete data=temp_&table;run;	

%mend;

