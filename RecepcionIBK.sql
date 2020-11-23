--update datosbanco set flag6 = 1 where cuentabanco = '041-3000106330';
--update datosbanco set flag6 = 1 where cuentabanco = '041-3000106347';

DECLARE
	linebuf				VARCHAR2 (1000);
	cRecauda			recaudacionbanco%ROWTYPE;
	vFechapago			VARCHAR2(8);
	vValidaTrama		NUMBER;
	vFechaProceso		DATE:= SYSDATE;
	vNumerocuota		prestamocuotas.numerocuota%TYPE;

	vRubro				VARCHAR(2);
	vCodEmpresa			VARCHAR(3);
	vCodServicio		VARCHAR(2);

	vPeriodoSolicitud		VARCHAR(4);
	vNumeroSolicitud		VARCHAR(7);
BEGIN
	--@@ Codigo del Servicio SVC
	vRubro := '03';
	vCodEmpresa := '953';
	vCodServicio := '01'; --Prestamo

	--linebuf := '0395301012229348             200101  05000218122    YAXA PERU S.A.                20010305124358000000000024400000000000000200101312001022000008839           ';
	linebuf := '0395301011800147             1820002 002-ACT-2020   ALVA MORENO POUL JUNIOR0021120202011211044460000000048137000000000000002020112020201121000034860000000000   ';
	
	IF linebuf IS NOT NULL AND SUBSTR(linebuf, 1, 7) = vRubro || vCodEmpresa || vCodServicio THEN
		SELECT COUNT(*)
		INTO vValidaTrama
		FROM RECAUDACIONBANCO
		WHERE REPLACE(TRIM(TRAMA), ' ', '') = REPLACE(TRIM(linebuf), ' ', '');

		IF vValidaTrama = 0 THEN

			--Moneda
			IF SUBSTR(linebuf, 8, 2) = '01' THEN
				cRecauda.moneda := 1;
			ELSE
				cRecauda.moneda := 2;
			END IF;

			cRecauda.trama					:= linebuf;
			cRecauda.fechacarga				:= vFechaProceso;
			cRecauda.usuariocarga			:= USER;
			cRecauda.codigobanco			:= 4;		-- Codigo Banco en Datosbanco

			BEGIN
				cRecauda.periodosolicitud	:= SUBSTR(linebuf, 46, 4);
				cRecauda.numerosolicitud	:= SUBSTR(linebuf, 30, 7);

				cRecauda.numerocuota		:= SUBSTR(linebuf, 38, 3);

				cRecauda.nombrecliente		:= TRIM(SUBSTR(linebuf, 53, 30));

				cRecauda.tipopago			:= SUBSTR(linebuf, 42, 3);
	            
				cRecauda.referencias      	:= SUBSTR(linebuf, 83, 14);

				cRecauda.numerocuentabanco 	:= pkg_datosbanco.f_obt_cuentabancorecauda(cRecauda.codigobanco, cRecauda.moneda);
	            
				SELECT b.CIP INTO cRecauda.codigosocio
				FROM PRESTAMO a,PERSONA b
				WHERE a.PERIODOSOLICITUD=cRecauda.periodosolicitud
					AND a.NUMEROSOLICITUD=cRecauda.numerosolicitud
					AND b.CODIGOPERSONA=a.CODIGOPERSONA
					AND ROWNUM = 1;

				cRecauda.importeorigen 		:= TO_NUMBER(LTRIM(SUBSTR(linebuf, 97, 13), '0')) / 100;
				cRecauda.importedepositado 	:= cRecauda.importeorigen;

				cRecauda.importemora 		:= TO_NUMBER(SUBSTR(linebuf, 110, 7));
				
	            cRecauda.oficinapago 		:= 0;

				cRecauda.nromovimiento 		:= SUBSTR(linebuf, 140, 8);

				cRecauda.fechaenvio 		:= TO_DATE 	(
														SUBSTR(linebuf, 130, 2)||'/'||
														SUBSTR(linebuf, 128, 2)||'/'||
														SUBSTR(linebuf, 124, 4),
														'DD/MM/RR'
														);

				cRecauda.fechavencimiento 	:= TO_DATE 	(
														SUBSTR(linebuf, 138, 2)||'/'||
														SUBSTR(linebuf, 136, 2)||'/'||
														SUBSTR(linebuf, 132, 4),
														'DD/MM/RR'
														);

				cRecauda.fechapago 			:= TO_DATE 	(
														SUBSTR(linebuf, 89, 2)||'/'||
														SUBSTR(linebuf, 87, 2)||'/'||
														SUBSTR(linebuf, 83, 4),
														'DD/MM/RR'
														);

				--cRecauda.fechaproceso 		:= TO_DATE(SUBSTR(linebuf, 105, 2) || '/' || SUBSTR(linebuf, 103, 2) || '/' || SUBSTR(linebuf, 99, 4), 'DD/MM/RRRR');
				--cRecauda.usuarioproceso 	:= SUBSTR(linebuf, 91, 8);
				cRecauda.fechaproceso 		:= SYSDATE;
				cRecauda.usuarioproceso 	:= USER;
				BEGIN
					SELECT MIN(numerocuota)
					INTO vNumerocuota
					FROM prestamocuotas 
					WHERE periodosolicitud = cRecauda.periodosolicitud 
					AND numerosolicitud = cRecauda.numerosolicitud 
					AND estado = 2;
				EXCEPTION WHEN OTHERS THEN
					vNumerocuota := NULL;
				END; 

				BEGIN
					PKG_RECAUDACIONBANCO.P_OBT_VERIFICARDEBITOAUTO(cRecauda.periodosolicitud, cRecauda.numerosolicitud, cRecauda.debitoautomatico);
					cRecauda.estado := '1';
				EXCEPTION WHEN OTHERS THEN
					RAISE_APPLICATION_ERROR(-20120,'  cRecauda.estado  ' || cRecauda.estado  );
				END;

				cRecauda.cuotacronograma 	:= vNumerocuota;

				cRecauda.amortizacion   	:= pkg_prestamocuotas.F_OBT_AMORTIZACION ( 	cRecauda.numerosolicitud, 
																						cRecauda.periodosolicitud, 
																						vNumerocuota);

				cRecauda.interes        	:= pkg_prestamocuotas.F_OBT_INTERES ( 		cRecauda.numerosolicitud,
																						cRecauda.periodosolicitud, 
																						vNumerocuota );

				cRecauda.mora           	:= 0;

				cRecauda.reajuste       	:= pkg_prestamocuotas.F_OBT_REAJUSTE (		cRecauda.numerosolicitud, 
																						cRecauda.periodosolicitud, 
																						vNumerocuota);

				cRecauda.portes         	:= pkg_prestamocuotas.F_OBT_PORTES (		cRecauda.numerosolicitud, 
																						cRecauda.periodosolicitud, 
																						vNumerocuota);

				cRecauda.segurointeres  	:= pkg_prestamocuotas.F_OBT_SEGUROINTERES( 	cRecauda.numerosolicitud, 
																						cRecauda.periodosolicitud, 
																						vNumerocuota); 
				cRecauda.totalcuota 		:= 	NVL(cRecauda.amortizacion, 0) +
												NVL(cRecauda.interes, 0) +
												NVL(cRecauda.mora, 0) +
												NVL(cRecauda.reajuste, 0) +
												NVL(cRecauda.portes, 0) +
												NVL(cRecauda.segurointeres, 0);
				--
				IF cRecauda.numerocuota <> cRecauda.cuotacronograma THEN 
					cRecauda.observaciones 	:= cRecauda.observaciones || ' CUOTAS DIFERENTES ' || CHR(9);
				END IF;

				IF cRecauda.importeorigen <> cRecauda.totalcuota THEN
					cRecauda.observaciones 	:= cRecauda.observaciones || ' IMPORTES DIFERENTES ' || CHR(9);
				END IF;

				BEGIN
					INSERT INTO recaudacionbanco( fechacarga,
					usuariocarga,
					codigosocio,
					nombrecliente,
					referencias,
					importeorigen,
					importedepositado,
					importemora,
					oficinapago,
					nromovimiento,
					fechapago,
					tipopago,
					estado,
					codigobanco,
					numerocuentabanco,
					periodosolicitud,
					numerosolicitud,
					moneda,
					numerocuota,
					fechavencimiento,
					amortizacion,
					interes,
					mora,
					reajuste,
					portes,
					segurointeres,
					fechaproceso,
					usuarioproceso,
					trama,
					fechaenvio,
					debitoautomatico,
					cuotacronograma,
					totalcuota,
					observaciones
					)
					VALUES ( cRecauda.fechacarga,
					cRecauda.usuariocarga,
					cRecauda.codigosocio,
					cRecauda.nombrecliente,
					cRecauda.referencias,
					cRecauda.importeorigen,
					cRecauda.importedepositado,
					cRecauda.importemora,
					cRecauda.oficinapago,
					cRecauda.nromovimiento,
					cRecauda.fechapago,
					cRecauda.tipopago,
					cRecauda.estado,
					cRecauda.codigobanco,
					cRecauda.numerocuentabanco,
					cRecauda.periodosolicitud,
					cRecauda.numerosolicitud,
					cRecauda.moneda,
					cRecauda.numerocuota,
					cRecauda.fechavencimiento,
					cRecauda.amortizacion,
					cRecauda.interes,
					cRecauda.mora,
					cRecauda.reajuste,
					cRecauda.portes,
					cRecauda.segurointeres,
					cRecauda.fechaproceso,
					cRecauda.usuarioproceso,
					cRecauda.trama,
					cRecauda.fechaenvio,
					cRecauda.debitoautomatico,
					cRecauda.cuotacronograma,
					cRecauda.totalcuota,
					cRecauda.observaciones
					) ;
					COMMIT;
				END;
			END;
		END IF;
	END IF;
END;

select * from recaudacionbanco order by fechacarga desc;