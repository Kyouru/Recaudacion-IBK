--9:00
DECLARE
	PIFECHA DATE := '20/11/2020';
	PIMONEDA NUMBER := 1;

	CURSOR detalle IS
    SELECT 1 AS COD1,
           2 AS COD2,
           '02' AS TIP_REGISTRO,
           RPAD(TRIM(SUBSTR(pkg_persona.f_obt_nombrecompletobancos(pre.Codigopersona ),1,23)),23,' ')
           ||(SELECT LPAD(TO_CHAR(MAX(numerocuota)),3,0)
                FROM TABLE( cre08070.deudacuotassip(pre.periodosolicitud, pre.numerosolicitud, hoy))
               WHERE fechavencimiento <= HOY)
           ||TO_CHAR(HOY,'MMDD') AS NOM_CLIENTE,
           (SELECT LPAD(TO_CHAR(MAX(numerocuota)),3,0)
                FROM TABLE( CRE08070.DEUDACUOTASSIP(PRE.PeriodoSolicitud, PRE.NumeroSolicitud, HOY))
               WHERE FECHAVENCIMIENTO <= HOY)||TO_CHAR(HOY,'MMDD')AS IDENTIFICADORCUOTA,
           PKG_PERSONA.F_OBT_CIP(SP.CODIGOPERSONA) AS CODIGOSOCIO,
           DECODE(substr(pkg_syst902.f_obt_tbldescri(pd.tiposolicitud,pd.tipoprestamo) ,1,3),
                  'PTP',
                  PKG_PERSONA.F_OBT_CIP(SP.CODIGOPERSONA)||'-'||
                  SUBSTR(GEN05010(SP.TIPOSOLICITUD,SP.TIPOPRESTAMO),1,3)||'-'|| LPAD(SP.PERIODOSOLICITUD,4,0)||'-'||
                 SUBSTR(LPAD(SP.NUMEROSOLICITUD,7,0),1,2)||'-'||SUBSTR(LPAD(SP.NUMEROSOLICITUD,7,0),3)||'ATR',
           --per.cip ||''||
           TO_CHAR(HOY,'YYMMDD') ||  
           SUBSTR(pkg_syst902.f_obt_tbldescri(sp.tiposolicitud,sp.tipoprestamo) ,1,3) ||''|| 
            SUBSTR(pkg_syst900.f_obt_tbldescri(22, pre.moneda),1,1)  ||''||
            LPAD(pre.PeriodoSolicitud,4,0)||''||
            LPAD(pre.NumeroSolicitud,7,0)||
            --pre.NumeroSolicitud||
           'ATR')PAGO_ID,
           DECODE(SUBSTR(pkg_syst902.f_obt_tbldescri(pd.tiposolicitud,pd.tipoprestamo) ,1,3),'PTP',
           (SELECT LPAD(TO_CHAR(MAX(numerocuota)),3,0)
                FROM TABLE( CRE08070.DEUDACUOTASSIP(PRE.PeriodoSolicitud, PRE.NumeroSolicitud, HOY))
               WHERE FECHAVENCIMIENTO <= HOY)||TO_CHAR(HOY,'MMDD')           
           ||'-'||
           SUBSTR(GEN05010(SP.TIPOSOLICITUD,SP.TIPOPRESTAMO),1,3)||'-'|| LPAD(SP.PERIODOSOLICITUD,4,0) ||'-'||
           SUBSTR(LPAD(SP.NUMEROSOLICITUD,7,0),1,2)||'-'||SUBSTR(LPAD(SP.NUMEROSOLICITUD,7,0),3)||'ATR',
           --per.cip||''||
           TO_CHAR(HOY,'YYMMDD') || 
           SUBSTR(pkg_syst902.f_obt_tbldescri(sp.tiposolicitud,sp.tipoprestamo) ,1,3) ||''||  
           SUBSTR(pkg_syst900.f_obt_tbldescri(22, pre.moneda),1,1)  ||''|| 
           LPAD(pre.PeriodoSolicitud,4,0)||''||
           LPAD(pre.NumeroSolicitud,7,0)||
           'ATR'  
           /*|| NVL((SELECT LPAD(TO_CHAR(MAX(numerocuota)),3,0)
                FROM TABLE( CRE08070.DEUDACUOTASSIP(PRE.PeriodoSolicitud, PRE.NumeroSolicitud, HOY))
               WHERE FECHAVENCIMIENTO <= HOY),'001')*/
           ) PAGO_ID_2,
           DECODE(substr(pkg_syst902.f_obt_tbldescri(pd.tiposolicitud,pd.tipoprestamo) ,1,3),
                  'PTP',
                  PKG_PERSONA.F_OBT_CIP(SP.CODIGOPERSONA)||'-'||
                  SUBSTR(GEN05010(SP.TIPOSOLICITUD,SP.TIPOPRESTAMO),1,3)||'-'|| LPAD(SP.PERIODOSOLICITUD,4,0)||'-'||
                 SUBSTR(LPAD(SP.NUMEROSOLICITUD,7,0),1,2)||'-'||SUBSTR(LPAD(SP.NUMEROSOLICITUD,7,0),3)||'ATR',
           LPAD(per.cip,7,0) ||''||             
           SUBSTR(pkg_syst902.f_obt_tbldescri(sp.tiposolicitud,sp.tipoprestamo) ,1,3) ||''|| 
            SUBSTR(pkg_syst900.f_obt_tbldescri(22, pre.moneda),1,1)  ||''||
            LPAD(pre.PeriodoSolicitud,4,0)||''||
            LPAD(pre.NumeroSolicitud,7,0)||           
           'ATR' ||
           TO_CHAR(HOY,'YYMMDD'))PAGO_ID_CONTINENTAL,
           PKG_RECAUDACIONENVIO.F_GET_MONTOADEUDADO (pre.PeriodoSolicitud , pre.NumeroSolicitud, 1 ) AS Monto_Minimo,
           PKG_RECAUDACIONENVIO.F_GET_MONTOADEUDADO (pre.PeriodoSolicitud , pre.NumeroSolicitud, 2 ) AS SALDOCAPITAL,
           CASE WHEN SUBSTR(pkg_syst902.f_obt_tbldescri(pd.tiposolicitud,pd.tipoprestamo) ,1,3)='PTP'
                 AND PKG_RECAUDACIONENVIO.F_GET_MONTOADEUDADO (pre.PeriodoSolicitud , pre.NumeroSolicitud, 2 )=0  THEN
                 PKG_RECAUDACIONENVIO.F_OBT_INTERESATRASADO (pre.PeriodoSolicitud,pre.NumeroSolicitud,hoy)
           ELSE
               PKG_RECAUDACIONENVIO.F_GET_MONTOADEUDADO (pre.PeriodoSolicitud , pre.NumeroSolicitud, 3 )
           END AS SALDOINTERES,
           PKG_RECAUDACIONENVIO.F_GET_MONTOADEUDADO (pre.PeriodoSolicitud , pre.NumeroSolicitud, 4 ) AS SALDOMORA,
           PKG_RECAUDACIONENVIO.F_GET_MONTOADEUDADO (pre.PeriodoSolicitud , pre.NumeroSolicitud, 5 ) AS SEGUROINTERES,
           PKG_RECAUDACIONENVIO.F_GET_MONTOADEUDADO (pre.PeriodoSolicitud , pre.NumeroSolicitud, 6 ) AS APORTES,
           PKG_RECAUDACIONENVIO.F_GET_MONTOADEUDADO (pre.PeriodoSolicitud , pre.NumeroSolicitud, 7 ) AS REAJUSTE,
           NVL(PKG_RECAUDACIONENVIO.F_OBT_FECVENCADEUDADO (pre.PeriodoSolicitud , pre.NumeroSolicitud ), trunc(HOY) ) AS FECHAVENCIMIENTO,
           pkg_cartera.DIAS_ATRASO_CARTERA(PIFECHA ,pre.PeriodoSolicitud , pre.NumeroSolicitud ) DIASATRASO,
           pre.codigopersona,
           (SELECT LPAD(TO_CHAR(MAX(numerocuota)),3,0)
              FROM TABLE( CRE08070.DEUDACUOTASSIP(PRE.PeriodoSolicitud, PRE.NumeroSolicitud, HOY))
             WHERE FECHAVENCIMIENTO <= HOY) AS NUMEROCUOTA,
           SUBSTR(pkg_syst902.f_obt_tbldescri(pd.tiposolicitud,pd.tipoprestamo) ,1,3)TipoProducto,
           pre.PeriodoSolicitud , LPAD(pre.NumeroSolicitud,7,'0')NumeroSolicitud
      FROM prestamo pre
    INNER JOIN (SELECT periodosolicitud, numerosolicitud, MAX(numeroampliacion) nroampl
                  FROM prestamodetalle GROUP BY periodosolicitud, numerosolicitud) presdet
    ON presdet.periodosolicitud = pre.periodosolicitud AND presdet.numerosolicitud = pre.numerosolicitud
    INNER JOIN prestamodetalle pd
            ON pd.periodosolicitud = presdet.periodosolicitud
           AND pd.numerosolicitud = presdet.numerosolicitud AND pd.numeroampliacion = presdet.nroampl
    INNER JOIN solicitudprestamo sp ON PRE.PERIODOSOLICITUD= sp.PERIODOSOLICITUD AND PRE.NUMEROSOLICITUD = sp.NUMEROSOLICITUD
    INNER JOIN persona per ON per.codigopersona = pre.codigopersona
    INNER JOIN ( SELECT p.codigopersona, TO_CHAR(p.numeroruc) AS nrodoc FROM persona p INNER JOIN datossocio ds ON p.codigopersona =ds.codigopersona WHERE p.tipopersona = 2
                        UNION ALL SELECT pn.codigopersona, pn.numerodocumentoid AS nrodoc FROM personanatural pn
                  INNER JOIN datossocio ds ON pn.codigopersona =ds.codigopersona) soc ON  soc.codigopersona = pre.codigopersona

     WHERE pre.moneda = PIMONEDA
       AND LENGTH(TRIM(soc.nrodoc))>=8
       AND pre.SALDOPRESTAMO > 0 -- prestamos con monto adeudado pendiente
       AND substr(pkg_syst902.f_obt_tbldescri(pd.tiposolicitud,pd.tipoprestamo) ,1,3) NOT  IN ('PCC','PCY','PCM','PFI',-- CARTERA
                                                                                            'PDP', 'PDD','PLR','PLC', 'TAN'-- Descuento por Planilla
                                                                                              )
       AND PRE.PERIODOSOLICITUD<>1        
       AND pre.periodosolicitudconcesional IS NULL
       AND pre.numerosolicitudconcesional IS NULL
       AND (pre.periodosolicitud, pre.numerosolicitud) NOT IN (SELECT periodosolicitud, numerosolicitud
                                                                  FROM solicitudprestamo
                                                                  WHERE periodosolicitudconcesional IS NOT NULL
                                                                     AND numerosolicitudconcesional IS NOT NULL)
       AND NVL(DECODE(SUBSTR(pkg_syst902.f_obt_tbldescri(pd.tiposolicitud,pd.tipoprestamo) ,1,3),'PTP',
              ( SELECT MAX(numerocuota)
                  FROM TABLE( CRE08070.DEUDACUOTASSIP(pre.periodosolicitud, pre.numerosolicitud, HOY))
                 WHERE fechavencimiento <= HOY ),pkg_cartera.DIAS_ATRASO_CARTERA(PIFECHA ,pre.PeriodoSolicitud , pre.NumeroSolicitud )),0)>0
       AND pre.periodosolicitudconcesional IS NULL AND pre.numerosolicitudconcesional IS NULL
       AND (pre.periodosolicitud, pre.numerosolicitud) NOT IN(SELECT periodosolicitud, numerosolicitud
                                                                 FROM solicitudprestamo
                                                                 WHERE periodosolicitudconcesional IS NOT NULL
                                                                 AND numerosolicitudconcesional IS NOT NULL )
     UNION ALL
    SELECT 1 AS cod1,
           2 AS cod2,
           '02' TIP_REGISTRO,
           RPAD(TRIM(SUBSTR( PKG_PERSONA.F_OBT_NOMBRECOMPLETOBANCOS(pre.codigopersona ),1,23)),23,' ')
           ||LPAD(TRIM(TO_CHAR(PC.NUMEROCUOTA,'999')),3,'0')
           ||TO_CHAR(HOY,'MMDD') AS NOM_CLIENTE,
           LPAD(TRIM(TO_CHAR(PC.NUMEROCUOTA,'999')),3,'0')||
           TO_CHAR(HOY,'MMDD') AS IDENTIFICADORCUOTA,
           PKG_PERSONA.F_OBT_CIP(SP.CODIGOPERSONA) AS CODIGOSOCIO,
           DECODE(SUBSTR(pkg_syst902.f_obt_tbldescri(pd.tiposolicitud,pd.tipoprestamo) ,1,3),
                  'PTP',PKG_PERSONA.F_OBT_CIP(SP.CODIGOPERSONA)||'-'||
                        SUBSTR(GEN05010(SP.TIPOSOLICITUD,SP.TIPOPRESTAMO),1,3)||'-'||
                        LPAD(SP.PERIODOSOLICITUD,4,0)||'-'||
                        SUBSTR(LPAD(SP.NUMEROSOLICITUD,7,0),1,2)||'-'||
                        SUBSTR(LPAD(SP.NUMEROSOLICITUD,7,0),3)||
                        'ACT',
           --per.cip ||''||
           TO_CHAR(HOY,'YYMMDD') || 
           SUBSTR(pkg_syst902.f_obt_tbldescri(sp.tiposolicitud,sp.tipoprestamo) ,1,3) ||''||  
           SUBSTR(pkg_syst900.f_obt_tbldescri(22, pre.moneda),1,1)  ||''||
           LPAD(pre.PeriodoSolicitud,4,0)||''||
           LPAD(pre.NumeroSolicitud,7,0)||
           --pre.NumeroSolicitud||-- PAGO_ID,
           'ACT') PAGO_ID ,
           DECODE(SUBSTR(pkg_syst902.f_obt_tbldescri(pd.tiposolicitud,pd.tipoprestamo) ,1,3),
                  'PTP',( SELECT LPAD(TO_CHAR(MAX(numerocuota)),3,0)
                            FROM TABLE( CRE08070.DEUDACUOTASSIP(PRE.PeriodoSolicitud, 
                                                                PRE.NumeroSolicitud, HOY)
                                      )
                           WHERE FECHAVENCIMIENTO <= HOY )||
                        TO_CHAR(HOY,'MMDD')||'-'||
                        SUBSTR(GEN05010(SP.TIPOSOLICITUD,SP.TIPOPRESTAMO),1,3)||'-'||
                        LPAD(SP.PERIODOSOLICITUD,4,0)||'-'||
                        SUBSTR(LPAD(SP.NUMEROSOLICITUD,7,0),1,2)||'-'||
                        SUBSTR(LPAD(SP.NUMEROSOLICITUD,7,0),3)||
                        'ACT',
                  --per.cip ||''||
                  TO_CHAR(HOY,'YYMMDD') ||
                  SUBSTR(pkg_syst902.f_obt_tbldescri(sp.tiposolicitud,sp.tipoprestamo) ,1,3) ||''||  
                  SUBSTR(pkg_syst900.f_obt_tbldescri(22, pre.moneda),1,1)  ||''|| 
                  LPAD(pre.PeriodoSolicitud,4,0)||''||
                  LPAD(pre.NumeroSolicitud,7,0)||
                  'ACT'
                  /*|| 
                  NVL(( SELECT LPAD(TO_CHAR(MAX(numerocuota)),3,0)
                      FROM TABLE( CRE08070.DEUDACUOTASSIP( PRE.PeriodoSolicitud, 
                                                           PRE.NumeroSolicitud, 
                                                           HOY 
                                                         )
                  )
                     WHERE FECHAVENCIMIENTO <= HOY),'001')*/           
            )PAGO_ID_2,    
            DECODE(SUBSTR(pkg_syst902.f_obt_tbldescri(pd.tiposolicitud,pd.tipoprestamo) ,1,3),
                  'PTP',PKG_PERSONA.F_OBT_CIP(SP.CODIGOPERSONA)||'-'||
                        SUBSTR(GEN05010(SP.TIPOSOLICITUD,SP.TIPOPRESTAMO),1,3)||'-'||
                        LPAD(SP.PERIODOSOLICITUD,4,0)||'-'||
                        SUBSTR(LPAD(SP.NUMEROSOLICITUD,7,0),1,2)||'-'||
                        SUBSTR(LPAD(SP.NUMEROSOLICITUD,7,0),3)||
                        'ACT',
           LPAD(per.cip,7,0) ||''||                       
           SUBSTR(pkg_syst902.f_obt_tbldescri(sp.tiposolicitud,sp.tipoprestamo) ,1,3) ||''||  
           SUBSTR(pkg_syst900.f_obt_tbldescri(22, pre.moneda),1,1)  ||''||
           LPAD(pre.PeriodoSolicitud,4,0)||''||
           LPAD(pre.NumeroSolicitud,7,0)||            
           'ACT' ||
           TO_CHAR(HOY,'YYMMDD')) PAGO_ID_CONTINENTAL,                 
           pc.AMORTIZACION + pc.INTERES as Monto_MINIMO,
           pc.AMORTIZACION AS SALDOCAPITAL,
           pc.INTERES AS SALDOINTERES,
           0 AS SALDOMORA,
           NVL(pc.segurointeres,0) AS SEGUROINTERES,
           NVL(pc.portes,0) AS APORTES,
           NVL(pc.reajuste,0) AS REAJUSTE,
           NVL(pc.fechavencimiento,TRUNC(HOY)) AS FECHAVENCIMIENTO,
           0 DIASATRASO,
           pre.codigopersona,
           LPAD(TRIM(TO_CHAR(PC.NUMEROCUOTA,'999')),3,'0') AS numerocuota,
           SUBSTR(pkg_syst902.f_obt_tbldescri(pd.tiposolicitud,pd.tipoprestamo) ,1,3)TipoProducto,
           pre.PeriodoSolicitud , LPAD(pre.NumeroSolicitud,7,'0')NumeroSolicitud
      FROM prestamo pre
    INNER JOIN (SELECT periodosolicitud, numerosolicitud, MAX(numeroampliacion) nroampl
                  FROM prestamodetalle GROUP BY periodosolicitud, numerosolicitud) presdet
    ON presdet.periodosolicitud = pre.periodosolicitud AND presdet.numerosolicitud = pre.numerosolicitud
    INNER JOIN prestamodetalle pd ON pd.periodosolicitud = presdet.periodosolicitud AND pd.numerosolicitud = presdet.numerosolicitud AND pd.numeroampliacion = presdet.nroampl
    INNER JOIN solicitudprestamo sp ON PRE.PERIODOSOLICITUD= sp.PERIODOSOLICITUD AND PRE.NUMEROSOLICITUD = sp.NUMEROSOLICITUD
    INNER JOIN persona per ON per.codigopersona = pre.codigopersona
    INNER JOIN prestamocuotas pc ON pre.periodosolicitud = pc.periodosolicitud AND pre.numerosolicitud  = pc.numerosolicitud
    INNER JOIN  ( SELECT p.codigopersona, TO_CHAR(p.numeroruc) AS nrodoc from persona p INNER JOIN datossocio ds ON p.codigopersona =ds.codigopersona WHERE p.tipopersona = 2
                        UNION ALL SELECT pn.codigopersona, pn.numerodocumentoid AS nrodoc FROM personanatural pn INNER JOIN datossocio ds ON pn.codigopersona =ds.codigopersona) soc ON soc.codigopersona = pre.codigopersona

     WHERE pc.estado = 2
       AND LENGTH(TRIM(soc.nrodoc))>=8
       AND pre.moneda =PIMONEDA
       AND pre.SALDOPRESTAMO > 0 -- prestamos con monto adeudado pendiente
       AND SUBSTR(pkg_syst902.f_obt_tbldescri(pd.tiposolicitud,pd.tipoprestamo) ,1,3)   NOT  IN ('PCC','PCY','PCM','PFI',-- CARTERA
                                                                                                    'PDP', 'PDD','PLR','PLC', 'TAN'-- Descuento por Planilla
                                                                                                   )
       AND PRE.PERIODOSOLICITUD <> 1       
       AND pre.periodosolicitudconcesional IS NULL
       AND pre.numerosolicitudconcesional IS NULL
       AND (pre.periodosolicitud, pre.numerosolicitud) NOT IN (SELECT periodosolicitud, numerosolicitud
                                                                  FROM solicitudprestamo
                                                                  WHERE periodosolicitudconcesional IS NOT NULL
                                                                    AND numerosolicitudconcesional IS NOT NULL)
       AND TRUNC(pc.fechavencimiento) IN (SELECT MIN(fechavencimiento ) FROM prestamocuotas
                                             WHERE periodosolicitud = pre.periodosolicitud
                                               AND numerosolicitud = pre.numerosolicitud
                                               AND estado =2
                                               AND fechavencimiento>= HOY
                                            )
     ---Pago de Inscripcion en Cuotas
       UNION ALL                                     
       SELECT 1 AS cod1,
           2 AS cod2,
           '02' TIP_REGISTRO,
           RPAD(TRIM(SUBSTR( PKG_PERSONA.F_OBT_NOMBRECOMPLETOBANCOS(pre.codigopersona ),1,23)),23,' ')
           ||LPAD(TRIM(TO_CHAR(PC.NUMEROCUOTA,'999')),3,'0')
           ||TO_CHAR(HOY,'MMDD') AS NOM_CLIENTE,
           LPAD(TRIM(TO_CHAR(PC.NUMEROCUOTA,'999')),3,'0')||
           TO_CHAR(HOY,'MMDD') AS IDENTIFICADORCUOTA,
           PKG_PERSONA.F_OBT_CIP(SP.CODIGOPERSONA) AS CODIGOSOCIO,
           DECODE(SUBSTR(pkg_syst902.f_obt_tbldescri(pd.tiposolicitud,pd.tipoprestamo) ,1,3),
                  'PTP',PKG_PERSONA.F_OBT_CIP(SP.CODIGOPERSONA)||'-'||
                        SUBSTR(GEN05010(SP.TIPOSOLICITUD,SP.TIPOPRESTAMO),1,3)||'-'||
                        LPAD(SP.PERIODOSOLICITUD,4,0)||'-'||
                        SUBSTR(LPAD(SP.NUMEROSOLICITUD,7,0),1,2)||'-'||
                        SUBSTR(LPAD(SP.NUMEROSOLICITUD,7,0),3)||
                        'ACT',
           --per.cip ||''||
           TO_CHAR(HOY,'YYMMDD') || 
           SUBSTR(pkg_syst902.f_obt_tbldescri(sp.tiposolicitud,sp.tipoprestamo) ,1,3) ||''||  
           SUBSTR(pkg_syst900.f_obt_tbldescri(22, pre.moneda),1,1)  ||''||
           LPAD(pre.PeriodoSolicitud,4,0)||''||
           --pre.NumeroSolicitud||-- PAGO_ID,
           LPAD(pre.NumeroSolicitud,7,0) ||
           'ACT') PAGO_ID ,
           DECODE(SUBSTR(pkg_syst902.f_obt_tbldescri(pd.tiposolicitud,pd.tipoprestamo) ,1,3),
                  'PTP',( SELECT LPAD(TO_CHAR(MAX(numerocuota)),3,0)
                            FROM TABLE( CRE08070.DEUDACUOTASSIP(PRE.PeriodoSolicitud, 
                                                                PRE.NumeroSolicitud, HOY)
                                      )
                           WHERE FECHAVENCIMIENTO <= HOY )||
                        TO_CHAR(HOY,'MMDD')||'-'||
                        SUBSTR(GEN05010(SP.TIPOSOLICITUD,SP.TIPOPRESTAMO),1,3)||'-'||
                        LPAD(SP.PERIODOSOLICITUD,4,0)||'-'||
                        SUBSTR(LPAD(SP.NUMEROSOLICITUD,7,0),1,2)||'-'||
                        SUBSTR(LPAD(SP.NUMEROSOLICITUD,7,0),3)||
                        'ACT',
                  --per.cip ||''||
                  TO_CHAR(HOY,'YYMMDD') || 
                  SUBSTR(pkg_syst902.f_obt_tbldescri(sp.tiposolicitud,sp.tipoprestamo) ,1,3) ||''||  
                  SUBSTR(pkg_syst900.f_obt_tbldescri(22, pre.moneda),1,1)  ||''|| 
                  LPAD(pre.PeriodoSolicitud,4,0)||''||
                  LPAD(pre.NumeroSolicitud,7,0)||
                  'ACT'
                  /*|| 
                  NVL(( SELECT LPAD(TO_CHAR(MAX(numerocuota)),3,0)
                      FROM TABLE( CRE08070.DEUDACUOTASSIP( PRE.PeriodoSolicitud, 
                                                           PRE.NumeroSolicitud, 
                                                           HOY 
                                                         )
                  )
                     WHERE FECHAVENCIMIENTO <= HOY),'001')*/           
            )PAGO_ID_2,    
           DECODE(SUBSTR(pkg_syst902.f_obt_tbldescri(pd.tiposolicitud,pd.tipoprestamo) ,1,3),
                  'PTP',PKG_PERSONA.F_OBT_CIP(SP.CODIGOPERSONA)||'-'||
                        SUBSTR(GEN05010(SP.TIPOSOLICITUD,SP.TIPOPRESTAMO),1,3)||'-'||
                        LPAD(SP.PERIODOSOLICITUD,4,0)||'-'||
                        SUBSTR(LPAD(SP.NUMEROSOLICITUD,7,0),1,2)||'-'||
                        SUBSTR(LPAD(SP.NUMEROSOLICITUD,7,0),3)||
                        'ACT',
           LPAD(per.cip,7,0) ||''||            
           SUBSTR(pkg_syst902.f_obt_tbldescri(sp.tiposolicitud,sp.tipoprestamo) ,1,3) ||''||  
           SUBSTR(pkg_syst900.f_obt_tbldescri(22, pre.moneda),1,1)  ||''||
           LPAD(pre.PeriodoSolicitud,4,0)||''||           
           LPAD(pre.NumeroSolicitud,7,0) ||
           'ACT'||
           TO_CHAR(HOY,'YYMMDD')
           ) PAGO_ID_CONTINENTAL ,                 
           pc.AMORTIZACION + pc.INTERES as Monto_MINIMO,
           pc.AMORTIZACION AS SALDOCAPITAL,
           pc.INTERES AS SALDOINTERES,
           0 AS SALDOMORA,
           NVL(pc.segurointeres,0) AS SEGUROINTERES,
           NVL(pc.portes,0) AS APORTES,
           NVL(pc.reajuste,0) AS REAJUSTE,
           NVL(pc.fechavencimiento,TRUNC(HOY)) AS FECHAVENCIMIENTO,
           0 DIASATRASO,
           pre.codigopersona,
           LPAD(TRIM(TO_CHAR(PC.NUMEROCUOTA,'999')),3,'0') AS numerocuota,
           SUBSTR(pkg_syst902.f_obt_tbldescri(pd.tiposolicitud,pd.tipoprestamo) ,1,3)TipoProducto,
           pre.PeriodoSolicitud , LPAD(pre.NumeroSolicitud,7,'0')NumeroSolicitud
      FROM prestamo pre
    INNER JOIN (SELECT periodosolicitud, numerosolicitud, MAX(numeroampliacion) nroampl
                  FROM prestamodetalle GROUP BY periodosolicitud, numerosolicitud) presdet
    ON presdet.periodosolicitud = pre.periodosolicitud AND presdet.numerosolicitud = pre.numerosolicitud
    INNER JOIN prestamodetalle pd ON pd.periodosolicitud = presdet.periodosolicitud AND pd.numerosolicitud = presdet.numerosolicitud AND pd.numeroampliacion = presdet.nroampl
    INNER JOIN solicitudprestamo sp ON PRE.PERIODOSOLICITUD= sp.PERIODOSOLICITUD AND PRE.NUMEROSOLICITUD = sp.NUMEROSOLICITUD
    INNER JOIN persona per ON per.codigopersona = pre.codigopersona
    INNER JOIN prestamocuotas pc ON pre.periodosolicitud = pc.periodosolicitud AND pre.numerosolicitud  = pc.numerosolicitud
    INNER JOIN  ( SELECT p.codigopersona, TO_CHAR(p.numeroruc) AS nrodoc from persona p INNER JOIN datossocio ds ON p.codigopersona =ds.codigopersona WHERE p.tipopersona = 2
                        UNION ALL SELECT pn.codigopersona, pn.numerodocumentoid AS nrodoc FROM personanatural pn INNER JOIN datossocio ds ON pn.codigopersona =ds.codigopersona) soc ON soc.codigopersona = pre.codigopersona

     WHERE pc.estado = 2      
       AND LENGTH(TRIM(soc.nrodoc))>=8
       AND pre.moneda =PIMONEDA
       AND pre.SALDOPRESTAMO > 0 -- prestamos con monto adeudado pendiente
       AND PRE.PERIODOSOLICITUD = 1
       AND pre.periodosolicitudconcesional IS NULL
       AND pre.numerosolicitudconcesional IS NULL
       AND (pre.periodosolicitud, pre.numerosolicitud) NOT IN (SELECT periodosolicitud, numerosolicitud
                                                                  FROM solicitudprestamo
                                                                  WHERE periodosolicitudconcesional IS NOT NULL
                                                                    AND numerosolicitudconcesional IS NOT NULL)
      --<04.03.2020-Richard Rodriguez -Para que traiga la primera cuota en estado vigente-Techo propio>                                                              
       /*AND TRUNC(pc.fechavencimiento) IN (SELECT fechavencimiento  FROM prestamocuotas
                                             WHERE periodosolicitud = pre.periodosolicitud
                                               AND numerosolicitud = pre.numerosolicitud
                                               AND estado =2
                                               AND fechavencimiento<= HOY
                                         )*/
        AND pc.numerocuota=1
        AND pc.estado=2
        --<F.04.03.2020-Richard Rodriguez -Para que traiga la primera cuota en estado vigente--Techo propio>   
        AND EXISTS (SELECT *
              FROM viviendaptp
              WHERE codigopersona = pre.codigopersona )
        AND EXISTS (    SELECT *
             FROM CuentaCorriente
            WHERE TipoTransaccion =2
              And Moneda = PIMoneda
              AND estado = 1
              AND CodigoPersona = pre.codigopersona
              AND tablaservicio=101
              AND argumentoservicio IN (13,14));

	vNumerocuota		prestamocuotas.numerocuota%TYPE;

	vTotalAmortizacion NUMBER(15,2):=0;
	vMontoadeudado   NUMBER(15,2):=0;
	vConteoTotal     NUMBER(9):=0;
	vTipoPersona     NUMBER(1);
	vNumeroDocumento VARCHAR2(15);
	vTipodocumento   VARCHAR2(1);
	vTipodocumentofin  VARCHAR2(1);
      vContaCREDITO      NUMBER(6):=0;
	--
	vSumaCREDITO       NUMBER(15,2):=0;
	--
	v01  VARCHAR2(2);
	v02  VARCHAR2(2);
	v03  VARCHAR2(2);
	v04  VARCHAR2(2);
	v05  VARCHAR2(2);
	v06  VARCHAR2(2);
	vMinimo NUMBER(15,2) :=0;
	
	vfechabloqueo date;
	vSumaTotal NUMBER(15,2):=0;

    --Datos IBK--

    --Tipos de Registo
    vCabeceraIBK        VARCHAR2(400);
    vCuotaIBK           VARCHAR2(400);
    vDetalleIBK         VARCHAR2(400);
    --

    vTipoCabecera       VARCHAR2(2) := '11';
    vCodGrupo           VARCHAR2(2) := '21';
    vCodRubro           VARCHAR2(2) := '03';
    vCodEmpresa         VARCHAR2(3) := '953';
    vCodServicio        VARCHAR2(2) := '01';
    vCodUnico           VARCHAR2(10) := '0005457247';

    vTipoCuota          VARCHAR2(2) := '12';

    vTipoDetalle        VARCHAR2(2) := '13';

    vMonedaIBK          VARCHAR2(2) := '12';

    vContaCreditoIBK      NUMBER(6):=0; --Cuenta solo < 10M
	vSumaCreditoIBK     NUMBER(15,2) := 0; --Suma solo < 10M

    --Fin Datos IBK--

BEGIN

	EXECUTE IMMEDIATE 'TRUNCATE TABLE recaudainterbank';
	COMMIT;

	EXECUTE IMMEDIATE 'ALTER SESSION set NLS_LANGUAGE = "SPANISH" ';
	EXECUTE IMMEDIATE 'ALTER SESSION set NLS_TERRITORY = "SPAIN" ';

FOR x IN detalle LOOP
             vTotalAmortizacion  := NVL(x.SALDOCAPITAL,0) + NVL(x.SEGUROINTERES,0) +  NVL(x.APORTES,0) + NVL(x.REAJUSTE,0);
             vMontoadeudado :=  vTotalAmortizacion + NVL(x.SALDOINTERES,0) +  NVL(x.SALDOMORA,0);

             IF NVL (vTotalAmortizacion, 0) > 0 THEN
                IF NVL (x.Monto_Minimo, 0) >= 0
                   AND NVL (x.SALDOCAPITAL, 0) >= 0
                   AND NVL (x.SALDOINTERES, 0) >= 0
                   AND NVL (x.SALDOMORA, 0) >= 0
                   AND NVL (x.SEGUROINTERES, 0) >= 0
                   AND NVL (x.APORTES, 0) >= 0
                   AND NVL (x.REAJUSTE, 0) >= 0
                THEN
                    IF x.Tipoproducto <> 'PTP' THEN
                       --vTotalAdeudado := vTotalAdeudado + vMontoadeudado;
                       --vTotalMinimo   := vTotalMinimo + vminimo;
                       --vContaCREDITO := vContaCREDITO + 1;
                       IF NVL(vMontoadeudado, 0) < 10000000 THEN -- IBK solo acepta importes de 9 digitos (7 enteros y 2 decimales)
                        vContaCreditoIBK := vContaCreditoIBK + 1;
                        vSumaCreditoIBK  := vSumaCreditoIBK + FLOOR(vMontoadeudado*100); --Sin separador Decimal
                       END IF;
                       
                        --Insercion de la data enviada en los txt de recaudacion en la tabla RECAUDACIONENVIO -- David Chara 16-01-2020 
                        /*DECLARE
                        ITEMENVIO NUMBER;
                        PERIODOSOLICITUDENVIO VARCHAR2(40);
                        TIPOCUOTAENVIO VARCHAR2(3);
                        MONEDAENVIO VARCHAR2(10);
                        BEGIN
                        SELECT MAX(ITEM) INTO ITEMENVIO FROM RECAUDACIONENVIO WHERE FECHA=TRUNC(TO_DATE(SYSDATE,'DD/MM/YYYY'));
                        
                        PERIODOSOLICITUDENVIO := SUBSTR(x.PAGO_ID, -14, 4);
                        TIPOCUOTAENVIO := SUBSTR(x.PAGO_ID, -3, 3);
                        
                        IF PIMONEDA = 1 THEN
                            MONEDAENVIO :='PEN';
                        ELSE
                             MONEDAENVIO :='USD';             
                        END IF;
                        
                        
                        INSERT INTO RECAUDACIONENVIO (FECHA,ITEM,PERIODOSOLICITUD,NUMEROSOLICITUD,REFERENCIA,NROCUOTA,TIPOCUOTA,CAPITAL,MORA,INTERES,TOTAL,FECHAGENERACION,MONEDA,FECHAVENCIMIENTO)
                        VALUES ( TRUNC(TO_DATE(SYSDATE,'DD/MM/YYYY')), NVL(ITEMENVIO,0) +1,PERIODOSOLICITUDENVIO, x.NumeroSolicitud, x.NOM_CLIENTE, x.numerocuota, 
                        TIPOCUOTAENVIO,x.SALDOCAPITAL,x.SALDOMORA,x.SALDOINTERES,vMontoadeudado,SYSDATE,MONEDAENVIO,TO_CHAR(x.fechavencimiento,'DD/MM/YYYY'));
                        END;*/
                       --Insercion de la data enviada en los txt de recaudacion en la tabla RECAUDACIONENVIO -- David Chara 16-01-2020
                       
                                             
                    END IF;
                END IF;
             END IF;

         END LOOP;
        IF PIMONEDA = 1 THEN
            vMonedaIBK := '01';
        ELSE
            vMonedaIBK := '10';
        END IF;
        

        vCabeceraIBK := vTipoCabecera ||                            --Tipo de registro
                        vCodGrupo ||                                --Código de grupo
                        vCodRubro ||                                --Código de rubro
                        vCodEmpresa ||                              --Código de empresa
                        vCodServicio ||                             --Código de servicio
                        '01' ||                                     --Código de solicitud
                        RPAD('PRESTAMO', 30, ' ') ||                --Descripción de solicitud
                        '0' ||                                      --Origen de la Solicitud
                        '002' ||                                    --Código de requerimiento
                        '1' ||                                      --Canal de envío
                        (CASE WHEN (PIMONEDA) = 1 THEN 'M' ELSE 'A' END) ||                                      --Tipo de información --M: Reemplaza --A:Agrega
                        LPAD(vContaCreditoIBK, 15, '0') ||          --Número de registros
                        vCodUnico ||                                --Código único
                        TO_CHAR(HOY, 'YYYYMMDD') ||                 --Fecha de proceso
                        '00000000' ||                               --Fecha de Inicio de Cargos
                        vMonedaIBK ||                               --Moneda
                        LPAD((CASE WHEN (PIMONEDA) = 1 THEN NVL(vSumaCreditoIBK, 0) ELSE 0 END), 15, '0') ||
                                                                    --Suma Total Soles
                        LPAD((CASE WHEN (PIMONEDA) = 2 THEN NVL(vSumaCreditoIBK, 0) ELSE 0 END), 15, '0') ||
                                                                    --Suma Total Dolares
                        'P' ||                                      --Tipo de Glosa        
                        LPAD(' ', 50, ' ') ||                       --Glosa General
                        LPAD(' ', 221, ' ') ||                      --Glosa General
                        '02' ||                                     --Tipo Formato
                        '0000';                                     --Código fijo

        vCuotaIBK :=    vTipoCuota ||                               --Tipo de registro
                        '00000000' ||                               --Código de cuota
                        '1' ||                                      --Número de conceptos
                        RPAD(UPPER(TRIM(TO_CHAR(HOY, 'MONTH'))), 10, ' ') ||
                                                                    --Descripción del concepto 1
                        LPAD(' ', 10,' ') ||                        --Descripción del concepto 2
                        LPAD(' ', 10,' ') ||                        --Descripción del concepto 3
                        LPAD(' ', 10,' ') ||                        --Descripción del concepto 4
                        LPAD(' ', 10,' ') ||                        --Descripción del concepto 5
                        LPAD(' ', 10,' ') ||                        --Descripción del concepto 6
                        LPAD(' ', 10,' ') ||                        --Descripción del concepto 7                
                        LPAD(' ', 313,' ') ||                       --Libre
                        '02' ||                                     --Tipo Formato
                        '0000';                                     --Código fijo
        --

         INSERT INTO recaudainterbank ( campo ) VALUES ( vCabeceraIBK );

         INSERT INTO recaudainterbank ( campo ) VALUES ( vCuotaIBK );

	FOR x IN detalle LOOP
        vTipoPersona := pkg_persona.f_obt_tipopersona( x.codigopersona );
        IF vTipoPersona = 1 THEN
            vNumerodocumento := pkg_personanatural.F_OBT_NUMERODOCUMENTOID( x.codigopersona );
            vTipodocumento   := 'L';
            vTipodocumentofin := 'C';
        ELSE
            vNumerodocumento := pkg_persona.F_OBT_NUMERORUC( x.codigopersona );
            vTipodocumento   := 'R';
            vTipodocumentofin := 'R';
        END IF;
        --

        vTotalAmortizacion  := NVL(x.SALDOCAPITAL,0) + NVL(x.SEGUROINTERES,0) +  NVL(x.APORTES,0) + NVL(x.REAJUSTE,0);
        vMontoadeudado :=  vTotalAmortizacion + NVL(x.SALDOINTERES,0) +  NVL(x.SALDOMORA,0);

        vMinimo  := vMontoadeudado;

        --
        IF x.fechavencimiento> TRUNC(HOY) THEN
            vfechabloqueo := x.fechavencimiento;
        ELSE
            vfechabloqueo := TRUNC(HOY);
        END IF;
        --
        IF LENGTH(TRIM(x.numerocuota)) >= 3  THEN
            vNumerocuota := SUBSTR (TRIM(x.numerocuota),2,2);
        ELSE
            vNumerocuota := x.numerocuota;
        END IF;

        vDetalleIBK :=  vTipoDetalle ||                             --Tipo de registro
                        RPAD(TRIM(x.codigosocio), 20, ' ') ||       --Código de deudor
                        RPAD(TRIM(SUBSTR(PKG_PERSONA.F_OBT_NOMBRECOMPLETOBANCOS(pre.codigopersona), 1, 30)), 30, ' ') ||
                                                                    --Nombre del deudor
                        RPAD(x.PeriodoSolicitud, 10, ' ') ||        --Referencia 1
                        RPAD(CASE SUBSTR(x.pago_id, -3, 3) WHEN 'ATR' THEN 'ATRASADO' WHEN 'ACT' THEN 'VIGENTE' ELSE ' ' END, 10, ' ') ||
                                                                    --Referencia 2
                        'A' ||                                      --Tipo de Operación
                        RPAD(x.NumeroSolicitud, 8, ' ') ||          --Código de cuota
                        TO_CHAR(HOY, 'YYYYMMDD') ||                 --Fecha de emisión
                        TO_CHAR(x.fechavencimiento, 'YYYYMMDD') ||  --Fecha de vencimiento
                        RPAD(x.numerocuota || '-' || SUBSTR(x.pago_id, -3, 3) || '-' || x.PeriodoSolicitud, 15, ' ') ||
                                                                    --Número de documento
                        vMonedaIBK ||                               --Moneda de la deuda
                        LPAD(FLOOR((NVL(vMontoadeudado, 0) * 100)), 9, '0') ||
                                                                    --Importe del concepto 1
                        LPAD('0',  9, '0') ||                       --Importe del concepto 2
                        LPAD('0',  9, '0') ||                       --Importe del concepto 3
                        LPAD('0',  9, '0') ||                       --Importe del concepto 4
                        LPAD('0',  9, '0') ||                       --Importe del concepto 5
                        LPAD('0',  9, '0') ||                       --Importe del concepto 6
                        LPAD('0',  9, '0') ||                       --Importe del concepto 7
                        LPAD(' ',  1, ' ') ||                       --Tipo de la cuenta Principal
                        LPAD(' ',  3, ' ') ||                       --Producto de la cuenta Principal
                        LPAD(' ',  2, ' ') ||                       --Moneda de la cuenta Principal
                        LPAD(' ', 20, ' ') ||                       --Numero de la cuenta Principal
                        LPAD(' ', 15, '0') ||                       --Importe a abonar cuenta 1
                        LPAD(' ',  1, ' ') ||                       --Tipo de la cuenta Secundaria
                        LPAD(' ',  3, ' ') ||                       --Producto de la cuenta Secundaria
                        LPAD(' ',  2, ' ') ||                       --Moneda de la cuenta Secundaria
                        LPAD(' ', 20, ' ') ||                       --Numero de la cuenta Secundaria
                        LPAD(' ', 15, '0') ||                       --Importe a abonar cuenta 2
                        LPAD(' ', 67, ' ') ||                       --Glosa Particular
                        LPAD(' ', 68, ' ') ||                       --Libre
                        '02' ||                                     --Tipo Formato
                        '0000';                                     --Código fijo
                                
             IF NVL (vTotalAmortizacion, 0) > 0 THEN
                IF NVL (x.Monto_Minimo, 0) >= 0
                   AND NVL (x.SALDOCAPITAL, 0) >= 0
                   AND NVL (x.SALDOINTERES, 0) >= 0
                   AND NVL (x.SALDOMORA, 0) >= 0
                   AND NVL (x.SEGUROINTERES, 0) >= 0
                   AND NVL (x.APORTES, 0) >= 0
                   AND NVL (x.REAJUSTE, 0) >= 0
                THEN
                    IF x.Tipoproducto <> 'PTP' THEN
                        IF NVL(vMontoadeudado, 0) < 10000000 THEN
                            INSERT INTO recaudaInterbank ( campo ) VALUES ( vDetalleIBK );
                        END IF;
                    END IF;
                END IF;
             END IF;
         END LOOP;

         --
         COMMIT;  --IBK
END;

select * from recaudaInterbank order by campo asc;