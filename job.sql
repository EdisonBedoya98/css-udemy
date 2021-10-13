PROCEDURE traePuntosProdAutorV2
(
PREGIMEN         IN VARCHAR2,  --D1279,AC161
PACTA            IN NUMBER,
pproduccion      in number,
pcalificacion    in varchar2, --DE NO_CALIFICACI
pporcentaje      in number,
ptipomovimiento  in varchar2,  --NO,AJ,RE (NORMAL, AJUSTE,RETIRO), ojo, restringir el retiro?????
palcance         in varchar2,  -- TA, NA (Todos autores, nuevos autores)
pfechavigencia   in varchar2,
pfechaaplicacion in varchar2,

pnumeroautores   in number,
ptipomaterial    in varchar2,
pcodigomaterial  in varchar2,
pfecharegistro   in varchar2, 
pestado          in varchar2,
panorealizacion  in number, 
pmesrealizacion  in number,

ptabAutores      in obj_tab_autorProdu,

pPUNTOSPRODUCCIO out number,
pPUNTOSEMPLEADOS out number,
pPUNTOSADICIONAL out number,
pconceasignpunta out varchar2,
ptipopunto       out varchar2, --SA, BO = Salario o Bominicacion
poperacion       out varchar2, --S o R = suma o resta
ppuntajeautores  out obj_tab_puntosAutorProd ,
perror           out varchar2,
pmensaje         out varchar2
) IS 


	werror  varchar2(1);
	wmensaje varchar2(500);
	
  wconce varchar2(100);
	wfechaacta date;
	wPUNTOSPRODUCCIO number(8,4); 
	wPUNTOSEMPLEADOS number(8,4);
	wPUNTOSADICIONAL number(8,4);
	
	wnumeroautores  number(4);
	wtipomaterial   varchar2(10);
	wcodigomaterial varchar2(10);
	wfecharegistro  date;
	westado         varchar2(1);
	wanorealizacion number(4);
	wmesrealizacion number(2);

	wtipoobjecion   varchar2(2);
	wtipomaterialobj varchar2(10);  
  wnumeroautoresobj number(4);
	
	wconceasignpunta varchar2(3);
	wnomconceasigna varchar2(50);
	wareaasignacion  VARCHAR2(10);
	wmanejcontrcombi varchar2(1);
	wtipopunto       varchar2(2);
	wdivisor         number(2);
	wdivideautor     varchar2(1);
	wdivisorautor    number(2);
	
	wformaasignacion varchar2(1);
	wmaterial        varchar2(2); --RE,PR,NI
	wtienepuntaje  VARCHAR2(1);
	wpuntosaspira  number(8,4);
	
	wpuntaje       number(8,4);
	woperacion     varchar2(1);
	
	werrorp varchar2(500);  --error al llamar procedimientos

  wporceautom number(3);
  wporcentaje number(3);
  wformamanejo varchar2(1);

  wvecautores varchar2(500);

  wtienecalifanter         varchar2(1);
  wultactareg              number(7);
  wulttipomovimientoreg    varchar2(2);
  wultalcanceaplicacireg   varchar2(2);
  wultpuntosproduccionreg  number(8,4);
  wultpuntosautorreg       number(8,4);
  wultcalificacion         varchar2(10);
  wultsecueobjecprodu      number(6);
  wulttipopunto            varchar2(2);
  
  wfechavigenautor date;
  wfechaaplicautor date;

 	wfechavigenciap date;
 	wfechaaplicacionp date;

  wsecueobjecprodu         number(6);
  wobjacta                 number(7);
  wobjrespuesta            varchar2(100);

  wcont number(3);
  wexiste varchar2(1);

BEGIN
  --NOTA: Ojo: para hacer el control,, debe tener en cuenta tabien los puntos a asignar en este reg. Llevar a una temporal y sumarlos en el control????
  --ppuntajeautores := new obj_tab_puntosAutorProd();

  werror := 'N';
  wPUNTOSPRODUCCIO :=0;
  wPUNTOSEMPLEADOS :=0;
  wPUNTOSADICIONAL :=0;
  woperacion := null;
  wtipopunto := null;
  wconceasignpunta := null;
  wnomconceasigna   := null;
  
  wmensaje := null;
  -- SI NO VIENE PRODUCCION, ASUMO QUE ES NUEVA, OSEA QUE NO APLICABUSCAR CALIFICACIONES ANTERIORES Y NO APLICA aj
  -- POR EL MOMENTO NO SE VA A RECIBIR TIPOMOVIMIENTO = 'RE'
  -- ME LLEGA ARREGLO DE AUTORES
  -- SI NO VIENE PRODUCCION , VIENEN DATOS REQUERIDOS (PASARSELOS A EDWIN)
  --ojo, valdiar datos requeridos
  --valida porcentaje entre 1 y 100
  
  	
  begin
   	select 'S'
   	into wexiste
    from no_regimsalar
    where rsatipocalcusalari in ('C','P') and 
          (rsafechafinvigenci is null or 
          rsafechafinvigenci >= to_date(to_char(sysdate,'dd/mm/yyyy'),'dd/mm/yyyy')) and
          rsacodigo = pregimen;
	exception
    when no_data_found then
      werror := 'S';
      wmensaje := 'Regimen salarial no existe, no es valido o no esta vigente';
      goto fin;		 
	end;    
        
        
  begin
		select actfechareunion
		into wfechaacta
		from no_actas
		where actnumeroacta = Pacta and
			  actregimensalarial   = PREGIMEN;
	exception
    when no_data_found then
      werror := 'S';
      wmensaje := 'Error hallando fecha reunion del acta '||pacta||' y regimen '||pregimen;
      goto fin;		 
	end;    
  if pfechavigencia is null or pfechaaplicacion is null then
     	werror := 'S';
      wmensaje := 'Fechas de vigencia y aplicacion deben ser informadas' ;
      goto fin;		 
  else
    begin
  	  wfechavigenciap := to_date(pfechavigencia,'DD/MM/YYYY');
  	  wfechaaplicacionp := to_date(pfechaaplicacion,'DD/MM/YYYY');
    	if wfechaaplicacionp < wfechavigenciap then
       	werror := 'S';
        wmensaje := 'Fecha de vigencia no puede ser mayor a la fecha de aplicacion' ;
        goto fin;		 
    	end if;	
    exception
    	when others then
       	werror := 'S';
        wmensaje := 'Error leyendo fechas de vigencia y aplicacion' ;
        goto fin;		 
    end;	
  end if;	
  
  if palcance not in ('NA','TA') then
   	werror := 'S';
    wmensaje := 'Alcance enviado es invalido' ;
    goto fin;		 
  end if;   		
  	
  if ptipomovimiento not in ('NO','AJ') then
   	werror := 'S';
    wmensaje := 'Tipo movimiento enviado es invalido' ;
    goto fin;		 
  end if;   		
  if ptipomovimiento = 'AJ' and palcance = 'NA' then
   	werror := 'S';
    wmensaje := 'Tipo movimiento es invalido, Los ajustes deben hacerse para todos los autores' ;
    goto fin;		 
  end if;   		
  	

  if pproduccion is null then
  	if ptipomovimiento = 'AJ' then
     	werror := 'S';
      wmensaje := 'no envio produccion. No se puede hacer ajuste para una produccion no creada' ;
      goto fin;		 
    end if;   		
  	if palcance = 'NA' then
     	werror := 'S';
      wmensaje := 'no envio produccion. No se puede dar puntaje a nuevos autores, para una produccion no creada' ;
      goto fin;		 
    end if;   		
  	if pnumeroautores is null or ptipomaterial is null or pfecharegistro is null or pestado is null or
  		 panorealizacion is null or pmesrealizacion is null then
     	werror := 'S';
      wmensaje := 'Se requieren datos de la produccion a evaluar: NroAutores, TipoMaterial, FechaRegistro, Estado, año y mes realizacion' ;
      goto fin;		 
    else
    	wnumeroautores  := pnumeroautores;
    	wtipomaterial   := ptipomaterial;
    	wcodigomaterial := pcodigomaterial;
    	wfecharegistro  := to_date(pfecharegistro,'DD/MM/YYYY');
      westado         := pestado;
      wanorealizacion := panorealizacion;
      wmesrealizacion := pmesrealizacion;
    end if;
    
  	begin
  		select tmamaterial
  		into wmaterial
  		from no_tipomateri
  		where TMACODIGO = ptipomaterial;
  	exception
  		when no_data_found then
     	werror := 'S';
      wmensaje := 'Error buscando datos del Tipomaterial '||ptipomaterial ;  
      goto fin;		 
  	end;
  	if wmaterial in ('RE','PR') and wcodigomaterial is null then 
     	werror := 'S';
      wmensaje :='Se requieren datos de la produccion a evaluar: CodigoMaterial' ;
      goto fin;		 
  	end if;
  	wtipoobjecion := null;
  	wobjacta := null;
  	wsecueobjecprodu   := null;
  	wobjrespuesta := null;
  	wtipomaterialobj      := null;
    wnumeroautoresobj := null;
  else    
    --si es un ajuste y manda datos de la produccion, se asumen esos para calcular con ese supusto. 
    --En ese momento no se ha cambiado datos	de lap roduccion
  	if ptipomovimiento = 'AJ' then
    	wnumeroautores  := pnumeroautores;
    	wtipomaterial   := ptipomaterial;
    	wcodigomaterial := pcodigomaterial;
    	wfecharegistro  := to_date(pfecharegistro,'DD/MM/YYYY');
      westado         := pestado;
      wanorealizacion := panorealizacion;
      wmesrealizacion := pmesrealizacion;
    else 
	  	begin
	      select pronumeroautores,protipomaterial,procodigomaterial,profecharegistro,
	             eobestado, proanorealizacion , promesrealizacion
	     into wnumeroautores,wtipomaterial, wcodigomaterial,wfecharegistro,
	          westado,wanorealizacion , wmesrealizacion
	     from no_produccion
	     where procodigo = pproduccion;
	  	exception
	      when no_data_found then
	        werror := 'S';
	        wmensaje := 'Error hallando datos de la produccion '||pproduccion;
	        goto fin;		 
	  	end;    
    end if;
  	wtipoobjecion := null;
  	
    begin
    	select obpsecuencia,aronumeroacta, substr(ltrim(obprespuesobjecion),1,100), obptipoobjecion,obptipomaterinuevo, obpnroautoresnue
      into wsecueobjecprodu,  wobjacta, wobjrespuesta, wtipoobjecion,wtipomaterialobj, wnumeroautoresobj
      from no_objecprodu, no_actregobje 
      where aroregimensalarial = Pregimen and
            aroproduccion      = obpproduccion and
            arosecuencobjecion = obpsecuencia and
            obpestadoobjecion  = 'A' and 
            obpproduccion      = pproduccion;

      if  wtipoobjecion = 'TM' then
      	wtipomaterial := wtipomaterialobj;
      end if;	
      if  wtipoobjecion = 'AU' then
      	wnumeroautores := wnumeroautoresobj;
      end if;	
    exception
    	when no_data_found Then
    	  	wtipoobjecion := null;
    	  	wobjacta := null;
    	when too_many_rows then  	
        werror := 'S';
        wmensaje := 'Error hallando objeciones,  mas de una objeccion abierta para la produccion '||pproduccion;
        goto fin;		 
    	
    end;	  
  end if;

  --  Selecciona el concepto de asignación de puntaje 
  begin
    select capcodigo, capareaasignacion, capmanejcontrcombi, captipopunto, CAPNOMBRECORTO        
    into wconceasignpunta, wareaasignacion, wmanejcontrcombi, wtipopunto, wnomconceasigna
    from no_conasipunt
    where capvalorarea = wtipomaterial and
          capareaasignacion = 'PRODUCCION' and
          capregimensalarial = pregimen;
  exception 
    when no_data_found then
      werror := 'S';
      wmensaje := 'No existe concepto para el tipo de material [' ||wtipomaterial || '].';
      goto fin;		 
  end;

  -- Selecciona el origen de puntaje 
  begin
    select tmaformaasignpunta,tmamaterial
    into   wformaasignacion, wmaterial
    from no_tipomateri
    where tmacodigo = wtipomaterial;
  end;
  -- Evalua si necesita division de puntaje 
  begin
    select dptdivideautor,dptdivisorautor,dptdivisor 
    into   wdivideautor, wdivisorautor , wdivisor
    from no_divpuntima
    where dptrangofinalautor >= wnumeroautores and
          dptrangoiniciautor <= wnumeroautores and
          dpttipomaterial     = wtipomaterial and
          dptregimensalarial  = pregimen;
  exception 
    when no_data_found then
      wdivisor := 1;
      wdivideautor := 'N';
      wdivisorautor := 1;
  end;


  werrorp := null;
  wtienecalifanter := 'N';
  wultactareg := null;
  wulttipomovimientoreg := null;
  wultalcanceaplicacireg := null;
  wultpuntosproduccionreg := null;
  wultpuntosautorreg := null;
  wultcalificacion := null;
  wultsecueobjecprodu  := null;
  wulttipopunto := null;                              
  
  if pproduccion is not null then
	  HallaUltimaCalificacionReg (pregimen, pproduccion, wtienecalifanter, wultactareg, wulttipomovimientoreg, wultalcanceaplicacireg, 
                              wultpuntosproduccionreg, wultpuntosautorreg, wultcalificacion , wultsecueobjecprodu , wulttipopunto,werrorp);
DBMS_OUTPUT.PUT_LINE('OJO1 TRAJO, '||wulttipopunto||'*'||wtienecalifanter); 
                              
	  if werrorp is not null then
      werror := 'S';
      wmensaje := werrorp;
      goto fin;		 
	  end if;
  end if;
DBMS_OUTPUT.PUT_LINE('OJO, '||wulttipopunto||'*'||WTIPOPUNTO||'*'||PTIPOMOVIMIENTO||'*'||wtienecalifanter); 
  
  if (ptipomovimiento = 'NO') then
    if (wtienecalifanter = 'S' and palcance = 'TA') then
      werror := 'S';
      wmensaje := 'El movimiento normal para todos los autores no se puede realizar. La producción ya tiene una asignación de puntaje.';
      goto fin;		 
    end if;
    if (wtienecalifanter = 'S' and westado ='O') then
      werror := 'S';
      wmensaje := 'El movimiento normal no se puede realizar porque la producción está en estado Objección';
      goto fin;		 
    end if;

    if westado in ('E','A') then
    	wcont :=0;
      select count(*)
        into wcont
        from no_objecprodu 
        where obpestadoobjecion  = 'A' and 
              obpproduccion      = pproduccion;
      if wcont <> 0 then
        werror := 'S';
        wmensaje := 'El movimiento normal no se puede realizar porque la producción tiene una objección abierta.';
        goto fin;		 
      end if;
      
      wcont :=0;
      select count(*) 
      into wcont 
      from no_evaluprodu
      where  eprfechaentreevalu is null and
             eprproduccion      = pproduccion;
      if wcont <> 0 then
        werror := 'S';
        wmensaje := 'El movimiento normal no se puede realizar porque la producción tiene una evaluación pendiente por entregar';
        goto fin;		 
      end if;
    end if;
  end if;
--
  if (ptipomovimiento = 'AJ') then
    if westado = 'R' then
      werror := 'S';
      wmensaje := 'El movimiento de ajuste no se puede realizar porque la producción está en estado Registrada.';
      goto fin;		 
    end if;
    if (wtienecalifanter = 'S' and wtipopunto <> wulttipopunto) then
      werror := 'S';
      wmensaje := 'El movimiento de ajuste no se puede realizar porque el Tipo de Puntaje cambió de '||wulttipopunto||' a '||wtipopunto;
      goto fin;		 
    end if;

    if (wtienecalifanter = 'S' and westado in ('E','O','A')) then

      if wobjacta is null then
        werror := 'S';
        wmensaje := 'El movimiento de ajuste no se puede realizar porque no hay objección abierta o no tiene especificada el acta .';
        goto fin;		 
      end if;
      if (wobjrespuesta is null) then
        werror := 'S';
        wmensaje := 'El movimiento de ajuste no se puede realizar porque la objección relacionada no tiene respuesta del comite.';
        goto fin;		 
      end if;
      if (pacta <> nvl(wobjacta,-1)) then
        werror := 'S';
        wmensaje := 'El movimiento de ajuste no se puede realizar en esta acta porque la objección relacionada tiene un acta diferente.';
        goto fin;		 
      end if;

    end if;
  end if;
--
  if (wtienecalifanter = 'N') then
    if (ptipomovimiento = 'AJ')  then
      werror := 'S';
      wmensaje := 'El movimiento de ajuste no se puede realizar porque no existe una calificación inicial.';
      goto fin;		 
    elsif (palcance = 'NA') then
      werror := 'S';
      wmensaje := 'El movimiento para nuevos autores no se puede realizar por que no existe una calificación inicial.';
      goto fin;		 
    end if;
  end if;


  if pcalificacion <> nvl(wultcalificacion,pcalificacion) then
    if (nvl(wtipoobjecion,'*') <> 'CA' or ptipomovimiento <> 'AJ') then
      werror := 'S';
      wmensaje := 'La calificacion asignada a los nuevos autores debe ser [ '  ||wultcalificacion || ' ].';
      goto fin;		 
    elsif (ptipomovimiento = 'AJ' and wulttipomovimientoreg = ptipomovimiento and wultsecueobjecprodu = wsecueobjecprodu) then
      werror := 'S';
      wmensaje := 'La calificacion asignada a los nuevos autores debe ser [ '  ||wultcalificacion || ' ].';
      goto fin;		 
    end if;
  end if;

  --validar inconsistencias en datos:  ValidaTipoMovimiento

  wtienepuntaje := 'N';

  if (wformaasignacion = 'G') then  --G=Generica
	  werrorp := null;
    TraePuntajeGenerico(wtipomaterial, pregimen, wtienepuntaje,wpuntaje,werrorp); 
dbms_output.put_line('puntaje gen='|| wtienepuntaje||'*'||wpuntaje||'*'||werrorp);     
  else   -- E=Especifica
	  werrorp := null;
    TraePuntajeEspecifico(pregimen,wtipomaterial,wmaterial, wcodigomaterial, wanorealizacion,wmesrealizacion, wtienepuntaje,wpuntaje, werrorp);
dbms_output.put_line('puntaje esp='|| wtienepuntaje||'*'||wpuntaje||'*'||werrorp);     
  end if;
  if werrorp is not null then
    werror := 'S';
    wmensaje := werrorp;
    goto fin;		 
  end if;

  if (wtienepuntaje = 'S') then
    wpuntosaspira := wpuntaje;
dbms_output.put_line('wpuntosaspira='|| wpuntosaspira);     
    werrorp := null;                                         
    wformamanejo := null;                    
    wporceAutom := null;
    TraeDatosCalificacion   (pcalificacion , wporceAutom ,   wformamanejo ,   werrorp);
dbms_output.put_line('calificacion ='|| pcalificacion||'*'||wporceAutom||'*'||wformamanejo);     
	  if werrorp is not null then
      werror := 'S';
      wmensaje := werrorp;
      goto fin;		 
	  end if;
    if wformamanejo = 'M' then --es M = Manual	
    	wporcentaje := pporcentaje;  --como es manual, deben dar el % comoparametro
    else --es A = Automatic	
    	wporcentaje := wporceAutom;  --para calificaciopnes automaticas esta definido el %
    end if;	
dbms_output.put_line('wporcentaje ='|| wporcentaje);     
    --wpuntosproduccio := round((wpuntosaspira) * (wporcentaje/100),1);
    wpuntosproduccio := round((wpuntosaspira) * (wporcentaje/100),4);
dbms_output.put_line('wpuntosproduccio ='|| wpuntosproduccio);     

    if wdivideautor = 'N' then
      --wpuntosempleados := round(wpuntosproduccio/wdivisor,1);
      wpuntosempleados := round(wpuntosproduccio/wdivisor,4);
dbms_output.put_line('wpuntosempleados divideautorN ='|| wpuntosproduccio||'/'||wdivisor||'='||wpuntosempleados);     
    else
      --wpuntosempleados := round(wpuntosproduccio/(wnumeroautores/wdivisorautor),1);
      wpuntosempleados := round(wpuntosproduccio/(wnumeroautores/wdivisorautor),4);
dbms_output.put_line('wpuntosempleados divideautorS ='|| wpuntosproduccio||'/'||wnumeroautores||'/'||wdivisorautor||'='||wpuntosempleados);     
    end if;
    if (ptipomovimiento = 'AJ' and  wtienecalifanter = 'S') then
dbms_output.put_line('por AJ ');     

      if wdivideautor = 'N' then
        --wpuntosadicional := round((wpuntosproduccio - wultpuntosproduccionreg)/wdivisor,1);
        wpuntosadicional := round((wpuntosproduccio - wultpuntosproduccionreg)/wdivisor,4);
dbms_output.put_line('wpuntosadicionalN= '||wpuntosadicional);     
      else
       -- wpuntosadicional    := round((wpuntosproduccio - wultpuntosproduccionreg)/(wnumeroautores/wdivisorautor),1);
        wpuntosadicional    := round((wpuntosproduccio - wultpuntosproduccionreg)/(wnumeroautores/wdivisorautor),4);
dbms_output.put_line('wpuntosadicionalS= '||wpuntosadicional);     
      end if;
      if (wpuntosadicional < 0) then
        woperacion := 'R';
      else
        woperacion := 'S';
      end if;
      wpuntosadicional := abs(wpuntosadicional);
    else
dbms_output.put_line('por else wpuntosadicional= '||wpuntosadicional);     
      wpuntosadicional := 0.0;
dbms_output.put_line('por else wpuntosadicional= '||wpuntosadicional);     
      woperacion := 'S';
    end if;	
  end if;

  if ptabAutores.COUNT() = 0 then
  	werror := 'S';
   	wmensaje := 'Debe definir los autores con sus fechas de vigencia y de aplicacion';
   	goto fin;
  end if;
  wvecautores := ';';

  for p_regAutor in ptabAutores.first .. ptabAutores.last loop
    wvecautores := wvecautores||ptabAutores(p_regAutor).identificacion||';';
  end loop;


  if (palcance = 'TA' ) then
    wexiste := 'N';
    begin
      select 'S' 
      into wexiste
      from (select aprempleado
            from no_autorprodu
            where aprretirado   = 'N' and
                  aprproduccion = pproduccion and
                  aprcompania   = '01'
            minus
            select distinct paaempleado 
            from no_punautacta, no_producacta
            where paacompania        = '01' and
                  paasecueproducacta = pacsecuencia and
                  pacproduccion      = nvl(pproduccion,0) and
                  (pacnumeroacta     = pacta and
                  paatipoasignacion = 'I') and 
                  pacregimensalarial = pregimen)
      where instr(wvecautores,';'||aprempleado||';',1,1) = 0;
    exception
     when no_data_found then
       wexiste := 'N';  
    end;
  else
    wexiste := 'N';
    begin
      select 'S' 
      into wexiste
      from (select aprempleado
               from no_autorprodu
               where aprretirado   = 'N' and
                     aprproduccion = pproduccion and
                     aprcompania   = '01'
               minus
               select distinct paaempleado 
               from no_punautacta, no_producacta
               where paasecueproducacta = pacsecuencia and
                     pacproduccion      = pproduccion and
                     (pacnumeroacta      < pacta  or
                     pacnumeroacta      = pacta and
                     paatipoasignacion = 'I') and 
                     pacregimensalarial = pregimen)
      where instr(wvecautores,';'||aprempleado||';',1,1) = 0;
    exception
     when no_data_found then
       wexiste := 'N';  
    end;
  end if;
  if wexiste = 'S' then
    werror := 'S';
    wmensaje := 'Existen Autores a los que les aplica este puntaje y no se enviaron para la asignacion';
    goto fin;	
  end if;  

  for p_regAutor in ptabAutores.first .. ptabAutores.last loop
    wexiste := 'N';
    begin
      wfechavigenautor := to_date(ptabAutores(p_regAutor).fechavigencia,'DD/MM/YYYY');
    exception
      when others then
        werror := 'S';
        wmensaje := 'Error al leer fecha de vigencia del autor '||ptabAutores(p_regAutor).identificacion;
        goto fin;	
    end;
    begin
      wfechaaplicautor := to_date(ptabAutores(p_regAutor).fechaaplicacion,'DD/MM/YYYY');
    exception
      when others then
        werror := 'S';
        wmensaje := 'Error al leer fecha de aplicacion del autor '||ptabAutores(p_regAutor).identificacion;
        goto fin;	
    end;
    if wfechaaplicautor < wfechavigenautor then
      werror := 'S';
      wmensaje := 'fecha de vigencia > fecha aplicacion para autor '||ptabAutores(p_regAutor).identificacion|| '('||to_char(wfechaaplicautor,'DD/MM/YYYY') ||'*'|| to_char(wfechavigenautor,'DD/MM/YYYY') ||')';
      goto fin;	
    end if;
    
    if wfechaaplicautor < wfechaaplicacionp then
      werror := 'S';
      wmensaje := 'fecha de aplicacion del autor < fecha aplicacion produccion, para autor '||ptabAutores(p_regAutor).identificacion|| '('||to_char(wfechaaplicautor,'DD/MM/YYYY') ||'*'|| to_char(wfechaaplicacionp,'DD/MM/YYYY') ||')' ;
      goto fin;	
    end if;
/*   --emn consulta, no se han creado necesariamente los autores a la produccion asi que esto no se puede validar 
    if (palcance = 'TA' ) then
      begin
        select 'S' 
        into wexiste
        from (select aprempleado
              from no_autorprodu
              where aprretirado   = 'N' and
                    aprproduccion = pproduccion and
                    aprcompania   = '01'
               minus
               select distinct paaempleado 
               from no_punautacta, no_producacta
               where paacompania        = '01' and
                     paasecueproducacta = pacsecuencia and
                     pacproduccion      = nvl(pproduccion,0) and
                     (pacnumeroacta     = pacta and
                     paatipoasignacion = 'I') and 
                     pacregimensalarial = pregimen)
         where aprempleado = ptabAutores(p_regAutor).identificacion and
             rownum = 1;
       exception
         when no_data_found then
           wexiste := 'N';
       end;    
    else
      begin
        select 'S' 
        into wexiste
        from (select aprempleado
               from no_autorprodu
               where aprretirado   = 'N' and
                     aprproduccion = pproduccion and
                     aprcompania   = '01'
               minus
               select distinct paaempleado 
               from no_punautacta, no_producacta
               where paasecueproducacta = pacsecuencia and
                     pacproduccion      = pproduccion and
                     (pacnumeroacta      < pacta  or
                     pacnumeroacta      = pacta and
                     paatipoasignacion = 'I') and 
                     pacregimensalarial = pregimen)
         where aprempleado = ptabAutores(p_regAutor).identificacion and
             rownum = 1;
       exception
         when no_data_found then
           wexiste := 'N';
       end;    
             
    end if;
    if wexiste = 'N' then
      werror := 'S';
      wmensaje := 'Autor '||ptabAutores(p_regAutor).identificacion||' enviado, no es valido para la asignacion';
      goto fin;	
    end if;  
*/
  end loop;

--ojo
  << fin >>
    perror := werror;
    pmensaje := wmensaje;	 
    if perror = 'S' then
      wPUNTOSPRODUCCIO :=-1;
		  wPUNTOSEMPLEADOS :=-1; 
		  wPUNTOSADICIONAL :=-1;
		  woperacion       := null;
		  wtipopunto       := null;
		  wconceasignpunta := null;
		  ppuntajeautores := new obj_tab_puntosAutorProd();
    else
dbms_output.put_line('INICIARA AUTORES.....' );    	
      --ppuntajeautores := new obj_tab_puntosAutorProd();

    	ConsultaPuntajeAutores2(  pproduccion,  pregimen,  pacta,  wfechaacta,  wfecharegistro,  ptipomovimiento,  palcance,
                           wtipopunto,  woperacion,  wconceasignpunta,  wareaasignacion, wfechavigenciap, wfechaaplicacionp,
                           wpuntosempleados,  wpuntosadicional,  wmanejcontrcombi,   ptabAutores, ppuntajeautores); 
                           
dbms_output.put_line('termina autores' );    	

--      NULL;
    end if;    	
dbms_output.put_line('puntos para la produccion '||wPUNTOSPRODUCCIO||'*'|| wPUNTOSEMPLEADOS||'*'||wPUNTOSADICIONAL);    	
    
    wconce := null;
    select decode(wconceasignpunta, null,wconceasignpunta,wconceasignpunta||' '||wnomconceasigna)
    into wconce
    from dual;

    pPUNTOSPRODUCCIO := wPUNTOSPRODUCCIO;
    pPUNTOSEMPLEADOS := wPUNTOSEMPLEADOS;
    pPUNTOSADICIONAL := wPUNTOSADICIONAL;
    pconceasignpunta := wconce;
    poperacion       := woperacion;
    ptipopunto       := wtipopunto ;


dbms_output.put_line('al salir '||wPUNTOSPRODUCCIO||'*'|| wPUNTOSEMPLEADOS||'*'||wPUNTOSADICIONAL||'*'||poperacion ||'*'||pconceasignpunta ||'*'||ptipopunto);    	

END;
