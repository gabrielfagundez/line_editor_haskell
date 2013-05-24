	ejecutar_comando_delete_con_dos_dir :: Maybe Comando -> State -> (String, State)
	ejecutar_comando_delete_con_dos_dir (Just (CDeleteT (Direc Ultima off1) (Direc Ultima off2))) st = 
		ejecutar_comando_delete_automatico (maximo + offset1) (maximo + offset2) st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch) = st
			maximo = length buf
			offset1 = foldr (+) 0 off1
			offset2 = foldr (+) 0 off2
	ejecutar_comando_delete_con_dos_dir (Just (CDeleteT (Direc Corriente off1) (Direc Corriente off2))) st = 
		ejecutar_comando_delete_automatico (linea + offset1) (linea + offset2) st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch) = st
			maximo = length buf
			offset1 = foldr (+) 0 off1
			offset2 = foldr (+) 0 off2
	ejecutar_comando_delete_con_dos_dir (Just (CDeleteT (Direc (Abs a) off1) (Direc (Abs b) off2))) st = 
		ejecutar_comando_delete_automatico (a + offset1) (b + offset2) st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch) = st
			maximo = length buf
			offset1 = foldr (+) 0 off1
			offset2 = foldr (+) 0 off2
	ejecutar_comando_delete_con_dos_dir (Just (CDeleteT (Direc (Rel a) off1) (Direc (Rel b) off2))) st = 
		ejecutar_comando_delete_automatico (a + offset1) (b + offset2) st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch) = st
			maximo = length buf
			offset1 = foldr (+) 0 off1
			offset2 = foldr (+) 0 off2
	ejecutar_comando_delete_con_dos_dir (Just (CDeleteT (Direc Corriente off1) (Direc Ultima off2))) st = 
		ejecutar_comando_delete_automatico (linea + offset1) (maximo + offset2) st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch) = st
			maximo = length buf
			offset1 = foldr (+) 0 off1
			offset2 = foldr (+) 0 off2
	ejecutar_comando_delete_con_dos_dir (Just (CDeleteT (Direc Ultima off1) (Direc Corriente off2))) st = 
		ejecutar_comando_delete_automatico (maximo + offset1) (linea + offset2) st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch) = st
			maximo = length buf
			offset1 = foldr (+) 0 off1
			offset2 = foldr (+) 0 off2
	ejecutar_comando_delete_con_dos_dir (Just (CDeleteT (Direc (Abs a) off1) (Direc (Rel b) off2))) st = 
		ejecutar_comando_delete_automatico (a + offset1) (b + offset2) st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch) = st
			maximo = length buf
			offset1 = foldr (+) 0 off1
			offset2 = foldr (+) 0 off2
	ejecutar_comando_delete_con_dos_dir (Just (CDeleteT (Direc (Rel a) off1) (Direc (Abs b) off2))) st = 
		ejecutar_comando_delete_automatico (a + offset1) (b + offset2) st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch) = st
			maximo = length buf
			offset1 = foldr (+) 0 off1
			offset2 = foldr (+) 0 off2
	ejecutar_comando_delete_con_dos_dir (Just (CDeleteT (Direc Ultima off1) (Direc (Rel b) off2))) st = 
		ejecutar_comando_delete_automatico (maximo + offset1) (b + offset2) st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch) = st
			maximo = length buf
			offset1 = foldr (+) 0 off1
			offset2 = foldr (+) 0 off2
	ejecutar_comando_delete_con_dos_dir (Just (CDeleteT (Direc (Rel a) off1) (Direc Ultima off2))) st = 
		ejecutar_comando_delete_automatico (a + offset1) (maximo + offset2) st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch) = st
			maximo = length buf
			offset1 = foldr (+) 0 off1
			offset2 = foldr (+) 0 off2
	ejecutar_comando_delete_con_dos_dir (Just (CDeleteT (Direc Ultima off1) (Direc (Abs b) off2))) st = 
		ejecutar_comando_delete_automatico (maximo + offset1) (b + offset2) st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch) = st
			maximo = length buf
			offset1 = foldr (+) 0 off1
			offset2 = foldr (+) 0 off2
	ejecutar_comando_delete_con_dos_dir (Just (CDeleteT (Direc (Abs a) off1) (Direc Ultima off2))) st = 
		ejecutar_comando_delete_automatico (a + offset1) (maximo + offset2) st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch) = st
			maximo = length buf
			offset1 = foldr (+) 0 off1
			offset2 = foldr (+) 0 off2
	ejecutar_comando_delete_con_dos_dir (Just (CDeleteT (Direc Corriente off1) (Direc (Rel b) off2))) st = 
		ejecutar_comando_delete_automatico (linea + offset1) (b + offset2) st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch) = st
			maximo = length buf
			offset1 = foldr (+) 0 off1
			offset2 = foldr (+) 0 off2
	ejecutar_comando_delete_con_dos_dir (Just (CDeleteT (Direc (Rel a) off1) (Direc Corriente off2))) st = 
		ejecutar_comando_delete_automatico (a + offset1) (linea + offset2) st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch) = st
			maximo = length buf
			offset1 = foldr (+) 0 off1
			offset2 = foldr (+) 0 off2
	ejecutar_comando_delete_con_dos_dir (Just (CDeleteT (Direc Corriente off1) (Direc (Abs b) off2))) st = 
		ejecutar_comando_delete_automatico (linea + offset1) (b + offset2) st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch) = st
			maximo = length buf
			offset1 = foldr (+) 0 off1
			offset2 = foldr (+) 0 off2
	ejecutar_comando_delete_con_dos_dir (Just (CDeleteT (Direc (Abs a) off1) (Direc Corriente off2))) st = 
		ejecutar_comando_delete_automatico (a + offset1) (linea + offset2) st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch) = st
			maximo = length buf
			offset1 = foldr (+) 0 off1
			offset2 = foldr (+) 0 off2

	ejecutar_comando_delete_automatico :: Int -> Int -> State -> (String, State)
	ejecutar_comando_delete_automatico indice1 indice2 st
		| indice1 > maximo				= ("?\n", (linea, buf, modo, esta_modificado, 'n', nom_arch))	
		| indice2 > maximo				= ("?\n", (linea, buf, modo, esta_modificado, 'n', nom_arch))	
		| indice1 <= 0						= ("?\n", (linea, buf, modo, esta_modificado, 'n', nom_arch))	
		|	indice1 <= 0						= ("?\n", (linea, buf, modo, esta_modificado, 'n', nom_arch))	
		| indice1 > indice2 			= ("?\n", (linea, buf, modo, esta_modificado, 'n', nom_arch))	
		| otherwise 							= ("", (indice1, borrar_lineas indice1 indice2 buf, modo, True, 'I', nom_arch))
		where 
			(linea, buf, modo, _, _, nom_arch) = st
			maximo = length buf

	borrar_lineas :: Int -> Int -> [a] -> [a]
	borrar_lineas indice1 indice2 buf = take (indice1-1) buf ++ drop indice2 buf