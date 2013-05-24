module Comandos where

	import ActionParser
	import ArgsParser
	import DirectionsParser
	import ParseLib

	-- Definir Data Comando = ...
	data Comando = 	CExitIncond | CExit | CInsertCurr | CWrite | CWriteArg Arg | CPrintCurr | Int |
									CPrint Direc | CInsert Direc | CAppend Direc | CAppendCurr | CShow Direc | CShowCurr |
									CPrintT Direc Direc |CShowT Direc Direc | CDelete Direc | CDeleteCurr | CDeleteT Direc Direc |
									CChangeT Direc Direc | CChange Direc | CChangeCurr
		deriving (Eq, Show)

	data ConsoleState 	= ModoComando | ModoInsertar deriving (Eq, Ord, Show)
	type LineaActual 		= Int
	type Buffer					= [String]
	type UltimoComando	= Char

	-- El estado del sistema se ve reflejado en esta terna
	type State					= (LineaActual, Buffer, ConsoleState, Bool, UltimoComando, String)
	


	-- *** *** *** *** *** *** --
	-- Funcion que parsea la entrada estandar recibida en el Main
	-- *** *** *** *** *** *** --
	
	parse_string_entrada :: String -> Maybe Comando
	parse_string_entrada string =
	    case filter (null.snd) (comando string) of
	      [(e,"")] -> Just e
	      _        -> Nothing

	-- *** *** *** *** *** *** --
	-- Funcion que retorna un comando
	-- *** *** *** *** *** *** --
	
	comando :: Parse Char Comando
	comando = comando_salir `alt` comando_salir_incondicional `alt` comando_insertar_linea_actual 
						`alt` comando_escribir `alt` comando_escribir_con_argumento `alt` comando_imprimir_actual 
						`alt` comando_imprimir_con_direccion `alt` comando_insertar_con_direccion						
						`alt` comando_append_con_direccion `alt` comando_mostrar_linea_con_direccion
						`alt` comando_imprimir_con_dos_dir `alt` comando_mostrar_linea_con_dos_dir
						`alt` comando_borrar_linea_con_direccion `alt` comando_borrar_actual
						`alt` comando_borrar_con_dos_dir `alt` comando_change_con_dos_dir `alt` comando_change_con_direccion


	-- *** *** *** *** *** *** --
	-- Funciones aplicadas a un unico tipo de comandos
	-- *** *** *** *** *** *** --

	comando_salir :: Parse Char Comando
	comando_salir = (action_parser_cond 'q') `build` const CExit

	comando_salir_incondicional :: Parse Char Comando
	comando_salir_incondicional = (action_parser_cond 'Q') `build` const CExitIncond

	comando_insertar_linea_actual :: Parse Char Comando
	comando_insertar_linea_actual = (action_parser_cond 'i') `build` const CInsertCurr

	comando_append_linea_actual :: Parse Char Comando
	comando_append_linea_actual = (action_parser_cond 'a') `build` const CAppendCurr

	comando_escribir :: Parse Char Comando
	comando_escribir = (action_parser_cond 'w') `build` const CWrite

	comando_escribir_con_argumento :: Parse Char Comando
	comando_escribir_con_argumento = ((action_parser_cond 'w') >*> (token ' ') >*> (args_parser)) `build` \(_, (_, arg)) -> CWriteArg arg 
										
	comando_imprimir_actual :: Parse Char Comando
	comando_imprimir_actual = (action_parser_cond 'p') `build` const CPrintCurr

	comando_borrar_actual :: Parse Char Comando
	comando_borrar_actual = (action_parser_cond 'd') `build` const CDeleteCurr

	comando_show_actual :: Parse Char Comando
	comando_show_actual = (action_parser_cond 'n') `build` const CShowCurr

	comando_change_actual :: Parse Char Comando
	comando_change_actual = (action_parser_cond 'c') `build` const CChangeCurr

	
	comando_imprimir_con_direccion :: Parse Char Comando
	comando_imprimir_con_direccion = (directions_parser >*> (action_parser_cond 'p')) `build` \(dir, _) -> CPrint dir

	comando_insertar_con_direccion :: Parse Char Comando
	comando_insertar_con_direccion = (directions_parser >*> (action_parser_cond 'i')) `build` \(dir, _) -> CInsert dir

	comando_append_con_direccion :: Parse Char Comando
	comando_append_con_direccion = (directions_parser >*> (action_parser_cond 'a')) `build` \(dir, _) -> CAppend dir

	comando_mostrar_linea_con_direccion :: Parse Char Comando
	comando_mostrar_linea_con_direccion = (directions_parser >*> (action_parser_cond 'n')) `build` \(dir, _) -> CShow dir

	comando_borrar_linea_con_direccion :: Parse Char Comando
	comando_borrar_linea_con_direccion = (directions_parser >*> (action_parser_cond 'd')) `build` \(dir, _) -> CDelete dir

	comando_change_con_direccion :: Parse Char Comando
	comando_change_con_direccion = (directions_parser >*> (action_parser_cond 'd')) `build` \(dir, _) -> CChange dir


	comando_imprimir_con_dos_dir :: Parse Char Comando
	comando_imprimir_con_dos_dir = (directions_parser >*> (token ',') >*> directions_parser >*> (action_parser_cond 'p')) `build` \(dir1,(_,(dir2, _))) -> CPrintT dir1 dir2

	comando_mostrar_linea_con_dos_dir :: Parse Char Comando
	comando_mostrar_linea_con_dos_dir = (directions_parser >*> (token ',') >*> directions_parser >*> (action_parser_cond 'n')) `build` \(dir1,(_,(dir2, _))) -> CShowT dir1 dir2

	comando_borrar_con_dos_dir :: Parse Char Comando
	comando_borrar_con_dos_dir = (directions_parser >*> (token ',') >*> directions_parser >*> (action_parser_cond 'd')) `build` \(dir1,(_,(dir2, _))) -> CDeleteT dir1 dir2

	comando_change_con_dos_dir :: Parse Char Comando
	comando_change_con_dos_dir = (directions_parser >*> (token ',') >*> directions_parser >*> (action_parser_cond 'c')) `build` \(dir1,(_,(dir2, _))) -> CChangeT dir1 dir2

	-- *** *** *** *** *** *** --
	-- Ejecucion de los comandos
	-- *** *** *** *** *** *** --

	-- Ejecucion en modo insertar
	ejecutar_comando_modo_insertar :: String -> State -> (String, State)
	ejecutar_comando_modo_insertar "." st = ("", (linea, buf, ModoComando, esta_modificado, ult_com, nom_arch))
		where (linea, buf, _, esta_modificado, ult_com, nom_arch) = st
	ejecutar_comando_modo_insertar string st@(_, _, _, _, 'i', _) = ("", (nueva_linea, nuevo_buffer, modo, True, ult_com, nom_arch))
		where 
			(linea, buf, modo, esta_modificado, ult_com, nom_arch) = st
			nueva_linea = linea
			nuevo_buffer = insert (linea) string buf
	ejecutar_comando_modo_insertar string st@(_, _, _, _, 'a', _) = ("", (nueva_linea, nuevo_buffer, modo, True, ult_com, nom_arch))
		where 
			(linea, buf, modo, esta_modificado, ult_com, nom_arch) = st
			nueva_linea = linea + 1
			nuevo_buffer = insert (linea + 1) string buf



	-- Ejecucion en modo comando
	ejecutar_comando_modo_comando :: Maybe Comando -> State -> (String, State)
	ejecutar_comando_modo_comando comando st 
		| (comando == Just CExit)																=	ejecutar_salir comando st
		| (comando == Just CExitIncond)													= ejecutar_salir_incond comando st
		| (comando == Just CInsertCurr) 												= ejecutar_insertar_actual comando st
		| (comando == Just CWrite)															=	ejecutar_comando_write comando st 
		| (comando == Just CPrintCurr)													=	ejecutar_comando_print_current comando st 
		| (comando == Just CAppendCurr)													= ejecutar_comando_append_current comando st
		| (comando == Just CDeleteCurr)													=	ejecutar_comando_delete_current comando st
		| (comando == Just CShowCurr)														= ejecutar_comando_show_current comando st
		| (comando == Just CChangeCurr)													= ejecutar_comando_change_current comando st
	ejecutar_comando_modo_comando (Just (CWriteArg arg)) st 	= ejecutar_comando_write_con_ruta (Just (CWriteArg arg)) st
	ejecutar_comando_modo_comando (Just (CPrint direc)) st 		= ejecutar_comando_print_con_dir (Just (CPrint direc)) st
	ejecutar_comando_modo_comando (Just (CInsert direc)) st 	= ejecutar_comando_insert_con_dir (Just (CInsert direc)) st
	ejecutar_comando_modo_comando (Just (CAppend direc)) st 	= ejecutar_comando_append_con_dir (Just (CAppend direc)) st
	ejecutar_comando_modo_comando (Just (CShow direc)) st 		= ejecutar_comando_show_con_dir (Just (CShow direc)) st
	ejecutar_comando_modo_comando (Just (CDelete direc)) st 	= ejecutar_comando_delete_con_dir (Just (CDelete direc)) st  
	ejecutar_comando_modo_comando (Just (CChange direc)) st 	= ejecutar_comando_change_con_dir (Just (CChange direc)) st  
	ejecutar_comando_modo_comando (Just (CPrintT direc1 direc2)) st 		= ejecutar_comando_print_con_dos_dir (Just (CPrintT direc1 direc2)) st
 	ejecutar_comando_modo_comando (Just (CShowT direc1 direc2)) st 			= ejecutar_comando_show_con_dos_dir (Just (CShowT direc1 direc2)) st
 	ejecutar_comando_modo_comando (Just (CDeleteT direc1 direc2)) st 		= ejecutar_comando_delete_con_dos_dir (Just (CDeleteT direc1 direc2)) st
 	ejecutar_comando_modo_comando (Just (CChangeT direc1 direc2)) st 		= ejecutar_comando_change_con_dos_dir (Just (CChangeT direc1 direc2)) st



	-- *** *** *** *** *** *** --
	-- Ejecucion de los comandos especificos con una o ninguna direccion
	-- *** *** *** *** *** *** --

	ejecutar_salir :: Maybe Comando -> State -> (String, State)
	ejecutar_salir comando st
		| (esta_modificado) && (ult_com /= 'q')		=	("?\n", (linea, buf, modo, not esta_modificado, 'q', nom_arch))
		| otherwise																= ("", st)
		where (linea, buf, modo, esta_modificado, ult_com, nom_arch) = st

	ejecutar_salir_incond :: Maybe Comando -> State -> (String, State)
	ejecutar_salir_incond comando st = ("", st)

	ejecutar_insertar_actual :: Maybe Comando -> State -> (String, State)
	ejecutar_insertar_actual comando st = ("", (linea, buf, ModoInsertar, esta_modificado, 'Q', nom_arch))
		where (linea, buf, _, esta_modificado, _, nom_arch) = st

	ejecutar_comando_append_current :: Maybe Comando -> State -> (String, State)
	ejecutar_comando_append_current comando st = ("", (linea + 1, buf, ModoInsertar, esta_modificado, 'Q', nom_arch))
		where (linea, buf, _, esta_modificado, _, nom_arch) = st

	ejecutar_comando_write :: Maybe Comando -> State -> (String, State)
	ejecutar_comando_write comando st = ("", (linea, buf, modo, False, 'w', nom_arch))
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch) = st		

	ejecutar_comando_write_con_ruta :: Maybe Comando -> State -> (String, State)
	ejecutar_comando_write_con_ruta comando st = ("", (linea, buf, modo, False, 'w', ruta))
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch) = st	
			Just (CWriteArg ruta) = comando

	ejecutar_comando_print_current :: Maybe Comando -> State -> (String, State)
	ejecutar_comando_print_current comando st = ((obtener_linea linea buf), (linea, buf, modo, esta_modificado, 'p', nom_arch))
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch) = st

	ejecutar_comando_show_current :: Maybe Comando -> State -> (String, State)
	ejecutar_comando_show_current comando st = 
		((show $ linea) ++ "\t" ++ obtener_linea (linea) buf, (linea, buf, modo, esta_modificado, 'n', nom_arch))
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch) = st

	ejecutar_comando_delete_current :: Maybe Comando -> State -> (String, State)
	ejecutar_comando_delete_current comando st = borrar_linea linea st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch) = st

	ejecutar_comando_change_current :: Maybe Comando -> State -> (String, State)
	ejecutar_comando_change_current comando st = borrar_linea_change linea st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch) = st

	ejecutar_comando_print_con_dir :: Maybe Comando -> State -> (String, State)
	ejecutar_comando_print_con_dir (Just (CPrint (Direc Ultima []))) st = 
		(obtener_linea (length buf) buf ,(length buf, buf, modo, esta_modificado, 'p', nom_arch))
		where 
			(_, buf, modo, esta_modificado, _, nom_arch) = st
	ejecutar_comando_print_con_dir (Just (CPrint (Direc Corriente []))) st = 
		((obtener_linea linea buf), (linea, buf, modo, esta_modificado, 'p', nom_arch))
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch) = st
	ejecutar_comando_print_con_dir (Just (CPrint (Direc (Abs a) []))) st 
		| a == 0 										= ("?\n", (linea, buf, modo, esta_modificado, 'p', nom_arch))
		| a > maximo								= ("?\n", (linea, buf, modo, esta_modificado, 'p', nom_arch))
		| otherwise 								= ((obtener_linea a buf), (a, buf, modo, esta_modificado, 'p', nom_arch))
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch) = st
			maximo = length buf
	ejecutar_comando_print_con_dir (Just (CPrint (Direc (Rel a) []))) st 
		| absoluta <= 0 						= ("?\n", (linea, buf, modo, esta_modificado, 'p', nom_arch))
		| absoluta > maximo					= ("?\n", (linea, buf, modo, esta_modificado, 'p', nom_arch))
		| otherwise			 						= ((obtener_linea absoluta buf), (absoluta, buf, modo, esta_modificado, 'p', nom_arch))
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch) = st
			maximo = length buf
			absoluta = linea + a
	ejecutar_comando_print_con_dir (Just (CPrint (Direc Todo []))) st = (compactar buf ,(length buf, buf, modo, esta_modificado, 'p', nom_arch))
		where 
			(_, buf, modo, esta_modificado, _, nom_arch) = st
	ejecutar_comando_print_con_dir (Just (CPrint (Direc Ultima off))) st
		| offset > 0 									= ("?\n", (linea, buf, modo, esta_modificado, 'p', nom_arch))			
		| maximo + offset <= 0					= ("?\n", (linea, buf, modo, esta_modificado, 'p', nom_arch))
		| otherwise 								= ((obtener_linea (maximo + offset) buf), (maximo + offset, buf, modo, esta_modificado, 'p', nom_arch))
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch) = st
			maximo = length buf
			offset = foldr (+) 0 off 
	ejecutar_comando_print_con_dir (Just (CPrint (Direc Corriente off))) st
		| linea + offset > maximo	 		= ("?\n", (linea, buf, modo, esta_modificado, 'p', nom_arch))			
		| linea + offset <= 0					= ("?\n", (linea, buf, modo, esta_modificado, 'p', nom_arch))
		| otherwise 								= ((obtener_linea (linea + offset) buf), (linea + offset, buf, modo, esta_modificado, 'p', nom_arch))
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch) = st
			maximo = length buf
			offset = foldr (+) 0 off 
	ejecutar_comando_print_con_dir (Just (CPrint (Direc (Abs a) off))) st
		| a + offset > maximo	 				= ("?\n", (linea, buf, modo, esta_modificado, 'p', nom_arch))			
		| a + offset <= 0							= ("?\n", (linea, buf, modo, esta_modificado, 'p', nom_arch))
		| otherwise 								= ((obtener_linea (a + offset) buf), (a + offset, buf, modo, esta_modificado, 'p', nom_arch))
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch) = st
			maximo = length buf
			offset = foldr (+) 0 off 
	ejecutar_comando_print_con_dir (Just (CPrint (Direc (Rel a) off))) st
		| a + offset > maximo	 			= ("?\n", (linea, buf, modo, esta_modificado, 'p', nom_arch))			
		| a + offset <= 0							= ("?\n", (linea, buf, modo, esta_modificado, 'p', nom_arch))
		| otherwise 								= ((obtener_linea (a + offset) buf), (a + offset, buf, modo, esta_modificado, 'p', nom_arch))
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch) = st
			maximo = length buf
			offset = foldr (+) 0 off 

	ejecutar_comando_insert_con_dir :: Maybe Comando -> State -> (String, State)
	ejecutar_comando_insert_con_dir (Just (CInsert (Direc Ultima []))) st = ("" ,(length buf, buf, ModoInsertar, esta_modificado, 'i', nom_arch))
		where 
			(_, buf, _, esta_modificado, _, nom_arch) = st
	ejecutar_comando_insert_con_dir (Just (CInsert (Direc Corriente []))) st = ("", (linea, buf, ModoInsertar, esta_modificado, 'i', nom_arch))
		where 
			(linea, buf, _, esta_modificado, _, nom_arch) = st
	ejecutar_comando_insert_con_dir (Just (CInsert (Direc (Abs a) []))) st 
		| a > maximo								= ("?\n", (linea, buf, modo, esta_modificado, 'i', nom_arch))
		| otherwise 								= ("", (a, buf, ModoInsertar, esta_modificado, 'i', nom_arch))
		where  	
			(linea, buf, modo, esta_modificado, _, nom_arch) = st
			maximo = length buf
	ejecutar_comando_insert_con_dir (Just (CInsert (Direc (Rel a) []))) st 
		| absoluta < 0 							= ("?\n", (linea, buf, modo, esta_modificado, 'i', nom_arch))
		| absoluta > maximo					= ("?\n", (linea, buf, modo, esta_modificado, 'i', nom_arch))
		| otherwise			 						= ("", (absoluta, buf, ModoInsertar, esta_modificado, 'i', nom_arch))
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch) = st
			maximo = length buf
			absoluta = linea + a
	ejecutar_comando_insert_con_dir com st = ("?\n", st)

	ejecutar_comando_append_con_dir :: Maybe Comando -> State -> (String, State)
	ejecutar_comando_append_con_dir (Just (CAppend (Direc Ultima []))) st = ("" ,(length buf, buf, ModoInsertar, esta_modificado, 'a', nom_arch))
		where 
			(_, buf, _, esta_modificado, _, nom_arch) = st
	ejecutar_comando_append_con_dir (Just (CAppend (Direc Corriente []))) st = ("", (linea, buf, ModoInsertar, esta_modificado, 'a', nom_arch))
		where 
			(linea, buf, _, esta_modificado, _, nom_arch) = st
	ejecutar_comando_append_con_dir (Just (CAppend (Direc (Abs a) []))) st 
		| a > maximo								= ("?\n", (linea, buf, modo, esta_modificado, 'a', nom_arch))
		| otherwise 								= ("", (a, buf, ModoInsertar, esta_modificado, 'a', nom_arch))
		where  	
			(linea, buf, modo, esta_modificado, _, nom_arch) = st
			maximo = length buf
	ejecutar_comando_append_con_dir (Just (CAppend (Direc (Rel a) []))) st 
		| absoluta < 0 							= ("?\n", (linea, buf, modo, esta_modificado, 'a', nom_arch))
		| absoluta > maximo					= ("?\n", (linea, buf, modo, esta_modificado, 'a', nom_arch))
		| otherwise			 						= ("", (absoluta, buf, ModoInsertar, esta_modificado, 'a', nom_arch))
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch) = st
			maximo = length buf
			absoluta = linea + a
	ejecutar_comando_append_con_dir com st = ("?\n", st)

	ejecutar_comando_show_con_dir :: Maybe Comando -> State -> (String, State)
	ejecutar_comando_show_con_dir (Just (CShow (Direc Ultima []))) st = 
		((show $ length buf) ++ "\t" ++ obtener_linea (length buf) buf ,(length buf, buf, modo, esta_modificado, 'n', nom_arch))
		where 
			(_, buf, modo, esta_modificado, _, nom_arch) = st
	ejecutar_comando_show_con_dir (Just (CShow (Direc Corriente []))) st = 
		((show $ linea) ++ "\t" ++ obtener_linea (linea) buf, (linea, buf, modo, esta_modificado, 'n', nom_arch))
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch) = st
	ejecutar_comando_show_con_dir (Just (CShow (Direc (Abs a) []))) st 
		| a == 0 										= ("?\n", (linea, buf, modo, esta_modificado, 'n', nom_arch))
		| a > maximo								= ("?\n", (linea, buf, modo, esta_modificado, 'n', nom_arch))
		| otherwise 								= ((show $ a) ++ "\t" ++ obtener_linea a buf, (a, buf, modo, esta_modificado, 'n', nom_arch))
		where  	
			(linea, buf, modo, esta_modificado, _, nom_arch) = st
			maximo = length buf
	ejecutar_comando_show_con_dir (Just (CShow (Direc (Rel a) []))) st 
		| absoluta <= 0 						= ("?\n", (linea, buf, modo, esta_modificado, 'n', nom_arch))
		| absoluta > maximo					= ("?\n", (linea, buf, modo, esta_modificado, 'n', nom_arch))
		| otherwise									= ((show $ absoluta) ++ "\t" ++ obtener_linea absoluta buf, (absoluta, buf, modo, esta_modificado, 'n', nom_arch))
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch) = st
			maximo = length buf
			absoluta = linea + a
	ejecutar_comando_show_con_dir (Just (CShow (Direc Ultima off))) st
		| offset > 0 									= ("?\n", (linea, buf, modo, esta_modificado, 'n', nom_arch))			
		| maximo + offset <= 0					= ("?\n", (linea, buf, modo, esta_modificado, 'n', nom_arch))
		| otherwise 								= ((show $ (maximo + offset)) ++ "\t" ++ (obtener_linea (maximo + offset) buf), (maximo + offset, buf, modo, esta_modificado, 'n', nom_arch))
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch) = st
			maximo = length buf
			offset = foldr (+) 0 off 
	ejecutar_comando_show_con_dir (Just (CShow (Direc Corriente off))) st
		| linea + offset > maximo	 		= ("?\n", (linea, buf, modo, esta_modificado, 'n', nom_arch))			
		| linea + offset <= 0					= ("?\n", (linea, buf, modo, esta_modificado, 'n', nom_arch))
		| otherwise 								= ((show $ (linea + offset)) ++ "\t" ++ (obtener_linea (linea + offset) buf), (linea + offset, buf, modo, esta_modificado, 'n', nom_arch))
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch) = st
			maximo = length buf
			offset = foldr (+) 0 off 
	ejecutar_comando_show_con_dir (Just (CShow (Direc (Abs a) off))) st
		| a + offset > maximo	 				= ("?\n", (linea, buf, modo, esta_modificado, 'n', nom_arch))			
		| a + offset <= 0							= ("?\n", (linea, buf, modo, esta_modificado, 'n', nom_arch))
		| otherwise 								= ((show $ (a + offset)) ++ "\t" ++ (obtener_linea (a + offset) buf), (a + offset, buf, modo, esta_modificado, 'n', nom_arch))
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch) = st
			maximo = length buf
			offset = foldr (+) 0 off 
	ejecutar_comando_show_con_dir (Just (CShow (Direc (Rel a) off))) st
		| a + offset > maximo	 			= ("?\n", (linea, buf, modo, esta_modificado, 'n', nom_arch))			
		| a + offset <= 0							= ("?\n", (linea, buf, modo, esta_modificado, 'n', nom_arch))
		| otherwise 								= ((show $ (a + offset)) ++ "\t" ++ (obtener_linea (a + offset) buf), (a + offset, buf, modo, esta_modificado, 'n', nom_arch))
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch) = st
			maximo = length buf
			offset = foldr (+) 0 off 

	ejecutar_comando_delete_con_dir :: Maybe Comando -> State -> (String, State)
	ejecutar_comando_delete_con_dir (Just (CDelete (Direc Ultima off))) st =
		borrar_linea (maximo + offset) st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch) = st
			maximo = length buf
			offset = foldr (+) 0 off
	ejecutar_comando_delete_con_dir (Just (CDelete (Direc Corriente off))) st = 
		borrar_linea (linea + offset) st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch) = st
			maximo = length buf
			offset = foldr (+) 0 off
	ejecutar_comando_delete_con_dir (Just (CDelete (Direc (Abs a) off))) st =
		borrar_linea (a + offset) st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch) = st
			maximo = length buf
			offset = foldr (+) 0 off
	ejecutar_comando_delete_con_dir (Just (CDelete (Direc (Rel a) off))) st =
		borrar_linea (a + offset) st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch) = st
			maximo = length buf
			offset = foldr (+) 0 off

	borrar_linea a st 
		| a <= 0 				= ("?\n", (linea, buf, modo, esta_modificado, 'I', nom_arch))
		| a > maximo 		= ("?\n", (linea, buf, modo, esta_modificado, 'I', nom_arch))
		| otherwise			= ("", (a, borrar_linea_buf linea buf, modo, esta_modificado, 'I', nom_arch))
		where  	
			(linea, buf, modo, esta_modificado, _, nom_arch) = st
			maximo = length buf

	borrar_linea_buf linea [] = []
	borrar_linea_buf linea arr = take (linea-1) arr ++ drop linea arr

	ejecutar_comando_change_con_dir :: Maybe Comando -> State -> (String, State)
	ejecutar_comando_change_con_dir (Just (CChange (Direc Ultima off))) st =
		borrar_linea_change (maximo + offset) st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch) = st
			maximo = length buf
			offset = foldr (+) 0 off
	ejecutar_comando_change_con_dir (Just (CChange (Direc Corriente off))) st = 
		borrar_linea_change (linea + offset) st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch) = st
			maximo = length buf
			offset = foldr (+) 0 off
	ejecutar_comando_change_con_dir (Just (CChange (Direc (Abs a) off))) st =
		borrar_linea_change (a + offset) st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch) = st
			maximo = length buf
			offset = foldr (+) 0 off
	ejecutar_comando_change_con_dir (Just (CChange (Direc (Rel a) off))) st =
		borrar_linea_change (a + offset) st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch) = st
			maximo = length buf
			offset = foldr (+) 0 off

	borrar_linea_change a st 
		| a <= 0 				= ("?\n", (linea, buf, modo, esta_modificado, 'I', nom_arch))
		| a > maximo 		= ("?\n", (linea, buf, modo, esta_modificado, 'I', nom_arch))
		| otherwise			= ("", (a, borrar_linea_change_buf linea buf, ModoInsertar, esta_modificado, 'c', nom_arch))
		where  	
			(linea, buf, modo, esta_modificado, _, nom_arch) = st
			maximo = length buf

	borrar_linea_change_buf linea [] = []
	borrar_linea_change_buf linea arr = take (linea-1) arr ++ drop linea arr

	-- *** *** *** *** *** *** --
	-- Ejecucion de comandos con dos direcciones
	-- *** *** *** *** *** *** --

	ejecutar_comando_show_con_dos_dir :: Maybe Comando -> State -> (String, State)
	ejecutar_comando_show_con_dos_dir (Just (CShowT (Direc Ultima off1) (Direc Ultima off2))) st = 
		ejecutar_comando_show_automatico (maximo + offset1) (maximo + offset2) st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch) = st
			maximo = length buf
			offset1 = foldr (+) 0 off1
			offset2 = foldr (+) 0 off2
	ejecutar_comando_show_con_dos_dir (Just (CShowT (Direc Corriente off1) (Direc Corriente off2))) st = 
		ejecutar_comando_show_automatico (linea + offset1) (linea + offset2) st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch) = st
			maximo = length buf
			offset1 = foldr (+) 0 off1
			offset2 = foldr (+) 0 off2
	ejecutar_comando_show_con_dos_dir (Just (CShowT (Direc (Abs a) off1) (Direc (Abs b) off2))) st = 
		ejecutar_comando_show_automatico (a + offset1) (b + offset2) st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch) = st
			maximo = length buf
			offset1 = foldr (+) 0 off1
			offset2 = foldr (+) 0 off2
	ejecutar_comando_show_con_dos_dir (Just (CShowT (Direc (Rel a) off1) (Direc (Rel b) off2))) st = 
		ejecutar_comando_show_automatico (a + offset1) (b + offset2) st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch) = st
			maximo = length buf
			offset1 = foldr (+) 0 off1
			offset2 = foldr (+) 0 off2
	ejecutar_comando_show_con_dos_dir (Just (CShowT (Direc Corriente off1) (Direc Ultima off2))) st = 
		ejecutar_comando_show_automatico (linea + offset1) (maximo + offset2) st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch) = st
			maximo = length buf
			offset1 = foldr (+) 0 off1
			offset2 = foldr (+) 0 off2
	ejecutar_comando_show_con_dos_dir (Just (CShowT (Direc Ultima off1) (Direc Corriente off2))) st = 
		ejecutar_comando_show_automatico (maximo + offset1) (linea + offset2) st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch) = st
			maximo = length buf
			offset1 = foldr (+) 0 off1
			offset2 = foldr (+) 0 off2
	ejecutar_comando_show_con_dos_dir (Just (CShowT (Direc (Abs a) off1) (Direc (Rel b) off2))) st = 
		ejecutar_comando_show_automatico (a + offset1) (b + offset2) st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch) = st
			maximo = length buf
			offset1 = foldr (+) 0 off1
			offset2 = foldr (+) 0 off2
	ejecutar_comando_show_con_dos_dir (Just (CShowT (Direc (Rel a) off1) (Direc (Abs b) off2))) st = 
		ejecutar_comando_show_automatico (a + offset1) (b + offset2) st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch) = st
			maximo = length buf
			offset1 = foldr (+) 0 off1
			offset2 = foldr (+) 0 off2
	ejecutar_comando_show_con_dos_dir (Just (CShowT (Direc Ultima off1) (Direc (Rel b) off2))) st = 
		ejecutar_comando_show_automatico (maximo + offset1) (b + offset2) st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch) = st
			maximo = length buf
			offset1 = foldr (+) 0 off1
			offset2 = foldr (+) 0 off2
	ejecutar_comando_show_con_dos_dir (Just (CShowT (Direc (Rel a) off1) (Direc Ultima off2))) st = 
		ejecutar_comando_show_automatico (a + offset1) (maximo + offset2) st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch) = st
			maximo = length buf
			offset1 = foldr (+) 0 off1
			offset2 = foldr (+) 0 off2
	ejecutar_comando_show_con_dos_dir (Just (CShowT (Direc Ultima off1) (Direc (Abs b) off2))) st = 
		ejecutar_comando_show_automatico (maximo + offset1) (b + offset2) st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch) = st
			maximo = length buf
			offset1 = foldr (+) 0 off1
			offset2 = foldr (+) 0 off2
	ejecutar_comando_show_con_dos_dir (Just (CShowT (Direc (Abs a) off1) (Direc Ultima off2))) st = 
		ejecutar_comando_show_automatico (a + offset1) (maximo + offset2) st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch) = st
			maximo = length buf
			offset1 = foldr (+) 0 off1
			offset2 = foldr (+) 0 off2
	ejecutar_comando_show_con_dos_dir (Just (CShowT (Direc Corriente off1) (Direc (Rel b) off2))) st = 
		ejecutar_comando_show_automatico (linea + offset1) (b + offset2) st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch) = st
			maximo = length buf
			offset1 = foldr (+) 0 off1
			offset2 = foldr (+) 0 off2
	ejecutar_comando_show_con_dos_dir (Just (CShowT (Direc (Rel a) off1) (Direc Corriente off2))) st = 
		ejecutar_comando_show_automatico (a + offset1) (linea + offset2) st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch) = st
			maximo = length buf
			offset1 = foldr (+) 0 off1
			offset2 = foldr (+) 0 off2
	ejecutar_comando_show_con_dos_dir (Just (CShowT (Direc Corriente off1) (Direc (Abs b) off2))) st = 
		ejecutar_comando_show_automatico (linea + offset1) (b + offset2) st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch) = st
			maximo = length buf
			offset1 = foldr (+) 0 off1
			offset2 = foldr (+) 0 off2
	ejecutar_comando_show_con_dos_dir (Just (CShowT (Direc (Abs a) off1) (Direc Corriente off2))) st = 
		ejecutar_comando_show_automatico (a + offset1) (linea + offset2) st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch) = st
			maximo = length buf
			offset1 = foldr (+) 0 off1
			offset2 = foldr (+) 0 off2

	ejecutar_comando_show_automatico :: Int -> Int -> State -> (String, State)
	ejecutar_comando_show_automatico indice1 indice2 st
		| indice1 > maximo				= ("?\n", (linea, buf, modo, esta_modificado, 'n', nom_arch))	
		| indice2 > maximo				= ("?\n", (linea, buf, modo, esta_modificado, 'n', nom_arch))	
		| indice1 <= 0						= ("?\n", (linea, buf, modo, esta_modificado, 'n', nom_arch))	
		|	indice1 <= 0						= ("?\n", (linea, buf, modo, esta_modificado, 'n', nom_arch))	
		| indice1 > indice2 			= ("?\n", (linea, buf, modo, esta_modificado, 'n', nom_arch))	
		| otherwise 							= (obtener_lineas_con_tabulador_e_indice indice1 indice2 buf, (indice2, buf, modo, esta_modificado, 'n', nom_arch))
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch) = st
			maximo = length buf

	ejecutar_comando_print_con_dos_dir :: Maybe Comando -> State -> (String, State)
	ejecutar_comando_print_con_dos_dir (Just (CPrintT (Direc Ultima off1) (Direc Ultima off2))) st =
		ejecutar_comando_print_automatico (maximo + offset1) (maximo + offset2) st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch) = st
			maximo = length buf
			offset1 = foldr (+) 0 off1
			offset2 = foldr (+) 0 off2
	ejecutar_comando_print_con_dos_dir (Just (CPrintT (Direc Corriente off1) (Direc Corriente  off2))) st = 
		ejecutar_comando_print_automatico (linea + offset1) (linea + offset2) st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch) = st
			maximo = length buf
			offset1 = foldr (+) 0 off1
			offset2 = foldr (+) 0 off2
	ejecutar_comando_print_con_dos_dir (Just (CPrintT (Direc (Abs a) off1) (Direc (Abs b) off2))) st =
		ejecutar_comando_print_automatico (a + offset1) (b + offset2) st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch) = st
			maximo = length buf
			offset1 = foldr (+) 0 off1
			offset2 = foldr (+) 0 off2
	ejecutar_comando_print_con_dos_dir (Just (CPrintT (Direc (Rel a) off1) (Direc (Rel b) off2))) st =
		ejecutar_comando_print_automatico (a + offset1) (b+ offset2) st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch) = st
			maximo = length buf
			offset1 = foldr (+) 0 off1
			offset2 = foldr (+) 0 off2
	ejecutar_comando_print_con_dos_dir (Just (CPrintT (Direc (Abs a) off1) (Direc (Rel b) off2))) st =
		ejecutar_comando_print_automatico (a + offset1) (b + offset2) st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch) = st
			maximo = length buf
			offset1 = foldr (+) 0 off1
			offset2 = foldr (+) 0 off2
	ejecutar_comando_print_con_dos_dir (Just (CPrintT (Direc (Rel a) off1) (Direc (Abs b) off2))) st =
		ejecutar_comando_print_automatico (a + offset1) (b + offset2) st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch) = st
			maximo = length buf
			offset1 = foldr (+) 0 off1
			offset2 = foldr (+) 0 off2
	ejecutar_comando_print_con_dos_dir (Just (CPrintT (Direc Ultima off1) (Direc Corriente off2))) st = 
		ejecutar_comando_print_automatico (maximo + offset1) (linea + offset2) st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch) = st
			maximo = length buf
			offset1 = foldr (+) 0 off1
			offset2 = foldr (+) 0 off2
	ejecutar_comando_print_con_dos_dir (Just (CPrintT (Direc Corriente off1) (Direc Ultima off2))) st = 
		ejecutar_comando_print_automatico (linea + offset1) (maximo + offset2) st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch) = st
			maximo = length buf
			offset1 = foldr (+) 0 off1
			offset2 = foldr (+) 0 off2
	ejecutar_comando_print_con_dos_dir (Just (CPrintT (Direc Corriente off1) (Direc (Abs b) off2))) st = 
		ejecutar_comando_print_automatico (linea + offset1) (b + offset2) st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch) = st
			maximo = length buf
			offset1 = foldr (+) 0 off1
			offset2 = foldr (+) 0 off2
	ejecutar_comando_print_con_dos_dir (Just (CPrintT (Direc Corriente off1) (Direc (Rel b) off2))) st = 
		ejecutar_comando_print_automatico (linea + offset1) (b + offset2) st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch) = st
			maximo = length buf
			offset1 = foldr (+) 0 off1
			offset2 = foldr (+) 0 off2
	ejecutar_comando_print_con_dos_dir (Just (CPrintT (Direc Ultima off1) (Direc (Abs b) off2))) st = 
		ejecutar_comando_print_automatico (maximo + offset1) (b + offset2) st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch) = st
			maximo = length buf
			offset1 = foldr (+) 0 off1
			offset2 = foldr (+) 0 off2
	ejecutar_comando_print_con_dos_dir (Just (CPrintT (Direc Ultima off1) (Direc (Rel b) off2))) st = 
		ejecutar_comando_print_automatico (maximo + offset1) (b + offset2) st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch) = st
			maximo = length buf
			offset1 = foldr (+) 0 off1
			offset2 = foldr (+) 0 off2
	ejecutar_comando_print_con_dos_dir (Just (CPrintT (Direc (Rel a) off1) (Direc Ultima off2))) st = 
		ejecutar_comando_print_automatico (a + offset1) (maximo + offset2) st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch) = st
			maximo = length buf
			offset1 = foldr (+) 0 off1
			offset2 = foldr (+) 0 off2
	ejecutar_comando_print_con_dos_dir (Just (CPrintT (Direc (Rel a) off1) (Direc Corriente off2))) st = 
		ejecutar_comando_print_automatico (a + offset1) (linea + offset2) st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch) = st
			maximo = length buf
			offset1 = foldr (+) 0 off1
			offset2 = foldr (+) 0 off2
	ejecutar_comando_print_con_dos_dir (Just (CPrintT (Direc (Abs a) off1) (Direc Ultima off2))) st = 
		ejecutar_comando_print_automatico (a + offset1) (maximo + offset2) st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch) = st
			maximo = length buf
			offset1 = foldr (+) 0 off1
			offset2 = foldr (+) 0 off2
	ejecutar_comando_print_con_dos_dir (Just (CPrintT (Direc (Abs a) off1) (Direc Corriente off2))) st = 
		ejecutar_comando_print_automatico (a + offset1) (linea + offset2) st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch) = st
			maximo = length buf
			offset1 = foldr (+) 0 off1
			offset2 = foldr (+) 0 off2


	ejecutar_comando_print_automatico :: Int -> Int -> State -> (String, State)
	ejecutar_comando_print_automatico indice1 indice2 st
		| indice1 > maximo				= ("?\n", (linea, buf, modo, esta_modificado, 'p', nom_arch))	
		| indice2 > maximo				= ("?\n", (linea, buf, modo, esta_modificado, 'p', nom_arch))	
		| indice1 <= 0						= ("?\n", (linea, buf, modo, esta_modificado, 'p', nom_arch))	
		|	indice1 <= 0						= ("?\n", (linea, buf, modo, esta_modificado, 'p', nom_arch))	
		| indice1 > indice2 			= ("?\n", (linea, buf, modo, esta_modificado, 'p', nom_arch))	
		| otherwise 							= (obtener_lineas indice1 indice2 buf, (indice2, buf, modo, esta_modificado, 'p', nom_arch))
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch) = st
			maximo = length buf

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
		| indice1 > maximo				= ("?\n", (linea, buf, modo, esta_modificado, 'd', nom_arch))	
		| indice2 > maximo				= ("?\n", (linea, buf, modo, esta_modificado, 'd', nom_arch))	
		| indice1 <= 0						= ("?\n", (linea, buf, modo, esta_modificado, 'd', nom_arch))	
		|	indice1 <= 0						= ("?\n", (linea, buf, modo, esta_modificado, 'd', nom_arch))	
		| indice1 > indice2 			= ("?\n", (linea, buf, modo, esta_modificado, 'd', nom_arch))	
		| otherwise 							= ("", (indice1, borrar_lineas indice1 indice2 buf, modo, True, 'I', nom_arch))
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch) = st
			maximo = length buf

	ejecutar_comando_change_con_dos_dir :: Maybe Comando -> State -> (String, State)
	ejecutar_comando_change_con_dos_dir (Just (CChangeT (Direc Ultima off1) (Direc Ultima off2))) st = 
		ejecutar_comando_change_automatico (maximo + offset1) (maximo + offset2) st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch) = st
			maximo = length buf
			offset1 = foldr (+) 0 off1
			offset2 = foldr (+) 0 off2
	ejecutar_comando_change_con_dos_dir (Just (CChangeT (Direc Corriente off1) (Direc Corriente off2))) st = 
		ejecutar_comando_change_automatico (linea + offset1) (linea + offset2) st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch) = st
			maximo = length buf
			offset1 = foldr (+) 0 off1
			offset2 = foldr (+) 0 off2
	ejecutar_comando_change_con_dos_dir (Just (CChangeT (Direc (Abs a) off1) (Direc (Abs b) off2))) st = 
		ejecutar_comando_change_automatico (a + offset1) (b + offset2) st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch) = st
			maximo = length buf
			offset1 = foldr (+) 0 off1
			offset2 = foldr (+) 0 off2
	ejecutar_comando_change_con_dos_dir (Just (CChangeT (Direc (Rel a) off1) (Direc (Rel b) off2))) st = 
		ejecutar_comando_change_automatico (a + offset1) (b + offset2) st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch) = st
			maximo = length buf
			offset1 = foldr (+) 0 off1
			offset2 = foldr (+) 0 off2
	ejecutar_comando_change_con_dos_dir (Just (CChangeT (Direc Corriente off1) (Direc Ultima off2))) st = 
		ejecutar_comando_change_automatico (linea + offset1) (maximo + offset2) st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch) = st
			maximo = length buf
			offset1 = foldr (+) 0 off1
			offset2 = foldr (+) 0 off2
	ejecutar_comando_change_con_dos_dir (Just (CChangeT (Direc Ultima off1) (Direc Corriente off2))) st = 
		ejecutar_comando_change_automatico (maximo + offset1) (linea + offset2) st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch) = st
			maximo = length buf
			offset1 = foldr (+) 0 off1
			offset2 = foldr (+) 0 off2
	ejecutar_comando_change_con_dos_dir (Just (CChangeT (Direc (Abs a) off1) (Direc (Rel b) off2))) st = 
		ejecutar_comando_change_automatico (a + offset1) (b + offset2) st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch) = st
			maximo = length buf
			offset1 = foldr (+) 0 off1
			offset2 = foldr (+) 0 off2
	ejecutar_comando_change_con_dos_dir (Just (CChangeT (Direc (Rel a) off1) (Direc (Abs b) off2))) st = 
		ejecutar_comando_change_automatico (a + offset1) (b + offset2) st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch) = st
			maximo = length buf
			offset1 = foldr (+) 0 off1
			offset2 = foldr (+) 0 off2
	ejecutar_comando_change_con_dos_dir (Just (CChangeT (Direc Ultima off1) (Direc (Rel b) off2))) st = 
		ejecutar_comando_change_automatico (maximo + offset1) (b + offset2) st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch) = st
			maximo = length buf
			offset1 = foldr (+) 0 off1
			offset2 = foldr (+) 0 off2
	ejecutar_comando_change_con_dos_dir (Just (CChangeT (Direc (Rel a) off1) (Direc Ultima off2))) st = 
		ejecutar_comando_change_automatico (a + offset1) (maximo + offset2) st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch) = st
			maximo = length buf
			offset1 = foldr (+) 0 off1
			offset2 = foldr (+) 0 off2
	ejecutar_comando_change_con_dos_dir (Just (CChangeT (Direc Ultima off1) (Direc (Abs b) off2))) st = 
		ejecutar_comando_change_automatico (maximo + offset1) (b + offset2) st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch) = st
			maximo = length buf
			offset1 = foldr (+) 0 off1
			offset2 = foldr (+) 0 off2
	ejecutar_comando_change_con_dos_dir (Just (CChangeT (Direc (Abs a) off1) (Direc Ultima off2))) st = 
		ejecutar_comando_change_automatico (a + offset1) (maximo + offset2) st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch) = st
			maximo = length buf
			offset1 = foldr (+) 0 off1
			offset2 = foldr (+) 0 off2
	ejecutar_comando_change_con_dos_dir (Just (CChangeT (Direc Corriente off1) (Direc (Rel b) off2))) st = 
		ejecutar_comando_change_automatico (linea + offset1) (b + offset2) st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch) = st
			maximo = length buf
			offset1 = foldr (+) 0 off1
			offset2 = foldr (+) 0 off2
	ejecutar_comando_change_con_dos_dir (Just (CChangeT (Direc (Rel a) off1) (Direc Corriente off2))) st = 
		ejecutar_comando_change_automatico (a + offset1) (linea + offset2) st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch) = st
			maximo = length buf
			offset1 = foldr (+) 0 off1
			offset2 = foldr (+) 0 off2
	ejecutar_comando_change_con_dos_dir (Just (CChangeT (Direc Corriente off1) (Direc (Abs b) off2))) st = 
		ejecutar_comando_change_automatico (linea + offset1) (b + offset2) st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch) = st
			maximo = length buf
			offset1 = foldr (+) 0 off1
			offset2 = foldr (+) 0 off2
	ejecutar_comando_change_con_dos_dir (Just (CChangeT (Direc (Abs a) off1) (Direc Corriente off2))) st = 
		ejecutar_comando_change_automatico (a + offset1) (linea + offset2) st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch) = st
			maximo = length buf
			offset1 = foldr (+) 0 off1
			offset2 = foldr (+) 0 off2

	ejecutar_comando_change_automatico :: Int -> Int -> State -> (String, State)
	ejecutar_comando_change_automatico indice1 indice2 st
		| indice1 > maximo				= ("?\n", (linea, buf, modo, esta_modificado, 'c', nom_arch))	
		| indice2 > maximo				= ("?\n", (linea, buf, modo, esta_modificado, 'c', nom_arch))	
		| indice1 <= 0						= ("?\n", (linea, buf, modo, esta_modificado, 'c', nom_arch))	
		|	indice1 <= 0						= ("?\n", (linea, buf, modo, esta_modificado, 'c', nom_arch))	
		| indice1 > indice2 			= ("?\n", (linea, buf, modo, esta_modificado, 'c', nom_arch))	
		| otherwise 							= ("", (indice1, borrar_lineas indice1 indice2 buf, ModoInsertar, True, 'c', nom_arch))
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch) = st
			maximo = length buf


	borrar_lineas :: Int -> Int -> [a] -> [a]
	borrar_lineas indice1 indice2 buf = take (indice1-1) buf ++ drop indice2 buf


	-- *** *** *** *** *** *** --
	-- Funciones auxiliares
	-- *** *** *** *** *** *** --

	insert :: Int -> String -> Buffer -> Buffer
	insert 0 string buffer 	= [string] ++ buffer
	insert n string []			= [string]
	insert n string (x:xs)	= x : insert (n-1) string xs

	escribir_arreglo_archivo_externo :: FilePath -> [[Char]] -> IO ()
	escribir_arreglo_archivo_externo ruta [] = return()
	escribir_arreglo_archivo_externo ruta (x:xs) = do
		escribir_linea_archivo_externo ruta x	
		escribir_arreglo_archivo_externo ruta xs	

	escribir_linea_archivo_externo :: FilePath -> [Char] -> IO ()
	escribir_linea_archivo_externo ruta linea = do
		appendFile ruta $ linea ++ "\n"

	crear_archivo_externo :: FilePath -> IO ()
	crear_archivo_externo ruta = do
		writeFile ruta ""

	espacios :: Parse Char ()
	espacios xs = [((), dropWhile (==' ') xs)]

	quitar_espacios_string :: String -> String
	quitar_espacios_string xs = dropWhile (== ' ') xs

	obtener_linea :: Int -> [String] -> String
	obtener_linea linea buf = (buf !! (linea - 1)) ++ ['\n']

	compactar :: [String] -> String
	compactar = unlines

	obtener_lineas :: Int -> Int -> [String] -> String
	obtener_lineas ini fin [] = []
	obtener_lineas ini fin arr
		| ini > fin 						=	[]
		| ini == fin 						=	(arr !! (ini - 1)) ++ "\n"
		| otherwise 						= (arr !! (ini - 1)) ++ "\n" ++ obtener_lineas (ini + 1) fin arr  

	obtener_lineas_con_tabulador_e_indice :: Int -> Int -> [String] -> String
	obtener_lineas_con_tabulador_e_indice ini fin [] = []
	obtener_lineas_con_tabulador_e_indice ini fin arr
		| ini > fin 						=	[]
		| ini == fin 						=	(show ini) ++ "\t" ++ (arr !! (ini - 1)) ++ "\n"
		| otherwise 						= (((show ini) ++ "\t" ++ (arr !! (ini - 1))) ++ "\n") ++ obtener_lineas_con_tabulador_e_indice (ini + 1) fin arr