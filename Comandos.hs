module Comandos where

	import ActionParser
	import ArgsParser
	import DirectionsParser
	import ParseLib

	data Comando = 	CExitIncond | CExit | CInsertCurr | CWrite | CWriteD Direc | CWriteT Direc Direc | CWriteArg Arg | CPrintCurr | Int |
									CPrint Direc | CInsert Direc | CAppend Direc | CAppendCurr | CShow Direc | CShowCurr |
									CPrintT Direc Direc |CShowT Direc Direc | CDelete Direc | CDeleteCurr | CDeleteT Direc Direc |
									CChangeT Direc Direc | CChange Direc | CChangeCurr | CYankCurr | CYankT Direc Direc | CYank Direc |
									CPasteCurr | CPaste Direc | CChangeDir Direc | CEqual Direc | CEqualCurr | CMoveT Direc Direc Direc | 
									CTransferT Direc Direc Direc | CMove Direc Direc | CTransfer Direc Direc | CJoin Direc | CJoinT Direc Direc |
									CMoveUn Direc | CMoveDos Direc Direc | CMoveCurren Direc |
									CTransferUn Direc | CTransferDos Direc Direc | CTransferCurren Direc | CTransferSolo

		deriving (Eq, Show)

	data ConsoleState 	= ModoComando | ModoInsertar deriving (Eq, Ord, Show)
	type LineaActual 		= Int
	type Buffer					= [String]
	type UltimoComando	= Char
	type EstaModificado = Bool
	type Papelera = [String]
	type BufferInsert = [String]
	type BufferWrite = [String]

	-- El estado del sistema se ve reflejado en esta terna
	type State					= (LineaActual, Buffer, ConsoleState, EstaModificado, UltimoComando, FilePath, Papelera, Int, BufferInsert, BufferWrite, Int)
	

	-- *** *** *** *** *** *** --
	-- Funcion que parsea la entrada estandar recibida en el Main
	-- *** *** *** *** *** *** --
	
	parse_string_entrada :: String -> (Maybe Comando, Char)
	parse_string_entrada string =
	    case filter (\x -> (snd x == "p") || (snd x == "n") || null (snd x)) (comando string) of
	      [(e,"")] 		-> (Just e, '-')
	      [(e,"p")]		-> (Just e, 'p')
	      [(e,"n")]		-> (Just e, 'n')
	      [(e,"p"), (f,"pp")]		-> (Just e, 'p')
	      [(e,"p"), (f,"pn")]		-> (Just e, 'p')
	      [(e,""), (f,"p")]			-> (Just e, '-')
	      [(e,"n"), (f,"np")]		-> (Just e, 'n')
	      [(e,"n"), (f,"nn")]		-> (Just e, 'n')
	      [(e,""), (f,"n")]			-> (Just e, '-')
	      [(e,""), (f,"")]			-> (Just e, '-')
	      _        		-> (Nothing, '-')

	-- *** *** *** *** *** *** --
	-- Funcion que retorna un comando
	-- *** *** *** *** *** *** --
	
	comando :: Parse Char Comando
	comando = comando_salir `alt` comando_salir_incondicional `alt` comando_insertar_linea_actual 
						`alt` comando_escribir_con_dos_dir `alt` comando_escribir_con_dir 
						`alt` comando_escribir_con_argumento `alt` comando_escribir
						`alt` comando_imprimir_actual 
						`alt` comando_imprimir_con_direccion `alt` comando_insertar_con_direccion						
						`alt` comando_append_con_direccion `alt` comando_mostrar_linea_con_direccion
						`alt` comando_imprimir_con_dos_dir `alt` comando_mostrar_linea_con_dos_dir
						`alt` comando_borrar_linea_con_direccion `alt` comando_borrar_actual
						`alt` comando_borrar_con_dos_dir `alt` comando_change_con_dos_dir `alt` comando_change_con_direccion
						`alt` comando_append_linea_actual `alt` comando_show_actual `alt` comando_copiar_actual
						`alt` comando_yank_con_dos_dir `alt` comando_yank_con_direccion `alt` comando_pegar_actual
						`alt` comando_pegar_con_dir `alt` comando_mostrar_numero_linea
						`alt` comando_mostrar_numer_linea_actual `alt` comando_mover_con_dos_dir `alt` comando_transferir_con_dos_dir
						`alt` comando_change_actual `alt` comando_cambiar_direccion `alt` comando_move_una_dir
						`alt` comando_transfer_una_dir `alt` comando_juntar_con_dir `alt` comando_juntar_con_dos_dir 
						`alt` comando_mover_una_dir `alt` comando_mover_dos_dir `alt` comando_move_curren
						`alt` comando_tra_una_dir `alt` comando_tra_dos_dir `alt` comando_tra_curren
						`alt` comando_tra_solo

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

	comando_copiar_actual :: Parse Char Comando
	comando_copiar_actual = (action_parser_cond 'y') `build` const CYankCurr

	comando_tra_solo :: Parse Char Comando
	comando_tra_solo =  (action_parser_cond 't') `build` const CTransferSolo

	comando_pegar_actual :: Parse Char Comando
	comando_pegar_actual = (action_parser_cond 'x') `build` const CPasteCurr

	comando_mostrar_numer_linea_actual :: Parse Char Comando
	comando_mostrar_numer_linea_actual = (action_parser_cond '=') `build` const CEqualCurr

	comando_escribir_con_dir :: Parse Char Comando
	comando_escribir_con_dir = (directions_parser >*> (action_parser_cond 'w')) `build` \(dir, _) -> CWriteD dir

	comando_move_una_dir :: Parse Char Comando
	comando_move_una_dir = (directions_parser >*> (action_parser_cond 'm') >*> directions_parser) `build` \(dir1,(_ ,dir2)) -> CMove dir1 dir2

	comando_move_curren :: Parse Char Comando
	comando_move_curren = ((action_parser_cond 'm') >*> directions_parser)  `build` \(_, dir) -> CMoveCurren dir

	comando_tra_curren :: Parse Char Comando
	comando_tra_curren = ((action_parser_cond 't') >*> directions_parser)  `build` \(_, dir) -> CTransferCurren dir

	comando_transfer_una_dir :: Parse Char Comando
	comando_transfer_una_dir = (directions_parser >*> (action_parser_cond 't') >*> directions_parser) `build` \(dir1,(_ ,dir2)) -> CTransfer dir1 dir2

	comando_imprimir_con_direccion :: Parse Char Comando
	comando_imprimir_con_direccion = (directions_parser >*> (action_parser_cond 'p')) `build` \(dir, _) -> CPrint dir

	comando_mover_una_dir :: Parse Char Comando
	comando_mover_una_dir = (directions_parser >*> (action_parser_cond 'm')) `build` \(dir, _) -> CMoveUn dir

	comando_tra_una_dir :: Parse Char Comando
	comando_tra_una_dir = (directions_parser >*> (action_parser_cond 't')) `build` \(dir, _) -> CTransferUn dir

	comando_insertar_con_direccion :: Parse Char Comando
	comando_insertar_con_direccion = (directions_parser >*> (action_parser_cond 'i')) `build` \(dir, _) -> CInsert dir

	comando_append_con_direccion :: Parse Char Comando
	comando_append_con_direccion = (directions_parser >*> (action_parser_cond 'a')) `build` \(dir, _) -> CAppend dir

	comando_mostrar_linea_con_direccion :: Parse Char Comando
	comando_mostrar_linea_con_direccion = (directions_parser >*> (action_parser_cond 'n')) `build` \(dir, _) -> CShow dir

	comando_borrar_linea_con_direccion :: Parse Char Comando
	comando_borrar_linea_con_direccion = (directions_parser >*> (action_parser_cond 'd')) `build` \(dir, _) -> CDelete dir

	comando_change_con_direccion :: Parse Char Comando
	comando_change_con_direccion = (directions_parser >*> (action_parser_cond 'c')) `build` \(dir, _) -> CChange dir

	comando_yank_con_direccion :: Parse Char Comando
	comando_yank_con_direccion = (directions_parser >*> (action_parser_cond 'y')) `build` \(dir, _) -> CYank dir

	comando_pegar_con_dir :: Parse Char Comando
	comando_pegar_con_dir = (directions_parser >*> (action_parser_cond 'x')) `build` \(dir, _) -> CPaste dir

	comando_cambiar_direccion :: Parse Char Comando
	comando_cambiar_direccion = directions_parser `build` \dir -> CChangeDir dir 

	comando_mostrar_numero_linea :: Parse Char Comando
	comando_mostrar_numero_linea = (directions_parser >*> (action_parser_cond '=')) `build` \(dir, _) -> CEqual dir

	comando_juntar_con_dir :: Parse Char Comando
	comando_juntar_con_dir = (directions_parser >*> (action_parser_cond 'j')) `build` \(dir, _) -> CJoin dir

	comando_escribir_con_dos_dir :: Parse Char Comando
	comando_escribir_con_dos_dir = (directions_parser >*> (token ',') >*> directions_parser >*> (action_parser_cond 'w')) `build` \(dir1,(_,(dir2, _))) -> CWriteT dir1 dir2

	comando_imprimir_con_dos_dir :: Parse Char Comando
	comando_imprimir_con_dos_dir = (directions_parser >*> (token ',') >*> directions_parser >*> (action_parser_cond 'p')) `build` \(dir1,(_,(dir2, _))) -> CPrintT dir1 dir2

	comando_mover_dos_dir :: Parse Char Comando
	comando_mover_dos_dir = (directions_parser >*> (token ',') >*> directions_parser >*> (action_parser_cond 'm')) `build` \(dir1,(_,(dir2, _))) -> CMoveDos dir1 dir2

	comando_tra_dos_dir :: Parse Char Comando
	comando_tra_dos_dir = (directions_parser >*> (token ',') >*> directions_parser >*> (action_parser_cond 't')) `build` \(dir1,(_,(dir2, _))) -> CTransferDos dir1 dir2

	comando_mostrar_linea_con_dos_dir :: Parse Char Comando
	comando_mostrar_linea_con_dos_dir = (directions_parser >*> (token ',') >*> directions_parser >*> (action_parser_cond 'n')) `build` \(dir1,(_,(dir2, _))) -> CShowT dir1 dir2

	comando_borrar_con_dos_dir :: Parse Char Comando
	comando_borrar_con_dos_dir = (directions_parser >*> (token ',') >*> directions_parser >*> (action_parser_cond 'd')) `build` \(dir1,(_,(dir2, _))) -> CDeleteT dir1 dir2

	comando_change_con_dos_dir :: Parse Char Comando
	comando_change_con_dos_dir = (directions_parser >*> (token ',') >*> directions_parser >*> (action_parser_cond 'c')) `build` \(dir1,(_,(dir2, _))) -> CChangeT dir1 dir2

	comando_yank_con_dos_dir :: Parse Char Comando
	comando_yank_con_dos_dir = (directions_parser >*> (token ',') >*> directions_parser >*> (action_parser_cond 'y')) `build` \(dir1,(_,(dir2, _))) -> CYankT dir1 dir2

	comando_mover_con_dos_dir :: Parse Char Comando
	comando_mover_con_dos_dir = (directions_parser >*> (token ',') >*> directions_parser >*> (action_parser_cond 'm') >*> directions_parser) `build` \(dir1,(_,(dir2,(_, dir3)))) -> CMoveT dir1 dir2 dir3
	
	comando_transferir_con_dos_dir :: Parse Char Comando
	comando_transferir_con_dos_dir = (directions_parser >*> (token ',') >*> directions_parser >*> (action_parser_cond 't') >*> directions_parser) `build` \(dir1,(_,(dir2,(_, dir3)))) -> CTransferT dir1 dir2 dir3

	comando_juntar_con_dos_dir :: Parse Char Comando
	comando_juntar_con_dos_dir = (directions_parser >*> (token ',') >*> directions_parser >*> (action_parser_cond 'j')) `build` \(dir1,(_,(dir2, _))) -> CJoinT dir1 dir2

	-- *** *** *** *** *** *** --
	-- Ejecucion de los comandos
	-- *** *** *** *** *** *** --

	-- *********************************************************************************************** --
	-- Ejecucion en modo insertar
	ejecutar_comando_modo_insertar :: String -> State -> (String, State)
	ejecutar_comando_modo_insertar "." st 
		| ult_com == 'i' 													= ("", (linea + aux - 1, insertar linea buffer_insert buf, ModoComando, esta_modificado, ult_com, nom_arch, papelera, 0, [], auxiliar1, auxiliar2))
		| ult_com == 'a'						 							= ("", (linea + aux, insertar (linea+1) buffer_insert buf, ModoComando, esta_modificado, ult_com, nom_arch, papelera, 0, [], auxiliar1, auxiliar2))		
		| ult_com == 'c'						 							= ("", (linea + aux - 1, insertar linea buffer_insert buf, ModoComando, esta_modificado, ult_com, nom_arch, papelera, 0, [], auxiliar1, auxiliar2))		
		| otherwise 															= error "Modo insertar con comando anterior no definido."--("", (linea + aux - 1, insertar linea papelera buf, ModoComando, esta_modificado, ult_com, nom_arch, [], 0, buffer_insert, auxiliar1, auxiliar2))
		where (linea, buf, _, esta_modificado, ult_com, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
	ejecutar_comando_modo_insertar string st 
		| ult_com == 'i' 		= ("", (linea, buf, modo, True, ult_com, nom_arch, papelera, aux + 1, nuevo_buffer_insert, auxiliar1, auxiliar2))
		| ult_com == 'a' 		= ("", (linea, buf, modo, True, ult_com, nom_arch, papelera, aux + 1, nuevo_buffer_insert, auxiliar1, auxiliar2))
		| ult_com == 'c' 		= ("", (linea, buf, modo, True, ult_com, nom_arch, papelera, aux + 1, nuevo_buffer_insert, auxiliar1, auxiliar2))
		| otherwise 															= error "Modo insertar con comando anterior no definido."--("", (linea + aux - 1, insertar linea papelera buf, ModoComando, esta_modificado, ult_com, nom_arch, [], 0, buffer_insert, auxiliar1, auxiliar2))
		where 
			(linea, buf, modo, esta_modificado, ult_com, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			nueva_linea = linea + 1
			nuevo_buffer_insert = buffer_insert ++ [string]


	-- *********************************************************************************************** --
	-- Ejecucion en modo comando
	ejecutar_comando_modo_comando :: Maybe Comando -> State -> (String, State)
	ejecutar_comando_modo_comando comando st 
		| (comando == Just CExit)																	=	ejecutar_salir comando st
		| (comando == Just CExitIncond)														= ejecutar_salir_incond comando st
		| (comando == Just CInsertCurr) 													= ejecutar_insertar_actual comando st
		| (comando == Just CWrite)																=	ejecutar_comando_write comando st 
		| (comando == Just CPrintCurr)														=	ejecutar_comando_print_current comando st 
		| (comando == Just CAppendCurr)														= ejecutar_comando_append_current comando st
		| (comando == Just CDeleteCurr)														=	ejecutar_comando_delete_current comando st
		| (comando == Just CShowCurr)															= ejecutar_comando_show_current comando st
		| (comando == Just CChangeCurr)														= ejecutar_comando_change_current comando st
		| (comando == Just CYankCurr)															= ejecutar_comando_yank_current comando st
		| (comando == Just CPasteCurr)														= ejecutar_comando_paste_current comando st
		| (comando == Just CEqualCurr)														= ejecutar_comando_equal_actual comando st
		| (comando == Just CTransferSolo)													= ejecutar_comando_tra_solo comando st
	
	ejecutar_comando_modo_comando (Just (CWriteArg arg)) st 		= ejecutar_comando_write_con_ruta (Just (CWriteArg arg)) st
	ejecutar_comando_modo_comando (Just (CWriteD direc)) st 		= ejecutar_comando_write_con_dir (Just (CWriteD direc)) st
	ejecutar_comando_modo_comando (Just (CPrint direc)) st 			= ejecutar_comando_print_con_dir (Just (CPrint direc)) st
	ejecutar_comando_modo_comando (Just (CInsert direc)) st 		= ejecutar_comando_insert_con_dir (Just (CInsert direc)) st
	ejecutar_comando_modo_comando (Just (CAppend direc)) st 		= ejecutar_comando_append_con_dir (Just (CAppend direc)) st
	ejecutar_comando_modo_comando (Just (CShow direc)) st 			= ejecutar_comando_show_con_dir (Just (CShow direc)) st
	ejecutar_comando_modo_comando (Just (CDelete direc)) st 		= ejecutar_comando_delete_con_dir (Just (CDelete direc)) st  
	ejecutar_comando_modo_comando (Just (CChange direc)) st 		= ejecutar_comando_change_con_dir (Just (CChange direc)) st 
	ejecutar_comando_modo_comando (Just (CYank direc)) st 			= ejecutar_comando_yank_con_dir (Just (CYank direc)) st 
	ejecutar_comando_modo_comando (Just (CPaste direc)) st 			= ejecutar_comando_paste_con_dir (Just (CPaste direc)) st 
	ejecutar_comando_modo_comando (Just (CChangeDir direc)) st 	= ejecutar_comando_change_dir (Just (CChangeDir direc)) st
	ejecutar_comando_modo_comando (Just (CEqual direc)) st 			= ejecutar_comando_equal_con_dir (Just (CEqual direc)) st
	ejecutar_comando_modo_comando (Just (CJoin direc)) st 			= ejecutar_comando_join_con_dir (Just (CJoin direc)) st
	ejecutar_comando_modo_comando (Just (CMoveUn direc)) st 		= ejecutar_comando_move_una_dir (Just (CMoveUn direc)) st
	ejecutar_comando_modo_comando (Just (CMoveCurren direc)) st = ejecutar_comando_move_current (Just (CMoveCurren direc)) st
	ejecutar_comando_modo_comando (Just (CTransferUn direc)) st 		= ejecutar_comando_tra_una_dir (Just (CTransferUn direc)) st
	ejecutar_comando_modo_comando (Just (CTransferCurren direc)) st = ejecutar_comando_tra_current (Just (CTransferCurren direc)) st

	ejecutar_comando_modo_comando (Just (CWriteT direc1 direc2)) st 		= ejecutar_comando_write_con_dos_dir (Just (CWriteT direc1 direc2)) st
	ejecutar_comando_modo_comando (Just (CPrintT direc1 direc2)) st 		= ejecutar_comando_print_con_dos_dir (Just (CPrintT direc1 direc2)) st
 	ejecutar_comando_modo_comando (Just (CShowT direc1 direc2)) st 			= ejecutar_comando_show_con_dos_dir (Just (CShowT direc1 direc2)) st
 	ejecutar_comando_modo_comando (Just (CDeleteT direc1 direc2)) st 		= ejecutar_comando_delete_con_dos_dir (Just (CDeleteT direc1 direc2)) st
 	ejecutar_comando_modo_comando (Just (CChangeT direc1 direc2)) st 		= ejecutar_comando_change_con_dos_dir (Just (CChangeT direc1 direc2)) st
 	ejecutar_comando_modo_comando (Just (CYankT direc1 direc2)) st  		= ejecutar_comando_yank_con_dos_dir (Just (CYankT direc1 direc2)) st
	ejecutar_comando_modo_comando (Just (CJoinT direc1 direc2)) st  		= ejecutar_comando_join_con_dos_dir (Just (CJoinT direc1 direc2)) st
	ejecutar_comando_modo_comando (Just (CMoveDos direc1 direc2)) st 		= ejecutar_comando_move_dos_dir (Just (CMoveDos direc1 direc2)) st
	ejecutar_comando_modo_comando (Just (CTransferDos direc1 direc2)) st 		= ejecutar_comando_tra_dos_dir (Just (CTransferDos direc1 direc2)) st

 	ejecutar_comando_modo_comando (Just (CMoveT direc1 direc2 direc3)) st  			= ejecutar_comando_move_con_dos_dir (Just (CMoveT direc1 direc2 direc3)) st
 	ejecutar_comando_modo_comando (Just (CTransferT direc1 direc2 direc3)) st  	= ejecutar_comando_transfer_con_dos_dir (Just (CTransferT direc1 direc2 direc3)) st
 	ejecutar_comando_modo_comando (Just (CMove direc1 direc2)) st 							= ejecutar_comando_move_con_dir (Just (CMove direc1 direc2)) st
 	ejecutar_comando_modo_comando (Just (CTransfer direc1 direc2)) st 					= ejecutar_comando_transfer_con_dir (Just (CTransfer direc1 direc2)) st


	-- *** *** *** *** *** *** --
	-- Ejecucion de los comandos especificos con una o ninguna direccion
	-- *** *** *** *** *** *** --
	
	-- *********************************************************************************************** --
	ejecutar_salir :: Maybe Comando -> State -> (String, State)
	ejecutar_salir comando st
		| (esta_modificado)	=	("?\n", (linea, buf, modo, esta_modificado, 'q', nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2))
		| otherwise					= ("", st)
		where (linea, buf, modo, esta_modificado, ult_com, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st

	ejecutar_salir_incond :: Maybe Comando -> State -> (String, State) 
	ejecutar_salir_incond comando st = ("", st)

	ejecutar_insertar_actual :: Maybe Comando -> State -> (String, State)
	ejecutar_insertar_actual comando st = ("", (linea, buf, ModoInsertar, esta_modificado, 'i', nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2))
		where (linea, buf, _, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st

	ejecutar_comando_append_current :: Maybe Comando -> State -> (String, State)
	ejecutar_comando_append_current comando st = ("", (linea, buf, ModoInsertar, esta_modificado, 'a', nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2))
		where (linea, buf, _, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st

	ejecutar_comando_write :: Maybe Comando -> State -> (String, State)
	ejecutar_comando_write comando st = ("", (linea, buf, modo, False, 'w', nom_arch, papelera, aux, buffer_insert, write_lineas 1 maximo buf, auxiliar2))
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf	

	ejecutar_comando_write_con_ruta :: Maybe Comando -> State -> (String, State)
	ejecutar_comando_write_con_ruta comando st = ("", (linea, buf, modo, False, 'w', ruta, papelera, aux, buffer_insert, write_lineas 1 maximo buf, auxiliar2))
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st	
			Just (CWriteArg ruta) = comando
			maximo = length buf	

	ejecutar_comando_print_current :: Maybe Comando -> State -> (String, State)
	ejecutar_comando_print_current comando st 
		| (length buf) == 0 	= ("?\n", (linea, buf, modo, esta_modificado, 'p', nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2))
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
	ejecutar_comando_print_current comando st = ((obtener_linea linea buf), (linea, buf, modo, esta_modificado, 'p', nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2))
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st

	ejecutar_comando_show_current :: Maybe Comando -> State -> (String, State)
	ejecutar_comando_show_current comando st
		| (length buf) == 0 	= ("?\n", (linea, buf, modo, esta_modificado, 'p', nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2))
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
	ejecutar_comando_show_current comando st = 
		((show $ linea) ++ "\t" ++ obtener_linea linea buf, (linea, buf, modo, esta_modificado, 'n', nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2))
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st

	ejecutar_comando_delete_current :: Maybe Comando -> State -> (String, State)
	ejecutar_comando_delete_current comando st = borrar_linea linea st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st

	ejecutar_comando_tra_solo :: Maybe Comando -> State -> (String, State)
	ejecutar_comando_tra_solo comando st = ejecutar_comando_transfer_con_dos_dir (Just (CTransferT (Direc Corriente []) (Direc Corriente []) (Direc Corriente []))) st

	ejecutar_comando_change_current :: Maybe Comando -> State -> (String, State)
	ejecutar_comando_change_current comando st = borrar_linea_change linea st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st

	ejecutar_comando_yank_current :: Maybe Comando -> State -> (String, State)
	ejecutar_comando_yank_current comando st = ("", (linea, buf, modo, esta_modificado, 'y', nom_arch, [(obtener_linea_sin_salto linea buf)], aux, buffer_insert, auxiliar1, auxiliar2))
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st

	ejecutar_comando_paste_current :: Maybe Comando -> State -> (String, State)
	ejecutar_comando_paste_current comando (linea, buf, modo, esta_modificado, ult_com, nom_arch, [], aux, buffer_insert, auxiliar1, auxiliar2) = ("?\n", (linea, buf, modo, esta_modificado, ult_com, nom_arch, [], aux, buffer_insert, auxiliar1, auxiliar2))
	ejecutar_comando_paste_current comando st = ("", (linea + length papelera, paste papelera linea buf, modo, True, 'x', nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2))
		where 
			(linea, buf, modo, _, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st

	ejecutar_comando_equal_actual :: Maybe Comando -> State -> (String, State)
	ejecutar_comando_equal_actual comando st =
		cambiar_linea_equal maximo st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf


	-- *********************************************************************************************** --
	ejecutar_comando_write_con_dir :: Maybe Comando -> State -> (String, State)
	ejecutar_comando_write_con_dir (Just (CWriteD (Direc Ultima off))) st = 
		write_linea (maximo + offset) st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf
			offset = foldr (+) 0 off 
	ejecutar_comando_write_con_dir (Just (CWriteD (Direc Corriente off))) st =
		write_linea (linea + offset) st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf
			offset = foldr (+) 0 off 
	ejecutar_comando_write_con_dir (Just (CWriteD (Direc (Abs a) off))) st =
		write_linea (a + offset) st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf
			offset = foldr (+) 0 off 
	ejecutar_comando_write_con_dir (Just (CWriteD (Direc (Rel a) off))) st =
		write_linea (a + linea + offset) st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf
			offset = foldr (+) 0 off 
	ejecutar_comando_write_con_dir (Just (CWriteD (Direc Todo off))) st =
		write_linea maximo st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf
	ejecutar_comando_write_con_dir (Just (CWriteD (Direc TdoRelativo off))) st =
		write_linea maximo st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf

	-- DEFAULT
	ejecutar_comando_write_con_dir com st =
		write_linea (maximo + 2) st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf

	write_linea :: Int -> State -> (String, State)
	write_linea indice st 
		| indice > maximo 		= ("?\n" ,(linea, buf, modo, esta_modificado, 'l', nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2))
		| indice <= 0			= ("?\n" ,(linea, buf, modo, esta_modificado, 'l', nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2))
		| otherwise 			= ("" ,(indice, buf, modo, esta_modificado, 'w', nom_arch, papelera, aux, buffer_insert, [buf !! (indice-1)], auxiliar2))
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf

	ejecutar_comando_change_dir :: Maybe Comando -> State -> (String, State)
	ejecutar_comando_change_dir (Just (CChangeDir (Direc Ultima off))) st = 
		cambiar_linea (maximo + offset) st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf
			offset = foldr (+) 0 off 
	ejecutar_comando_change_dir (Just (CChangeDir (Direc Corriente off))) st =
		cambiar_linea (linea + offset) st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf
			offset = foldr (+) 0 off 
	ejecutar_comando_change_dir (Just (CChangeDir (Direc (Abs a) off))) st =
		cambiar_linea (a + offset) st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf
			offset = foldr (+) 0 off 
	ejecutar_comando_change_dir (Just (CChangeDir (Direc (Rel a) off))) st =
		cambiar_linea (a + linea + offset) st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf
			offset = foldr (+) 0 off 
	ejecutar_comando_change_dir (Just (CChangeDir (Direc Todo off))) st =
		cambiar_linea maximo st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf
	ejecutar_comando_change_dir (Just (CChangeDir (Direc TdoRelativo off))) st =
		cambiar_linea maximo st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf


	-- DEFAULT
	ejecutar_comando_change_dir com st =
		cambiar_linea (maximo + 2) st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf
	

	cambiar_linea :: Int -> State -> (String, State)
	cambiar_linea indice st 
		| indice > maximo 		= ("?\n" ,(linea, buf, modo, esta_modificado, 'l', nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2))
		| indice <= 0					= ("?\n" ,(linea, buf, modo, esta_modificado, 'l', nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2))
		| otherwise 					= (obtener_linea indice buf ,(indice, buf, modo, esta_modificado, 'l', nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2))
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf


	-- *********************************************************************************************** --	
	ejecutar_comando_equal_con_dir :: Maybe Comando -> State -> (String, State)
	ejecutar_comando_equal_con_dir (Just (CEqual (Direc Ultima off))) st = 
		cambiar_linea_equal (maximo + offset) st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf
			offset = foldr (+) 0 off 
	ejecutar_comando_equal_con_dir (Just (CEqual (Direc Corriente off))) st =
		cambiar_linea_equal (linea + offset) st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf
			offset = foldr (+) 0 off 
	ejecutar_comando_equal_con_dir (Just (CEqual (Direc (Abs a) off))) st =
		cambiar_linea_equal (a + offset) st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf
			offset = foldr (+) 0 off 
	ejecutar_comando_equal_con_dir (Just (CEqual (Direc (Rel a) off))) st =
		cambiar_linea_equal (a + linea + offset) st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf
			offset = foldr (+) 0 off 
	ejecutar_comando_equal_con_dir (Just (CEqual (Direc Todo off))) st =
		cambiar_linea_equal maximo st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf
	ejecutar_comando_equal_con_dir (Just (CEqual (Direc TdoRelativo off))) st =
		cambiar_linea_equal maximo st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf
	

	-- DEFAULT
	ejecutar_comando_equal_con_dir com st =
		cambiar_linea_equal (maximo + 2) st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf
	

	cambiar_linea_equal :: Int -> State -> (String, State)
	cambiar_linea_equal indice st 
		| indice > maximo 		= ("?\n" ,(linea, buf, modo, esta_modificado, '=', nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2))
		| indice < 0					= ("?\n" ,(linea, buf, modo, esta_modificado, '=', nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2))
		| indice == 0					= ("0\n" ,(linea, buf, modo, esta_modificado, '=', nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2))
		| otherwise 					= ((show indice) ++ "\n" ,(linea, buf, modo, esta_modificado, '=', nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2))
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf


	-- *********************************************************************************************** --
	ejecutar_comando_print_con_dir :: Maybe Comando -> State -> (String, State)
	ejecutar_comando_print_con_dir dir st  
		| (length buf) == 0 = ("?\n" ,(length buf, buf, modo, esta_modificado, 'p', nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2))
		where 
			(_, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
	ejecutar_comando_print_con_dir (Just (CPrint (Direc Ultima off))) st
		| offset > 0 									= ("?\n", (linea, buf, modo, esta_modificado, 'p', nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2))			
		| maximo + offset <= 0					= ("?\n", (linea, buf, modo, esta_modificado, 'p', nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2))
		| otherwise 								= ((obtener_linea (maximo + offset) buf), (maximo + offset, buf, modo, esta_modificado, 'p', nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2))
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf
			offset = foldr (+) 0 off 
	ejecutar_comando_print_con_dir (Just (CPrint (Direc Corriente off))) st
		| linea + offset > maximo	 		= ("?\n", (linea, buf, modo, esta_modificado, 'p', nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2))			
		| linea + offset <= 0					= ("?\n", (linea, buf, modo, esta_modificado, 'p', nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2))
		| otherwise 								= ((obtener_linea (linea + offset) buf), (linea + offset, buf, modo, esta_modificado, 'p', nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2))
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf
			offset = foldr (+) 0 off 
	ejecutar_comando_print_con_dir (Just (CPrint (Direc (Abs a) off))) st
		| a + offset > maximo	 				= ("?\n", (linea, buf, modo, esta_modificado, 'p', nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2))			
		| a + offset <= 0							= ("?\n", (linea, buf, modo, esta_modificado, 'p', nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2))
		| otherwise 								= ((obtener_linea (a + offset) buf), (a + offset, buf, modo, esta_modificado, 'p', nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2))
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf
			offset = foldr (+) 0 off 
	ejecutar_comando_print_con_dir (Just (CPrint (Direc (Rel a) off))) st
		| a + linea + offset > maximo	 			= ("?\n", (linea, buf, modo, esta_modificado, 'p', nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2))			
		| a + linea + offset <= 0							= ("?\n", (linea, buf, modo, esta_modificado, 'p', nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2))
		| otherwise 								= ((obtener_linea (a + linea + offset) buf), (a + linea + offset, buf, modo, esta_modificado, 'p', nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2))
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf
			offset = foldr (+) 0 off 
	ejecutar_comando_print_con_dir (Just (CPrint (Direc Todo []))) st = (compactar buf ,(length buf, buf, modo, esta_modificado, 'p', nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2))
		where 
			(_, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
	ejecutar_comando_print_con_dir (Just (CPrint (Direc TdoRelativo []))) st = ejecutar_comando_print_automatico linea maximo st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf


	-- DEFAULT
	ejecutar_comando_print_con_dir com st = 
		("?\n", (linea, buf, modo, esta_modificado, 'p', nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2))			
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf


	-- *********************************************************************************************** --
	ejecutar_comando_insert_con_dir :: Maybe Comando -> State -> (String, State)
	ejecutar_comando_insert_con_dir (Just (CInsert (Direc Ultima off))) st = ejecutar_insert_automatico (maximo + offset) st
		where 
			maximo = length buf
			(_, buf, _, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			offset = foldr (+) 0 off
	ejecutar_comando_insert_con_dir (Just (CInsert (Direc Corriente off))) st = ejecutar_insert_automatico (linea + offset) st
		where 
			maximo = length buf
			(linea, buf, _, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			offset = foldr (+) 0 off
	ejecutar_comando_insert_con_dir (Just (CInsert (Direc (Abs a) off))) st = ejecutar_insert_automatico (a + offset) st
		where 
			maximo = length buf
			(_, buf, _, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			offset = foldr (+) 0 off
	ejecutar_comando_insert_con_dir (Just (CInsert (Direc (Rel a) off))) st = ejecutar_insert_automatico (a + linea + offset) st
		where 
			maximo = length buf
			(linea, buf, _, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			offset = foldr (+) 0 off
	ejecutar_comando_insert_con_dir (Just (CInsert (Direc Todo []))) st = ejecutar_insert_automatico maximo st
		where 
			maximo = length buf
			(_, buf, _, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
	ejecutar_comando_insert_con_dir (Just (CInsert (Direc TdoRelativo []))) st = ejecutar_insert_automatico maximo st
		where 
			maximo = length buf
			(_, buf, _, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st


	--DEFAULT 
	ejecutar_comando_insert_con_dir com st = ("?\n", st)


	ejecutar_insert_automatico :: Int -> State -> (String, State)
	ejecutar_insert_automatico int st 
		| int > maximo 	= ("?\n", (linea, buf, modo, esta_modificado, 'i', nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2))
		| int < 0 			= ("?\n", (linea, buf, modo, esta_modificado, 'i', nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2))
		| int == 0 			= ("", (1, buf, ModoInsertar, esta_modificado, 'i', nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2))
		| otherwise 		= ("", (int, buf, ModoInsertar, esta_modificado, 'i', nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2))
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf


	-- *********************************************************************************************** --
	ejecutar_comando_append_con_dir :: Maybe Comando -> State -> (String, State)
	ejecutar_comando_append_con_dir (Just (CAppend (Direc Ultima off))) st = ejecutar_append_automatico (maximo + offset) st
		where 
			maximo = length buf
			(_, buf, _, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			offset = foldr (+) 0 off
	ejecutar_comando_append_con_dir (Just (CAppend (Direc Corriente off))) st = ejecutar_append_automatico (linea + offset) st
		where 
			maximo = length buf
			(linea, buf, _, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			offset = foldr (+) 0 off
	ejecutar_comando_append_con_dir (Just (CAppend (Direc (Abs a) off))) st = ejecutar_append_automatico (a + offset) st
		where 
			maximo = length buf
			(_, buf, _, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			offset = foldr (+) 0 off
	ejecutar_comando_append_con_dir (Just (CAppend (Direc (Rel a) off))) st = ejecutar_append_automatico (a + linea + offset) st
		where 
			maximo = length buf
			(linea, buf, _, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			offset = foldr (+) 0 off
	ejecutar_comando_append_con_dir (Just (CAppend (Direc Todo []))) st = ejecutar_append_automatico maximo st
		where 
			maximo = length buf
			(_, buf, _, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
	ejecutar_comando_append_con_dir (Just (CAppend (Direc TdoRelativo []))) st = ejecutar_append_automatico maximo st
		where 
			maximo = length buf
			(_, buf, _, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
	

	-- DEFAULT
	ejecutar_comando_append_con_dir com st = ("?\n", st)


	ejecutar_append_automatico :: Int -> State -> (String, State)
	ejecutar_append_automatico int st 
		| int > maximo 	= ("?\n", (linea, buf, modo, esta_modificado, 'a', nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2))
		| int < 0 			= ("?\n", (linea, buf, modo, esta_modificado, 'a', nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2))
		| int == 0 			= ("", (0, buf, ModoInsertar, esta_modificado, 'a', nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2))
		| otherwise 		= ("", (int, buf, ModoInsertar, esta_modificado, 'a', nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2))
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf


	-- *********************************************************************************************** --
	ejecutar_comando_show_con_dir :: Maybe Comando -> State -> (String, State)
	ejecutar_comando_show_con_dir (Just (CShow (Direc Ultima off))) st
		| offset > 0 									= ("?\n", (linea, buf, modo, esta_modificado, 'n', nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2))			
		| maximo + offset <= 0				= ("?\n", (linea, buf, modo, esta_modificado, 'n', nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2))
		| otherwise 									= ((show $ (maximo + offset)) ++ "\t" ++ (obtener_linea (maximo + offset) buf), (maximo + offset, buf, modo, esta_modificado, 'n', nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2))
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf
			offset = foldr (+) 0 off 
	ejecutar_comando_show_con_dir (Just (CShow (Direc Corriente off))) st
		| linea + offset > maximo	 		= ("?\n", (linea, buf, modo, esta_modificado, 'n', nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2))			
		| linea + offset <= 0					= ("?\n", (linea, buf, modo, esta_modificado, 'n', nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2))
		| otherwise 								 	= ((show $ (linea + offset)) ++ "\t" ++ (obtener_linea (linea + offset) buf), (linea + offset, buf, modo, esta_modificado, 'n', nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2))
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf
			offset = foldr (+) 0 off 
	ejecutar_comando_show_con_dir (Just (CShow (Direc (Abs a) off))) st
		| a + offset > maximo	 				= ("?\n", (linea, buf, modo, esta_modificado, 'n', nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2))			
		| a + offset <= 0							= ("?\n", (linea, buf, modo, esta_modificado, 'n', nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2))
		| otherwise 									= ((show $ (a + offset)) ++ "\t" ++ (obtener_linea (a + offset) buf), (a + offset, buf, modo, esta_modificado, 'n', nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2))
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf
			offset = foldr (+) 0 off 
	ejecutar_comando_show_con_dir (Just (CShow (Direc (Rel a) off))) st
		| a + offset > maximo	 				= ("?\n", (linea, buf, modo, esta_modificado, 'n', nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2))			
		| a + offset <= 0							= ("?\n", (linea, buf, modo, esta_modificado, 'n', nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2))
		| otherwise 									= ((show $ (a + linea + offset)) ++ "\t" ++ (obtener_linea (a + linea + offset) buf), (a + linea + offset, buf, modo, esta_modificado, 'n', nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2))
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf
			offset = foldr (+) 0 off 
	ejecutar_comando_show_con_dir (Just (CShow (Direc Todo []))) st = 
		ejecutar_comando_show_automatico 1 maximo st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf
	ejecutar_comando_show_con_dir (Just (CShow (Direc TdoRelativo []))) st = 
		ejecutar_comando_show_automatico linea maximo st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf
	

	-- DEFAULT
	ejecutar_comando_show_con_dir direc st = 
		ejecutar_comando_show_automatico 0 0 st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf
	

	-- *********************************************************************************************** --
	ejecutar_comando_delete_con_dir :: Maybe Comando -> State -> (String, State)
	ejecutar_comando_delete_con_dir (Just (CDelete (Direc Ultima off))) st =
		borrar_linea (maximo + offset) st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf
			offset = foldr (+) 0 off
	ejecutar_comando_delete_con_dir (Just (CDelete (Direc Corriente off))) st = 
		borrar_linea (linea + offset) st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf
			offset = foldr (+) 0 off
	ejecutar_comando_delete_con_dir (Just (CDelete (Direc (Abs a) off))) st =
		borrar_linea (a + offset) st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf
			offset = foldr (+) 0 off
	ejecutar_comando_delete_con_dir (Just (CDelete (Direc (Rel a) off))) st =
		borrar_linea (a + linea + offset) st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf
			offset = foldr (+) 0 off
	ejecutar_comando_delete_con_dir (Just (CDelete (Direc Todo off))) st = 
		ejecutar_comando_delete_automatico 1 maximo st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf
	ejecutar_comando_delete_con_dir (Just (CDelete (Direc TdoRelativo off))) st = 
		ejecutar_comando_delete_automatico linea maximo st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf
	
	
	-- DEFAULT
	ejecutar_comando_delete_con_dir comando st = 
		ejecutar_comando_delete_automatico linea (maximo + 2) st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf
	

	borrar_linea :: Int -> State -> (String, State)
	borrar_linea a st 
		| a <= 0 				= ("?\n", (linea, buf, modo, esta_modificado, 'd', nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2))
		| a > maximo 		= ("?\n", (linea, buf, modo, esta_modificado, 'd', nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2))
		| a == maximo		= ("", (a - 1, borrar_linea_buf a buf, modo, True, 'd', nom_arch, obtener_lineas_papelera a a buf, aux, buffer_insert, auxiliar1, auxiliar2))
		| otherwise			= ("", (a, borrar_linea_buf a buf, modo, True, 'd', nom_arch, obtener_lineas_papelera a a buf, aux, buffer_insert, auxiliar1, auxiliar2))
		where  	
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf

	borrar_linea_buf :: Int -> [String] -> [String]
	borrar_linea_buf linea [] = []
	borrar_linea_buf linea arr = take (linea-1) arr ++ drop linea arr

	-- *********************************************************************************************** --
	ejecutar_comando_join_con_dir :: Maybe Comando -> State -> (String, State)
	ejecutar_comando_join_con_dir (Just (CJoin (Direc Ultima off))) st = ("", (linea, buf, modo, True, 'j', nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2))
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
	ejecutar_comando_join_con_dir (Just (CJoin (Direc Corriente off))) st = ("?\n", st)
	ejecutar_comando_join_con_dir (Just (CJoin (Direc (Abs a) off))) st = ("", (linea, buf, modo, True, 'j', nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2))
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
	ejecutar_comando_join_con_dir (Just (CJoin (Direc (Rel a) off))) st = ("", (linea, buf, modo, True, 'j', nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2))
		where 
			(linea, buf, modo, _, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
	ejecutar_comando_join_con_dir (Just (CJoin (Direc Todo off))) st = 
		ejecutar_comando_join_automatico 1 maximo st
			where 
				(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
				maximo = length buf
	ejecutar_comando_join_con_dir (Just (CJoin (Direc TdoRelativo off))) st = ("", (linea, buf, modo, True, 'j', nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2))
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st

	-- *********************************************************************************************** --
	ejecutar_comando_change_con_dir :: Maybe Comando -> State -> (String, State)
	ejecutar_comando_change_con_dir (Just (CChange (Direc Ultima off))) st =
		borrar_linea_change (maximo + offset) st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf
			offset = foldr (+) 0 off
	ejecutar_comando_change_con_dir (Just (CChange (Direc Corriente off))) st = 
		borrar_linea_change (linea + offset) st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf
			offset = foldr (+) 0 off
	ejecutar_comando_change_con_dir (Just (CChange (Direc (Abs a) off))) st =
		borrar_linea_change (a + offset) st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf
			offset = foldr (+) 0 off
	ejecutar_comando_change_con_dir (Just (CChange (Direc (Rel a) off))) st =
		borrar_linea_change (a + linea + offset) st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf
			offset = foldr (+) 0 off
	ejecutar_comando_change_con_dir (Just (CChange (Direc Todo []))) st =
		ejecutar_comando_change_automatico 1 maximo st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf
	ejecutar_comando_change_con_dir (Just (CChange (Direc TdoRelativo []))) st =
		ejecutar_comando_change_automatico linea maximo st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf


	-- DEFAULT
	ejecutar_comando_change_con_dir com st =
		ejecutar_comando_change_automatico linea maximo st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf


	borrar_linea_change :: Int -> State -> (String, State)
	borrar_linea_change a st 
		| a < 0 				= ("?\n", (linea, buf, modo, esta_modificado, 'c', nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2))
		| a > maximo 		= ("?\n", (linea, buf, modo, esta_modificado, 'c', nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2))
		| a == 0			 	= ("", (1, borrar_linea_change_buf 1 buf, ModoInsertar, True, 'c', nom_arch, obtener_lineas_papelera 1 1 buf, aux, buffer_insert, auxiliar1, auxiliar2))
		| otherwise			= ("", (a, borrar_linea_change_buf a buf, ModoInsertar, True, 'c', nom_arch, obtener_lineas_papelera a a buf, aux, buffer_insert, auxiliar1, auxiliar2))
		where  	
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf

	borrar_linea_change_buf :: Int -> [String] -> [String]
	borrar_linea_change_buf linea [] = []
	borrar_linea_change_buf linea arr = take (linea-1) arr ++ drop linea arr


	-- *********************************************************************************************** --
	ejecutar_comando_yank_con_dir :: Maybe Comando -> State -> (String, State)
	ejecutar_comando_yank_con_dir (Just (CYank (Direc Ultima off))) st = 
		ejecutar_comando_yank_automatico (maximo + offset) (maximo + offset) st
		where 
			(linea, buf, _, _, _, _, _, _, _, _, _) = st
			maximo = length buf
			offset = foldr (+) 0 off 
	ejecutar_comando_yank_con_dir (Just (CYank (Direc Corriente off))) st =
		ejecutar_comando_yank_automatico (linea + offset) (linea + offset) st
		where 
			(linea, buf, _, _, _, _, _, _, _, _, _) = st
			maximo = length buf
			offset = foldr (+) 0 off 
	ejecutar_comando_yank_con_dir (Just (CYank (Direc (Abs a) off))) st =
		ejecutar_comando_yank_automatico (a + offset) (a + offset) st
		where 
			(linea, buf, _, _, _, _, _, _, _, _, _) = st
			maximo = length buf
			offset = foldr (+) 0 off 
	ejecutar_comando_yank_con_dir (Just (CYank (Direc (Rel a) off))) st =
		ejecutar_comando_yank_automatico (a + linea + offset) (a + linea + offset) st
		where 
			(linea, buf, _, _, _, _, _, _, _, _, _) = st
			maximo = length buf
			offset = foldr (+) 0 off 
	ejecutar_comando_yank_con_dir (Just (CYank (Direc Todo []))) st = 
		ejecutar_comando_yank_automatico 1 maximo st
		where 
			(linea, buf, _, _, _, _, _, _, _, _, _) = st
			maximo = length buf
	ejecutar_comando_yank_con_dir (Just (CYank (Direc TdoRelativo []))) st = 
		ejecutar_comando_yank_automatico 1 maximo st
		where 
			(linea, buf, _, _, _, _, _, _, _, _, _) = st
			maximo = length buf


	--DEFAULT
	ejecutar_comando_yank_con_dir com st = 
		ejecutar_comando_yank_automatico 1 (maximo + 2) st
		where 
			(linea, buf, _, _, _, _, _, _, _, _, _) = st
			maximo = length buf


	-- *********************************************************************************************** --
	ejecutar_comando_paste_con_dir :: Maybe Comando -> State -> (String, State)
	ejecutar_comando_paste_con_dir (Just (CPaste (Direc Ultima off))) st = 
		ejecutar_comando_paste_automatico (maximo + offset) st
		where 
			(linea, buf, _, _, _, _, _, _, _, _, _) = st
			maximo = length buf
			offset = foldr (+) 0 off 
	ejecutar_comando_paste_con_dir (Just (CPaste (Direc Corriente off))) st = 
		ejecutar_comando_paste_automatico (linea + offset) st
		where 
			(linea, buf, _, _, _, _, _, _, _, _, _) = st
			maximo = length buf
			offset = foldr (+) 0 off 
	ejecutar_comando_paste_con_dir (Just (CPaste (Direc (Abs a) off))) st = 
		ejecutar_comando_paste_automatico (a + offset) st
		where 
			(_, buf, _, _, _, _, _, _, _, _, _) = st
			maximo = length buf
			offset = foldr (+) 0 off 
	ejecutar_comando_paste_con_dir (Just (CPaste (Direc (Rel a) off))) st = 
		ejecutar_comando_paste_automatico (a + linea + offset) st
		where 
			(linea, buf, _, _, _, _, _, _, _, _, _) = st
			maximo = length buf
			offset = foldr (+) 0 off 
	ejecutar_comando_paste_con_dir (Just (CPaste (Direc Todo []))) st = 
		ejecutar_comando_paste_automatico maximo st
		where 
			(linea, buf, _, _, _, _, _, _, _, _, _) = st
			maximo = length buf
	ejecutar_comando_paste_con_dir (Just (CPaste (Direc TdoRelativo []))) st = 
		ejecutar_comando_paste_automatico maximo st
		where 
			(linea, buf, _, _, _, _, _, _, _, _, _) = st
			maximo = length buf


	-- DEFAULT
	ejecutar_comando_paste_con_dir com st = ("?\n", (linea, buf, modo, esta_modificado, 'x', nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2))
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st


	ejecutar_comando_paste_automatico :: Int -> State -> (String, State)
	ejecutar_comando_paste_automatico indice (linea, buf, modo, esta_modificado, ult_com, nom_arch, [], aux, buffer_insert, auxiliar1, auxiliar2) = ("?\n", (linea, buf, modo, esta_modificado, ult_com, nom_arch, [], aux, buffer_insert, auxiliar1, auxiliar2))
	ejecutar_comando_paste_automatico indice st 
		| indice > maximo 								= ("?\n", (linea, buf, modo, esta_modificado, 'x', nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2))
		| indice < 0 											= ("?\n", (linea, buf, modo, esta_modificado, 'x', nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2))
		|	otherwise 											= ("", (indice + length papelera, paste papelera indice buf, modo, True, 'x', nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2))
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf


	-- *********************************************************************************************** --
	ejecutar_comando_move_con_dir (Just (CMove (Direc Ultima off) dir3)) st = 
		ejecutar_comando_move_automatico (maximo + offset) (maximo + offset) st dir3
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf
			offset = foldr (+) 0 off
	ejecutar_comando_move_con_dir (Just (CMove (Direc Corriente off) dir3)) st = 
		ejecutar_comando_move_automatico (linea + offset) (linea + offset) st dir3
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf
			offset = foldr (+) 0 off
	ejecutar_comando_move_con_dir (Just (CMove (Direc (Abs a) off) dir3)) st =
		ejecutar_comando_move_automatico (a + offset) (a + offset) st dir3
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf
			offset = foldr (+) 0 off
	ejecutar_comando_move_con_dir (Just (CMove (Direc (Rel a) off) dir3)) st =
		ejecutar_comando_move_automatico (a + linea + offset) (a + linea + offset) st dir3
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf
			offset = foldr (+) 0 off
	ejecutar_comando_move_con_dir (Just (CMove (Direc Todo []) dir3)) st =
		ejecutar_comando_move_automatico 1 maximo st dir3
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf
	ejecutar_comando_move_con_dir (Just (CMove (Direc TdoRelativo []) dir3)) st =
		ejecutar_comando_move_automatico linea maximo st dir3
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf


	--DEFAULT
	ejecutar_comando_move_con_dir com st =
		ejecutar_comando_move_automatico (maximo + 2) (maximo + 2) st (Direc (Abs 2) []) 
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf


    -- *********************************************************************************************** --
	ejecutar_comando_transfer_con_dir :: Maybe Comando -> State -> (String, State)
	ejecutar_comando_transfer_con_dir (Just (CTransfer (Direc Ultima off) dir3)) st =
		ejecutar_comando_transfer_automatico (maximo + offset) (maximo + offset) st dir3
		where
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf
			offset = foldr (+) 0 off
	ejecutar_comando_transfer_con_dir (Just (CTransfer (Direc Corriente off) dir3)) st =
	    ejecutar_comando_transfer_automatico (linea + offset) (linea + offset) st dir3
	    where
	        (linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
	        maximo = length buf
	        offset = foldr (+) 0 off
	ejecutar_comando_transfer_con_dir (Just (CTransfer (Direc (Abs a) off) dir3)) st =
	    ejecutar_comando_transfer_automatico (a + offset) (a + offset) st dir3
	    where
	        (linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
	        maximo = length buf
	        offset = foldr (+) 0 off
	ejecutar_comando_transfer_con_dir (Just (CTransfer (Direc (Rel a) off) dir3)) st =
	    ejecutar_comando_transfer_automatico (a + linea + offset) (a + linea + offset) st dir3
	    where
	        (linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
	        maximo = length buf
	        offset = foldr (+) 0 off
	ejecutar_comando_transfer_con_dir (Just (CTransfer (Direc Todo []) dir3)) st =
	    ejecutar_comando_transfer_automatico 1 maximo st dir3
	    where
	        (linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
	        maximo = length buf
	ejecutar_comando_transfer_con_dir (Just (CTransfer (Direc TdoRelativo []) dir3)) st =
	    ejecutar_comando_transfer_automatico linea maximo st dir3
	    where
	        (linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
	        maximo = length buf


    -- Default
	ejecutar_comando_transfer_con_dir com st =
		ejecutar_comando_transfer_automatico (maximo + 2) (maximo + 2) st (Direc (Abs 0) [])
			where
				(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
				maximo = length buf


	-- *** *** *** *** *** *** --
	-- Ejecucion de comandos con dos direcciones
	-- *** *** *** *** *** *** --
	-- *********************************************************************************************** --
	ejecutar_comando_show_con_dos_dir :: Maybe Comando -> State -> (String, State)
	ejecutar_comando_show_con_dos_dir (Just (CShowT (Direc Ultima off1) (Direc Ultima off2))) st = 
		ejecutar_comando_show_automatico (maximo + offset1) (maximo + offset2) st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf
			offset1 = foldr (+) 0 off1
			offset2 = foldr (+) 0 off2
	ejecutar_comando_show_con_dos_dir (Just (CShowT (Direc Corriente off1) (Direc Corriente off2))) st = 
		ejecutar_comando_show_automatico (linea + offset1) (linea + offset2) st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf
			offset1 = foldr (+) 0 off1
			offset2 = foldr (+) 0 off2
	ejecutar_comando_show_con_dos_dir (Just (CShowT (Direc (Abs a) off1) (Direc (Abs b) off2))) st = 
		ejecutar_comando_show_automatico (a + offset1) (b + offset2) st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf
			offset1 = foldr (+) 0 off1
			offset2 = foldr (+) 0 off2
	ejecutar_comando_show_con_dos_dir (Just (CShowT (Direc (Rel a) off1) (Direc (Rel b) off2))) st = 
		ejecutar_comando_show_automatico (a + linea + offset1) (linea + b + offset2) st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf
			offset1 = foldr (+) 0 off1
			offset2 = foldr (+) 0 off2
	ejecutar_comando_show_con_dos_dir (Just (CShowT (Direc Corriente off1) (Direc Ultima off2))) st = 
		ejecutar_comando_show_automatico (linea + offset1) (maximo + offset2) st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf
			offset1 = foldr (+) 0 off1
			offset2 = foldr (+) 0 off2
	ejecutar_comando_show_con_dos_dir (Just (CShowT (Direc Ultima off1) (Direc Corriente off2))) st = 
		ejecutar_comando_show_automatico (maximo + offset1) (linea + offset2) st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf
			offset1 = foldr (+) 0 off1
			offset2 = foldr (+) 0 off2
	ejecutar_comando_show_con_dos_dir (Just (CShowT (Direc (Abs a) off1) (Direc (Rel b) off2))) st = 
		ejecutar_comando_show_automatico (a + offset1) (linea + b + offset2) st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf
			offset1 = foldr (+) 0 off1
			offset2 = foldr (+) 0 off2
	ejecutar_comando_show_con_dos_dir (Just (CShowT (Direc (Rel a) off1) (Direc (Abs b) off2))) st = 
		ejecutar_comando_show_automatico (a + linea + offset1) (b + offset2) st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf
			offset1 = foldr (+) 0 off1
			offset2 = foldr (+) 0 off2
	ejecutar_comando_show_con_dos_dir (Just (CShowT (Direc Ultima off1) (Direc (Rel b) off2))) st = 
		ejecutar_comando_show_automatico (maximo + offset1) (linea + b + offset2) st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf
			offset1 = foldr (+) 0 off1
			offset2 = foldr (+) 0 off2
	ejecutar_comando_show_con_dos_dir (Just (CShowT (Direc (Rel a) off1) (Direc Ultima off2))) st = 
		ejecutar_comando_show_automatico (a + linea + offset1) (maximo + offset2) st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf
			offset1 = foldr (+) 0 off1
			offset2 = foldr (+) 0 off2
	ejecutar_comando_show_con_dos_dir (Just (CShowT (Direc Ultima off1) (Direc (Abs b) off2))) st = 
		ejecutar_comando_show_automatico (maximo + offset1) (b + offset2) st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf
			offset1 = foldr (+) 0 off1
			offset2 = foldr (+) 0 off2
	ejecutar_comando_show_con_dos_dir (Just (CShowT (Direc (Abs a) off1) (Direc Ultima off2))) st = 
		ejecutar_comando_show_automatico (a + offset1) (maximo + offset2) st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf
			offset1 = foldr (+) 0 off1
			offset2 = foldr (+) 0 off2
	ejecutar_comando_show_con_dos_dir (Just (CShowT (Direc Corriente off1) (Direc (Rel b) off2))) st = 
		ejecutar_comando_show_automatico (linea + offset1) (linea + b + offset2) st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf
			offset1 = foldr (+) 0 off1
			offset2 = foldr (+) 0 off2
	ejecutar_comando_show_con_dos_dir (Just (CShowT (Direc (Rel a) off1) (Direc Corriente off2))) st = 
		ejecutar_comando_show_automatico (a + linea + offset1) (linea + offset2) st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf
			offset1 = foldr (+) 0 off1
			offset2 = foldr (+) 0 off2
	ejecutar_comando_show_con_dos_dir (Just (CShowT (Direc Corriente off1) (Direc (Abs b) off2))) st = 
		ejecutar_comando_show_automatico (linea + offset1) (b + offset2) st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf
			offset1 = foldr (+) 0 off1
			offset2 = foldr (+) 0 off2
	ejecutar_comando_show_con_dos_dir (Just (CShowT (Direc (Abs a) off1) (Direc Corriente off2))) st = 
		ejecutar_comando_show_automatico (a + offset1) (linea + offset2) st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf
			offset1 = foldr (+) 0 off1
			offset2 = foldr (+) 0 off2
	

	-- DEFAULT
	ejecutar_comando_show_con_dos_dir com st = 
		ejecutar_comando_show_automatico 3 2 st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf
		

	ejecutar_comando_show_automatico :: Int -> Int -> State -> (String, State)
	ejecutar_comando_show_automatico indice1 indice2 st
		| indice1 > maximo				= ("?\n", (linea, buf, modo, esta_modificado, 'n', nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2))	
		| indice2 > maximo				= ("?\n", (linea, buf, modo, esta_modificado, 'n', nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2))	
		| indice1 <= 0						= ("?\n", (linea, buf, modo, esta_modificado, 'n', nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2))	
		|	indice2 <= 0						= ("?\n", (linea, buf, modo, esta_modificado, 'n', nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2))	
		| indice1 > indice2 			= ("?\n", (linea, buf, modo, esta_modificado, 'n', nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2))	
		| otherwise 							= (obtener_lineas_con_tabulador_e_indice indice1 indice2 buf, (indice2, buf, modo, esta_modificado, 'n', nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2))
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf


	-- *********************************************************************************************** --
	ejecutar_comando_print_con_dos_dir :: Maybe Comando -> State -> (String, State)
	ejecutar_comando_print_con_dos_dir (Just (CPrintT (Direc Ultima off1) (Direc Ultima off2))) st =
		ejecutar_comando_print_automatico (maximo + offset1) (maximo + offset2) st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf
			offset1 = foldr (+) 0 off1
			offset2 = foldr (+) 0 off2
	ejecutar_comando_print_con_dos_dir (Just (CPrintT (Direc Corriente off1) (Direc Corriente  off2))) st = 
		ejecutar_comando_print_automatico (linea + offset1) (linea + offset2) st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf
			offset1 = foldr (+) 0 off1
			offset2 = foldr (+) 0 off2
	ejecutar_comando_print_con_dos_dir (Just (CPrintT (Direc (Abs a) off1) (Direc (Abs b) off2))) st =
		ejecutar_comando_print_automatico (a + offset1) (b + offset2) st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf
			offset1 = foldr (+) 0 off1
			offset2 = foldr (+) 0 off2
	ejecutar_comando_print_con_dos_dir (Just (CPrintT (Direc (Rel a) off1) (Direc (Rel b) off2))) st =
		ejecutar_comando_print_automatico (a + linea + offset1) (linea + b + offset2) st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf
			offset1 = foldr (+) 0 off1
			offset2 = foldr (+) 0 off2
	ejecutar_comando_print_con_dos_dir (Just (CPrintT (Direc (Abs a) off1) (Direc (Rel b) off2))) st =
		ejecutar_comando_print_automatico (a + offset1) (b + linea +offset2) st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf
			offset1 = foldr (+) 0 off1
			offset2 = foldr (+) 0 off2
	ejecutar_comando_print_con_dos_dir (Just (CPrintT (Direc (Rel a) off1) (Direc (Abs b) off2))) st =
		ejecutar_comando_print_automatico (a + linea + offset1) (b + offset2) st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf
			offset1 = foldr (+) 0 off1
			offset2 = foldr (+) 0 off2
	ejecutar_comando_print_con_dos_dir (Just (CPrintT (Direc Ultima off1) (Direc Corriente off2))) st = 
		ejecutar_comando_print_automatico (maximo + offset1) (linea + offset2) st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf
			offset1 = foldr (+) 0 off1
			offset2 = foldr (+) 0 off2
	ejecutar_comando_print_con_dos_dir (Just (CPrintT (Direc Corriente off1) (Direc Ultima off2))) st = 
		ejecutar_comando_print_automatico (linea + offset1) (maximo + offset2) st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf
			offset1 = foldr (+) 0 off1
			offset2 = foldr (+) 0 off2
	ejecutar_comando_print_con_dos_dir (Just (CPrintT (Direc Corriente off1) (Direc (Abs b) off2))) st = 
		ejecutar_comando_print_automatico (linea + offset1) (b + offset2) st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf
			offset1 = foldr (+) 0 off1
			offset2 = foldr (+) 0 off2
	ejecutar_comando_print_con_dos_dir (Just (CPrintT (Direc Corriente off1) (Direc (Rel b) off2))) st = 
		ejecutar_comando_print_automatico (linea + offset1) (linea + b + offset2) st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf
			offset1 = foldr (+) 0 off1
			offset2 = foldr (+) 0 off2
	ejecutar_comando_print_con_dos_dir (Just (CPrintT (Direc Ultima off1) (Direc (Abs b) off2))) st = 
		ejecutar_comando_print_automatico (maximo + offset1) (b + offset2) st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf
			offset1 = foldr (+) 0 off1
			offset2 = foldr (+) 0 off2
	ejecutar_comando_print_con_dos_dir (Just (CPrintT (Direc Ultima off1) (Direc (Rel b) off2))) st = 
		ejecutar_comando_print_automatico (maximo + offset1) (linea + b + offset2) st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf
			offset1 = foldr (+) 0 off1
			offset2 = foldr (+) 0 off2
	ejecutar_comando_print_con_dos_dir (Just (CPrintT (Direc (Rel a) off1) (Direc Ultima off2))) st = 
		ejecutar_comando_print_automatico (a + linea + offset1) (maximo + offset2) st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf
			offset1 = foldr (+) 0 off1
			offset2 = foldr (+) 0 off2
	ejecutar_comando_print_con_dos_dir (Just (CPrintT (Direc (Rel a) off1) (Direc Corriente off2))) st = 
		ejecutar_comando_print_automatico (a + linea + offset1) (linea + offset2) st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf
			offset1 = foldr (+) 0 off1
			offset2 = foldr (+) 0 off2
	ejecutar_comando_print_con_dos_dir (Just (CPrintT (Direc (Abs a) off1) (Direc Ultima off2))) st = 
		ejecutar_comando_print_automatico (a + offset1) (maximo + offset2) st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf
			offset1 = foldr (+) 0 off1
			offset2 = foldr (+) 0 off2
	ejecutar_comando_print_con_dos_dir (Just (CPrintT (Direc (Abs a) off1) (Direc Corriente off2))) st = 
		ejecutar_comando_print_automatico (a + offset1) (linea + offset2) st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf
			offset1 = foldr (+) 0 off1
			offset2 = foldr (+) 0 off2
	

	-- DEFAULT
	ejecutar_comando_print_con_dos_dir com st = 
		ejecutar_comando_print_automatico 3 2 st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf


	ejecutar_comando_print_automatico :: Int -> Int -> State -> (String, State)
	ejecutar_comando_print_automatico indice1 indice2 st
		| indice1 > maximo				= ("?\n", (linea, buf, modo, esta_modificado, 'p', nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2))	
		| indice2 > maximo				= ("?\n", (linea, buf, modo, esta_modificado, 'p', nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2))	
		| indice1 <= 0						= ("?\n", (linea, buf, modo, esta_modificado, 'p', nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2))	
		|	indice2 <= 0						= ("?\n", (linea, buf, modo, esta_modificado, 'p', nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2))	
		| indice1 > indice2 			= ("?\n", (linea, buf, modo, esta_modificado, 'p', nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2))	
		| otherwise 							= (obtener_lineas indice1 indice2 buf, (indice2, buf, modo, esta_modificado, 'p', nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2))
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf


	-- *********************************************************************************************** --
	ejecutar_comando_delete_con_dos_dir :: Maybe Comando -> State -> (String, State)
	ejecutar_comando_delete_con_dos_dir (Just (CDeleteT (Direc Ultima off1) (Direc Ultima off2))) st = 
		ejecutar_comando_delete_automatico (maximo + offset1) (maximo + offset2) st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf
			offset1 = foldr (+) 0 off1
			offset2 = foldr (+) 0 off2
	ejecutar_comando_delete_con_dos_dir (Just (CDeleteT (Direc Corriente off1) (Direc Corriente off2))) st = 
		ejecutar_comando_delete_automatico (linea + offset1) (linea + offset2) st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf
			offset1 = foldr (+) 0 off1
			offset2 = foldr (+) 0 off2
	ejecutar_comando_delete_con_dos_dir (Just (CDeleteT (Direc (Abs a) off1) (Direc (Abs b) off2))) st = 
		ejecutar_comando_delete_automatico (a + offset1) (b + offset2) st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf
			offset1 = foldr (+) 0 off1
			offset2 = foldr (+) 0 off2
	ejecutar_comando_delete_con_dos_dir (Just (CDeleteT (Direc (Rel a) off1) (Direc (Rel b) off2))) st = 
		ejecutar_comando_delete_automatico (a + linea + offset1) (linea + b + offset2) st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf
			offset1 = foldr (+) 0 off1
			offset2 = foldr (+) 0 off2
	ejecutar_comando_delete_con_dos_dir (Just (CDeleteT (Direc Corriente off1) (Direc Ultima off2))) st = 
		ejecutar_comando_delete_automatico (linea + offset1) (maximo + offset2) st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf
			offset1 = foldr (+) 0 off1
			offset2 = foldr (+) 0 off2
	ejecutar_comando_delete_con_dos_dir (Just (CDeleteT (Direc Ultima off1) (Direc Corriente off2))) st = 
		ejecutar_comando_delete_automatico (maximo + offset1) (linea + offset2) st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf
			offset1 = foldr (+) 0 off1
			offset2 = foldr (+) 0 off2
	ejecutar_comando_delete_con_dos_dir (Just (CDeleteT (Direc (Abs a) off1) (Direc (Rel b) off2))) st = 
		ejecutar_comando_delete_automatico (a + offset1) (linea + b + offset2) st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf
			offset1 = foldr (+) 0 off1
			offset2 = foldr (+) 0 off2
	ejecutar_comando_delete_con_dos_dir (Just (CDeleteT (Direc (Rel a) off1) (Direc (Abs b) off2))) st = 
		ejecutar_comando_delete_automatico (a + linea + offset1) (b + offset2) st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf
			offset1 = foldr (+) 0 off1
			offset2 = foldr (+) 0 off2
	ejecutar_comando_delete_con_dos_dir (Just (CDeleteT (Direc Ultima off1) (Direc (Rel b) off2))) st = 
		ejecutar_comando_delete_automatico (maximo + offset1) (linea + b + offset2) st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf
			offset1 = foldr (+) 0 off1
			offset2 = foldr (+) 0 off2
	ejecutar_comando_delete_con_dos_dir (Just (CDeleteT (Direc (Rel a) off1) (Direc Ultima off2))) st = 
		ejecutar_comando_delete_automatico (a + linea + offset1) (maximo + offset2) st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf
			offset1 = foldr (+) 0 off1
			offset2 = foldr (+) 0 off2
	ejecutar_comando_delete_con_dos_dir (Just (CDeleteT (Direc Ultima off1) (Direc (Abs b) off2))) st = 
		ejecutar_comando_delete_automatico (maximo + offset1) (b + offset2) st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf
			offset1 = foldr (+) 0 off1
			offset2 = foldr (+) 0 off2
	ejecutar_comando_delete_con_dos_dir (Just (CDeleteT (Direc (Abs a) off1) (Direc Ultima off2))) st = 
		ejecutar_comando_delete_automatico (a + offset1) (maximo + offset2) st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf
			offset1 = foldr (+) 0 off1
			offset2 = foldr (+) 0 off2
	ejecutar_comando_delete_con_dos_dir (Just (CDeleteT (Direc Corriente off1) (Direc (Rel b) off2))) st = 
		ejecutar_comando_delete_automatico (linea + offset1) (linea + b + offset2) st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf
			offset1 = foldr (+) 0 off1
			offset2 = foldr (+) 0 off2
	ejecutar_comando_delete_con_dos_dir (Just (CDeleteT (Direc (Rel a) off1) (Direc Corriente off2))) st = 
		ejecutar_comando_delete_automatico (a + linea + offset1) (linea + offset2) st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf
			offset1 = foldr (+) 0 off1
			offset2 = foldr (+) 0 off2
	ejecutar_comando_delete_con_dos_dir (Just (CDeleteT (Direc Corriente off1) (Direc (Abs b) off2))) st = 
		ejecutar_comando_delete_automatico (linea + offset1) (b + offset2) st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf
			offset1 = foldr (+) 0 off1
			offset2 = foldr (+) 0 off2
	ejecutar_comando_delete_con_dos_dir (Just (CDeleteT (Direc (Abs a) off1) (Direc Corriente off2))) st = 
		ejecutar_comando_delete_automatico (a + offset1) (linea + offset2) st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf
			offset1 = foldr (+) 0 off1
			offset2 = foldr (+) 0 off2


	-- DEFAULT
	ejecutar_comando_delete_con_dos_dir com st = 
		ejecutar_comando_delete_automatico (maximo + 2) maximo st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf


	ejecutar_comando_delete_automatico :: Int -> Int -> State -> (String, State)
	ejecutar_comando_delete_automatico indice1 indice2 st
		| indice1 > maximo				= ("?\n", (linea, buf, modo, esta_modificado, 'd', nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2))	
		| indice2 > maximo				= ("?\n", (linea, buf, modo, esta_modificado, 'd', nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2))	
		| indice1 <= 0						= ("?\n", (linea, buf, modo, esta_modificado, 'd', nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2))	
		|	indice2 <= 0						= ("?\n", (linea, buf, modo, esta_modificado, 'd', nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2))	
		| indice1 > indice2 			= ("?\n", (linea, buf, modo, esta_modificado, 'd', nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2))	
		| indice2 == maximo 			= ("", ((maximo - (indice2 - indice1)) - 1, borrar_lineas indice1 indice2 buf, modo, True, 'd', nom_arch, obtener_lineas_papelera indice1 indice2 buf, aux, buffer_insert, auxiliar1, auxiliar2))
		| otherwise 							= ("", (indice1, borrar_lineas indice1 indice2 buf, modo, True, 'd', nom_arch, obtener_lineas_papelera indice1 indice2 buf, aux, buffer_insert, auxiliar1, auxiliar2))
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf


	-- *********************************************************************************************** --
	ejecutar_comando_write_con_dos_dir :: Maybe Comando -> State -> (String, State)
	ejecutar_comando_write_con_dos_dir (Just (CWriteT (Direc Ultima off1) (Direc Ultima off2))) st = 
		ejecutar_comando_write_automatico (maximo + offset1) (maximo + offset2) st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf
			offset1 = foldr (+) 0 off1
			offset2 = foldr (+) 0 off2
	ejecutar_comando_write_con_dos_dir (Just (CWriteT (Direc Corriente off1) (Direc Corriente off2))) st = 
		ejecutar_comando_write_automatico (linea + offset1) (linea + offset2) st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf
			offset1 = foldr (+) 0 off1
			offset2 = foldr (+) 0 off2
	ejecutar_comando_write_con_dos_dir (Just (CWriteT (Direc (Abs a) off1) (Direc (Abs b) off2))) st = 
		ejecutar_comando_write_automatico (a + offset1) (b + offset2) st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf
			offset1 = foldr (+) 0 off1
			offset2 = foldr (+) 0 off2
	ejecutar_comando_write_con_dos_dir (Just (CWriteT (Direc (Rel a) off1) (Direc (Rel b) off2))) st = 
		ejecutar_comando_write_automatico (a + linea + offset1) (linea + b + offset2) st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf
			offset1 = foldr (+) 0 off1
			offset2 = foldr (+) 0 off2
	ejecutar_comando_write_con_dos_dir (Just (CWriteT (Direc Corriente off1) (Direc Ultima off2))) st = 
		ejecutar_comando_write_automatico (linea + offset1) (maximo + offset2) st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf
			offset1 = foldr (+) 0 off1
			offset2 = foldr (+) 0 off2
	ejecutar_comando_write_con_dos_dir (Just (CWriteT (Direc Ultima off1) (Direc Corriente off2))) st = 
		ejecutar_comando_write_automatico (maximo + offset1) (linea + offset2) st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf
			offset1 = foldr (+) 0 off1
			offset2 = foldr (+) 0 off2
	ejecutar_comando_write_con_dos_dir (Just (CWriteT (Direc (Abs a) off1) (Direc (Rel b) off2))) st = 
		ejecutar_comando_write_automatico (a + offset1) (linea + b + offset2) st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf
			offset1 = foldr (+) 0 off1
			offset2 = foldr (+) 0 off2
	ejecutar_comando_write_con_dos_dir (Just (CWriteT (Direc (Rel a) off1) (Direc (Abs b) off2))) st = 
		ejecutar_comando_write_automatico (a + linea + offset1) (b + offset2) st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf
			offset1 = foldr (+) 0 off1
			offset2 = foldr (+) 0 off2
	ejecutar_comando_write_con_dos_dir (Just (CWriteT (Direc Ultima off1) (Direc (Rel b) off2))) st = 
		ejecutar_comando_write_automatico (maximo + offset1) (linea + b + offset2) st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf
			offset1 = foldr (+) 0 off1
			offset2 = foldr (+) 0 off2
	ejecutar_comando_write_con_dos_dir (Just (CWriteT (Direc (Rel a) off1) (Direc Ultima off2))) st = 
		ejecutar_comando_write_automatico (a + linea + offset1) (maximo + offset2) st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf
			offset1 = foldr (+) 0 off1
			offset2 = foldr (+) 0 off2
	ejecutar_comando_write_con_dos_dir (Just (CWriteT (Direc Ultima off1) (Direc (Abs b) off2))) st = 
		ejecutar_comando_write_automatico (maximo + offset1) (b + offset2) st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf
			offset1 = foldr (+) 0 off1
			offset2 = foldr (+) 0 off2
	ejecutar_comando_write_con_dos_dir (Just (CWriteT (Direc (Abs a) off1) (Direc Ultima off2))) st = 
		ejecutar_comando_write_automatico (a + offset1) (maximo + offset2) st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf
			offset1 = foldr (+) 0 off1
			offset2 = foldr (+) 0 off2
	ejecutar_comando_write_con_dos_dir (Just (CWriteT (Direc Corriente off1) (Direc (Rel b) off2))) st = 
		ejecutar_comando_write_automatico (linea + offset1) (linea + b + offset2) st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf
			offset1 = foldr (+) 0 off1
			offset2 = foldr (+) 0 off2
	ejecutar_comando_write_con_dos_dir (Just (CWriteT (Direc (Rel a) off1) (Direc Corriente off2))) st = 
		ejecutar_comando_write_automatico (a + linea + offset1) (linea + offset2) st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf
			offset1 = foldr (+) 0 off1
			offset2 = foldr (+) 0 off2
	ejecutar_comando_write_con_dos_dir (Just (CWriteT (Direc Corriente off1) (Direc (Abs b) off2))) st = 
		ejecutar_comando_write_automatico (linea + offset1) (b + offset2) st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf
			offset1 = foldr (+) 0 off1
			offset2 = foldr (+) 0 off2
	ejecutar_comando_write_con_dos_dir (Just (CWriteT (Direc (Abs a) off1) (Direc Corriente off2))) st = 
		ejecutar_comando_write_automatico (a + offset1) (linea + offset2) st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf
			offset1 = foldr (+) 0 off1
			offset2 = foldr (+) 0 off2


	-- DEFAULT
	ejecutar_comando_write_con_dos_dir com st = 
		ejecutar_comando_write_automatico maximo (maximo + 2) st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf
	

	ejecutar_comando_write_automatico :: Int -> Int -> State -> (String, State)
	ejecutar_comando_write_automatico indice1 indice2 st
		| indice1 > maximo		= ("?\n", (linea, buf, modo, esta_modificado, 'l', nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2))	
		| indice2 > maximo		= ("?\n", (linea, buf, modo, esta_modificado, 'l', nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2))	
		| indice1 <= 0			= ("?\n", (linea, buf, modo, esta_modificado, 'l', nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2))	
		| indice2 <= 0			= ("?\n", (linea, buf, modo, esta_modificado, 'l', nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2))	
		| indice1 > indice2 	= ("?\n", (linea, buf, modo, esta_modificado, 'l', nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2))	
		| otherwise 			= ("", (indice1, buf, modo, esta_modificado, 'w', nom_arch, papelera, aux, buffer_insert, write_lineas indice1 indice2 buf, auxiliar2))
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf

	write_lineas :: Int -> Int -> [a] -> [a]
	write_lineas indice1 indice2 buf = drop (indice1-1) (take indice2 buf)

	ejecutar_comando_change_con_dos_dir :: Maybe Comando -> State -> (String, State)
	ejecutar_comando_change_con_dos_dir (Just (CChangeT (Direc Ultima off1) (Direc Ultima off2))) st = 
		ejecutar_comando_change_automatico (maximo + offset1) (maximo + offset2) st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf
			offset1 = foldr (+) 0 off1
			offset2 = foldr (+) 0 off2
	ejecutar_comando_change_con_dos_dir (Just (CChangeT (Direc Corriente off1) (Direc Corriente off2))) st = 
		ejecutar_comando_change_automatico (linea + offset1) (linea + offset2) st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf
			offset1 = foldr (+) 0 off1
			offset2 = foldr (+) 0 off2
	ejecutar_comando_change_con_dos_dir (Just (CChangeT (Direc (Abs a) off1) (Direc (Abs b) off2))) st = 
		ejecutar_comando_change_automatico (a + offset1) (b + offset2) st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf
			offset1 = foldr (+) 0 off1
			offset2 = foldr (+) 0 off2
	ejecutar_comando_change_con_dos_dir (Just (CChangeT (Direc (Rel a) off1) (Direc (Rel b) off2))) st = 
		ejecutar_comando_change_automatico (a + linea + offset1) (linea + b + offset2) st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf
			offset1 = foldr (+) 0 off1
			offset2 = foldr (+) 0 off2
	ejecutar_comando_change_con_dos_dir (Just (CChangeT (Direc Corriente off1) (Direc Ultima off2))) st = 
		ejecutar_comando_change_automatico (linea + offset1) (maximo + offset2) st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf
			offset1 = foldr (+) 0 off1
			offset2 = foldr (+) 0 off2
	ejecutar_comando_change_con_dos_dir (Just (CChangeT (Direc Ultima off1) (Direc Corriente off2))) st = 
		ejecutar_comando_change_automatico (maximo + offset1) (linea + offset2) st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf
			offset1 = foldr (+) 0 off1
			offset2 = foldr (+) 0 off2
	ejecutar_comando_change_con_dos_dir (Just (CChangeT (Direc (Abs a) off1) (Direc (Rel b) off2))) st = 
		ejecutar_comando_change_automatico (a + offset1) (linea + b + offset2) st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf
			offset1 = foldr (+) 0 off1
			offset2 = foldr (+) 0 off2
	ejecutar_comando_change_con_dos_dir (Just (CChangeT (Direc (Rel a) off1) (Direc (Abs b) off2))) st = 
		ejecutar_comando_change_automatico (a + linea + offset1) (b + offset2) st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf
			offset1 = foldr (+) 0 off1
			offset2 = foldr (+) 0 off2
	ejecutar_comando_change_con_dos_dir (Just (CChangeT (Direc Ultima off1) (Direc (Rel b) off2))) st = 
		ejecutar_comando_change_automatico (maximo + offset1) (linea + b + offset2) st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf
			offset1 = foldr (+) 0 off1
			offset2 = foldr (+) 0 off2
	ejecutar_comando_change_con_dos_dir (Just (CChangeT (Direc (Rel a) off1) (Direc Ultima off2))) st = 
		ejecutar_comando_change_automatico (a + linea + offset1) (maximo + offset2) st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf
			offset1 = foldr (+) 0 off1
			offset2 = foldr (+) 0 off2
	ejecutar_comando_change_con_dos_dir (Just (CChangeT (Direc Ultima off1) (Direc (Abs b) off2))) st = 
		ejecutar_comando_change_automatico (maximo + offset1) (b + offset2) st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf
			offset1 = foldr (+) 0 off1
			offset2 = foldr (+) 0 off2
	ejecutar_comando_change_con_dos_dir (Just (CChangeT (Direc (Abs a) off1) (Direc Ultima off2))) st = 
		ejecutar_comando_change_automatico (a + offset1) (maximo + offset2) st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf
			offset1 = foldr (+) 0 off1
			offset2 = foldr (+) 0 off2
	ejecutar_comando_change_con_dos_dir (Just (CChangeT (Direc Corriente off1) (Direc (Rel b) off2))) st = 
		ejecutar_comando_change_automatico (linea + offset1) (linea + b + offset2) st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf
			offset1 = foldr (+) 0 off1
			offset2 = foldr (+) 0 off2
	ejecutar_comando_change_con_dos_dir (Just (CChangeT (Direc (Rel a) off1) (Direc Corriente off2))) st = 
		ejecutar_comando_change_automatico (a + linea + offset1) (linea + offset2) st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf
			offset1 = foldr (+) 0 off1
			offset2 = foldr (+) 0 off2
	ejecutar_comando_change_con_dos_dir (Just (CChangeT (Direc Corriente off1) (Direc (Abs b) off2))) st = 
		ejecutar_comando_change_automatico (linea + offset1) (b + offset2) st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf
			offset1 = foldr (+) 0 off1
			offset2 = foldr (+) 0 off2
	ejecutar_comando_change_con_dos_dir (Just (CChangeT (Direc (Abs a) off1) (Direc Corriente off2))) st = 
		ejecutar_comando_change_automatico (a + offset1) (linea + offset2) st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf
			offset1 = foldr (+) 0 off1
			offset2 = foldr (+) 0 off2


	-- DEFAULT
	ejecutar_comando_change_con_dos_dir com st = 
		ejecutar_comando_change_automatico maximo (maximo + 2) st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf
	

	ejecutar_comando_change_automatico :: Int -> Int -> State -> (String, State)
	ejecutar_comando_change_automatico indice1 indice2 st
		| indice1 > maximo						= ("?\n", (linea, buf, modo, esta_modificado, 'c', nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2))	
		| indice2 > maximo						= ("?\n", (linea, buf, modo, esta_modificado, 'c', nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2))	
		| indice1 < 0							= ("?\n", (linea, buf, modo, esta_modificado, 'c', nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2))	
		| indice2 < 0							= ("?\n", (linea, buf, modo, esta_modificado, 'c', nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2))	
		| indice1 > indice2 					= ("?\n", (linea, buf, modo, esta_modificado, 'c', nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2))	
		| indice1 == 0 && indice2 == 0			= ("", (indice1, borrar_lineas 1 1 buf, ModoInsertar, True, 'c', nom_arch, obtener_lineas_papelera 1 1 buf, aux, buffer_insert, auxiliar1, auxiliar2))
		| indice1 == 0							= ("", (1, borrar_lineas 1 indice2 buf, ModoInsertar, True, 'c', nom_arch, obtener_lineas_papelera 1 indice2 buf, aux, buffer_insert, auxiliar1, auxiliar2))
		| otherwise 							= ("", (indice1, borrar_lineas indice1 indice2 buf, ModoInsertar, True, 'c', nom_arch, obtener_lineas_papelera indice1 indice2 buf, aux, buffer_insert, auxiliar1, auxiliar2))
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf

	borrar_lineas :: Int -> Int -> [a] -> [a]
	borrar_lineas indice1 indice2 buf = take (indice1-1) buf ++ drop indice2 buf


	-- *********************************************************************************************** --
	ejecutar_comando_yank_con_dos_dir :: Maybe Comando -> State -> (String, State)
	ejecutar_comando_yank_con_dos_dir (Just (CYankT (Direc Ultima off1) (Direc Ultima off2))) st = 
		ejecutar_comando_yank_automatico (maximo + offset1) (maximo + offset2) st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf
			offset1 = foldr (+) 0 off1
			offset2 = foldr (+) 0 off2
	ejecutar_comando_yank_con_dos_dir (Just (CYankT (Direc Corriente off1) (Direc Corriente off2))) st = 
		ejecutar_comando_yank_automatico (linea + offset1) (linea + offset2) st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf
			offset1 = foldr (+) 0 off1
			offset2 = foldr (+) 0 off2
	ejecutar_comando_yank_con_dos_dir (Just (CYankT (Direc (Abs a) off1) (Direc (Abs b) off2))) st = 
		ejecutar_comando_yank_automatico (a + offset1) (b + offset2) st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf
			offset1 = foldr (+) 0 off1
			offset2 = foldr (+) 0 off2
	ejecutar_comando_yank_con_dos_dir (Just (CYankT (Direc (Rel a) off1) (Direc (Rel b) off2))) st = 
		ejecutar_comando_yank_automatico (a + linea + offset1) (linea + b + offset2) st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf
			offset1 = foldr (+) 0 off1
			offset2 = foldr (+) 0 off2
	ejecutar_comando_yank_con_dos_dir (Just (CYankT (Direc Corriente off1) (Direc Ultima off2))) st = 
		ejecutar_comando_yank_automatico (linea + offset1) (maximo + offset2) st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf
			offset1 = foldr (+) 0 off1
			offset2 = foldr (+) 0 off2
	ejecutar_comando_yank_con_dos_dir (Just (CYankT (Direc Ultima off1) (Direc Corriente off2))) st = 
		ejecutar_comando_yank_automatico (maximo + offset1) (linea + offset2) st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf
			offset1 = foldr (+) 0 off1
			offset2 = foldr (+) 0 off2
	ejecutar_comando_yank_con_dos_dir (Just (CYankT (Direc (Abs a) off1) (Direc (Rel b) off2))) st = 
		ejecutar_comando_yank_automatico (a + offset1) (linea + b + offset2) st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf
			offset1 = foldr (+) 0 off1
			offset2 = foldr (+) 0 off2
	ejecutar_comando_yank_con_dos_dir (Just (CYankT (Direc (Rel a) off1) (Direc (Abs b) off2))) st = 
		ejecutar_comando_yank_automatico (a + linea + offset1) (b + offset2) st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf
			offset1 = foldr (+) 0 off1
			offset2 = foldr (+) 0 off2
	ejecutar_comando_yank_con_dos_dir (Just (CYankT (Direc Ultima off1) (Direc (Rel b) off2))) st = 
		ejecutar_comando_yank_automatico (maximo + offset1) (linea + b + offset2) st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf
			offset1 = foldr (+) 0 off1
			offset2 = foldr (+) 0 off2
	ejecutar_comando_yank_con_dos_dir (Just (CYankT (Direc (Rel a) off1) (Direc Ultima off2))) st = 
		ejecutar_comando_yank_automatico (a + linea + offset1) (maximo + offset2) st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf
			offset1 = foldr (+) 0 off1
			offset2 = foldr (+) 0 off2
	ejecutar_comando_yank_con_dos_dir (Just (CYankT (Direc Ultima off1) (Direc (Abs b) off2))) st = 
		ejecutar_comando_yank_automatico (maximo + offset1) (b + offset2) st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf
			offset1 = foldr (+) 0 off1
			offset2 = foldr (+) 0 off2
	ejecutar_comando_yank_con_dos_dir (Just (CYankT (Direc (Abs a) off1) (Direc Ultima off2))) st = 
		ejecutar_comando_yank_automatico (a + offset1) (maximo + offset2) st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf
			offset1 = foldr (+) 0 off1
			offset2 = foldr (+) 0 off2
	ejecutar_comando_yank_con_dos_dir (Just (CYankT (Direc Corriente off1) (Direc (Rel b) off2))) st = 
		ejecutar_comando_yank_automatico (linea + offset1) (linea + b + offset2) st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf
			offset1 = foldr (+) 0 off1
			offset2 = foldr (+) 0 off2
	ejecutar_comando_yank_con_dos_dir (Just (CYankT (Direc (Rel a) off1) (Direc Corriente off2))) st = 
		ejecutar_comando_yank_automatico (a + linea + offset1) (linea + offset2) st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf
			offset1 = foldr (+) 0 off1
			offset2 = foldr (+) 0 off2
	ejecutar_comando_yank_con_dos_dir (Just (CYankT (Direc Corriente off1) (Direc (Abs b) off2))) st = 
		ejecutar_comando_yank_automatico (linea + offset1) (b + offset2) st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf
			offset1 = foldr (+) 0 off1
			offset2 = foldr (+) 0 off2
	ejecutar_comando_yank_con_dos_dir (Just (CYankT (Direc (Abs a) off1) (Direc Corriente off2))) st = 
		ejecutar_comando_yank_automatico (a + offset1) (linea + offset2) st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf
			offset1 = foldr (+) 0 off1
			offset2 = foldr (+) 0 off2


	-- DEFAULT
	ejecutar_comando_yank_con_dos_dir com st = 
		ejecutar_comando_yank_automatico maximo (maximo + 2) st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf


	ejecutar_comando_yank_automatico :: Int -> Int -> State -> (String, State)
	ejecutar_comando_yank_automatico indice1 indice2 st
		| indice1 > maximo				= ("?\n", (linea, buf, modo, esta_modificado, 'y', nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2))	
		| indice2 > maximo				= ("?\n", (linea, buf, modo, esta_modificado, 'y', nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2))	
		| indice1 <= 0						= ("?\n", (linea, buf, modo, esta_modificado, 'y', nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2))	
		|	indice2 <= 0						= ("?\n", (linea, buf, modo, esta_modificado, 'y', nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2))	
		| indice1 > indice2 			= ("?\n", (linea, buf, modo, esta_modificado, 'y', nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2))	
		| otherwise					 			= ("", (linea, buf, modo, esta_modificado, 'y', nom_arch, obtener_lineas_papelera indice1 indice2 buf, aux, buffer_insert, auxiliar1, auxiliar2))
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf

	obtener_lineas_papelera :: Int -> Int -> [String] -> [String]
	obtener_lineas_papelera a b [] = []
	obtener_lineas_papelera a b buf = drop (a-1) $ take b buf


	-- *********************************************************************************************** --
	ejecutar_comando_join_con_dos_dir :: Maybe Comando -> State -> (String, State)
	ejecutar_comando_join_con_dos_dir (Just (CJoinT (Direc Ultima off1) (Direc Ultima off2))) st = 
		ejecutar_comando_join_automatico (maximo + offset1) (maximo + offset2) st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf
			offset1 = foldr (+) 0 off1
			offset2 = foldr (+) 0 off2
	ejecutar_comando_join_con_dos_dir (Just (CJoinT (Direc Corriente off1) (Direc Corriente off2))) st = 
		ejecutar_comando_join_automatico (linea + offset1) (linea + offset2) st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf
			offset1 = foldr (+) 0 off1
			offset2 = foldr (+) 0 off2
	ejecutar_comando_join_con_dos_dir (Just (CJoinT (Direc (Abs a) off1) (Direc (Abs b) off2))) st = 
		ejecutar_comando_join_automatico (a + offset1) (b + offset2) st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf
			offset1 = foldr (+) 0 off1
			offset2 = foldr (+) 0 off2
	ejecutar_comando_join_con_dos_dir (Just (CJoinT (Direc (Rel a) off1) (Direc (Rel b) off2))) st = 
		ejecutar_comando_join_automatico (a + linea + offset1) (linea + b + offset2) st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf
			offset1 = foldr (+) 0 off1
			offset2 = foldr (+) 0 off2
	ejecutar_comando_join_con_dos_dir (Just (CJoinT (Direc Corriente off1) (Direc Ultima off2))) st = 
		ejecutar_comando_join_automatico (linea + offset1) (maximo + offset2) st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf
			offset1 = foldr (+) 0 off1
			offset2 = foldr (+) 0 off2
	ejecutar_comando_join_con_dos_dir (Just (CJoinT (Direc Ultima off1) (Direc Corriente off2))) st = 
		ejecutar_comando_join_automatico (maximo + offset1) (linea + offset2) st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf
			offset1 = foldr (+) 0 off1
			offset2 = foldr (+) 0 off2
	ejecutar_comando_join_con_dos_dir (Just (CJoinT (Direc (Abs a) off1) (Direc (Rel b) off2))) st = 
		ejecutar_comando_join_automatico (a + offset1) (linea + b + offset2) st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf
			offset1 = foldr (+) 0 off1
			offset2 = foldr (+) 0 off2
	ejecutar_comando_join_con_dos_dir (Just (CJoinT (Direc (Rel a) off1) (Direc (Abs b) off2))) st = 
		ejecutar_comando_join_automatico (a + linea + offset1) (b + offset2) st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf
			offset1 = foldr (+) 0 off1
			offset2 = foldr (+) 0 off2
	ejecutar_comando_join_con_dos_dir (Just (CJoinT (Direc Ultima off1) (Direc (Rel b) off2))) st = 
		ejecutar_comando_join_automatico (maximo + offset1) (linea + b + offset2) st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf
			offset1 = foldr (+) 0 off1
			offset2 = foldr (+) 0 off2
	ejecutar_comando_join_con_dos_dir (Just (CJoinT (Direc (Rel a) off1) (Direc Ultima off2))) st = 
		ejecutar_comando_join_automatico (a + linea + offset1) (maximo + offset2) st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf
			offset1 = foldr (+) 0 off1
			offset2 = foldr (+) 0 off2
	ejecutar_comando_join_con_dos_dir (Just (CJoinT (Direc Ultima off1) (Direc (Abs b) off2))) st = 
		ejecutar_comando_join_automatico (maximo + offset1) (b + offset2) st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf
			offset1 = foldr (+) 0 off1
			offset2 = foldr (+) 0 off2
	ejecutar_comando_join_con_dos_dir (Just (CJoinT (Direc (Abs a) off1) (Direc Ultima off2))) st = 
		ejecutar_comando_join_automatico (a + offset1) (maximo + offset2) st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf
			offset1 = foldr (+) 0 off1
			offset2 = foldr (+) 0 off2
	ejecutar_comando_join_con_dos_dir (Just (CJoinT (Direc Corriente off1) (Direc (Rel b) off2))) st = 
		ejecutar_comando_join_automatico (linea + offset1) (linea + b + offset2) st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf
			offset1 = foldr (+) 0 off1
			offset2 = foldr (+) 0 off2
	ejecutar_comando_join_con_dos_dir (Just (CJoinT (Direc (Rel a) off1) (Direc Corriente off2))) st = 
		ejecutar_comando_join_automatico (a + linea + offset1) (linea + offset2) st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf
			offset1 = foldr (+) 0 off1
			offset2 = foldr (+) 0 off2
	ejecutar_comando_join_con_dos_dir (Just (CJoinT (Direc Corriente off1) (Direc (Abs b) off2))) st = 
		ejecutar_comando_join_automatico (linea + offset1) (b + offset2) st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf
			offset1 = foldr (+) 0 off1
			offset2 = foldr (+) 0 off2
	ejecutar_comando_join_con_dos_dir (Just (CJoinT (Direc (Abs a) off1) (Direc Corriente off2))) st = 
		ejecutar_comando_join_automatico (a + offset1) (linea + offset2) st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf
			offset1 = foldr (+) 0 off1
			offset2 = foldr (+) 0 off2


	-- DEFAULT
	ejecutar_comando_join_con_dos_dir com st = 
		ejecutar_comando_join_automatico (maximo + 2) maximo st
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf
	

	ejecutar_comando_join_automatico :: Int -> Int -> State -> (String, State)
	ejecutar_comando_join_automatico indice1 indice2 st
		| indice1 > maximo	= ("?\n", (linea, buf, modo, esta_modificado, 'j', nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2))	
		| indice2 > maximo	= ("?\n", (linea, buf, modo, esta_modificado, 'j', nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2))	
		| indice1 <= 0		= ("?\n", (linea, buf, modo, esta_modificado, 'j', nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2))	
		| indice2 <= 0		= ("?\n", (linea, buf, modo, esta_modificado, 'j', nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2))	
		| indice1 > indice2 = ("?\n", (linea, buf, modo, esta_modificado, 'j', nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2))	
		| otherwise 		= ("", (indice1, unir_lineas indice1 indice2 buf, modo, True, 'j', nom_arch, obtener_lineas_papelera indice1 indice2 buf, aux, buffer_insert, auxiliar1, auxiliar2))
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf

	unir_lineas :: Int -> Int -> [String] -> [String]
	unir_lineas indice1 indice2 buf = take (indice1-1) buf ++ [unir_lineas_aux (drop (indice1-1) (take indice2 buf))] ++ drop indice2 buf 

	unir_lineas_aux :: [String] -> String
	unir_lineas_aux [] = ""
	unir_lineas_aux (x:xs) = x ++ unir_lineas_aux xs


	-- *********************************************************************************************** --
	ejecutar_comando_move_una_dir :: Maybe Comando -> State -> (String, State)
	ejecutar_comando_move_una_dir (Just (CTransferUn (Direc Todo []))) st = 
		ejecutar_comando_move_con_dos_dir (Just (CTransferT (Direc (Abs 1) []) (Direc Ultima []) (Direc Corriente []))) st
	ejecutar_comando_move_una_dir (Just (CTransferUn (Direc TdoRelativo []))) st = 
		ejecutar_comando_move_con_dos_dir (Just (CTransferT (Direc Corriente []) (Direc Ultima []) (Direc Corriente []))) st
	ejecutar_comando_move_una_dir (Just (CMoveUn direc)) st = 
		ejecutar_comando_move_con_dos_dir (Just (CMoveT direc direc (Direc Corriente []))) st

	ejecutar_comando_move_dos_dir :: Maybe Comando -> State -> (String, State)
	ejecutar_comando_move_dos_dir (Just (CMoveDos direc1 direc2)) st = 
		ejecutar_comando_move_con_dos_dir (Just (CMoveT direc1 direc2 (Direc Corriente []))) st

	ejecutar_comando_move_current (Just (CMoveCurren direc)) st = 
		ejecutar_comando_move_con_dos_dir (Just (CMoveT (Direc Corriente []) (Direc Corriente []) direc)) st

	ejecutar_comando_move_con_dos_dir :: Maybe Comando -> State -> (String, State)
	ejecutar_comando_move_con_dos_dir (Just (CMoveT (Direc Ultima off1) (Direc Ultima off2) dir3)) st = 
		ejecutar_comando_move_automatico (maximo + offset1) (maximo + offset2) st dir3
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf
			offset1 = foldr (+) 0 off1
			offset2 = foldr (+) 0 off2
	ejecutar_comando_move_con_dos_dir (Just (CMoveT (Direc Corriente off1) (Direc Corriente off2) dir3)) st = 
		ejecutar_comando_move_automatico (linea + offset1) (linea + offset2) st dir3
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf
			offset1 = foldr (+) 0 off1
			offset2 = foldr (+) 0 off2
	ejecutar_comando_move_con_dos_dir (Just (CMoveT (Direc (Abs a) off1) (Direc (Abs b) off2) dir3)) st = 
		ejecutar_comando_move_automatico (a + offset1) (b + offset2) st dir3
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf
			offset1 = foldr (+) 0 off1
			offset2 = foldr (+) 0 off2
	ejecutar_comando_move_con_dos_dir (Just (CMoveT (Direc (Rel a) off1) (Direc (Rel b) off2) dir3)) st = 
		ejecutar_comando_move_automatico (a + linea + offset1) (linea + b + offset2) st dir3
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf
			offset1 = foldr (+) 0 off1
			offset2 = foldr (+) 0 off2
	ejecutar_comando_move_con_dos_dir (Just (CMoveT (Direc Corriente off1) (Direc Ultima off2) dir3)) st = 
		ejecutar_comando_move_automatico (linea + offset1) (maximo + offset2) st dir3
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf
			offset1 = foldr (+) 0 off1
			offset2 = foldr (+) 0 off2
	ejecutar_comando_move_con_dos_dir (Just (CMoveT (Direc Ultima off1) (Direc Corriente off2) dir3)) st = 
		ejecutar_comando_move_automatico (maximo + offset1) (linea + offset2) st dir3
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf
			offset1 = foldr (+) 0 off1
			offset2 = foldr (+) 0 off2
	ejecutar_comando_move_con_dos_dir (Just (CMoveT (Direc (Abs a) off1) (Direc (Rel b) off2) dir3)) st = 
		ejecutar_comando_move_automatico (a + offset1) (linea + b + offset2) st dir3
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf
			offset1 = foldr (+) 0 off1
			offset2 = foldr (+) 0 off2
	ejecutar_comando_move_con_dos_dir (Just (CMoveT (Direc (Rel a) off1) (Direc (Abs b) off2) dir3)) st = 
		ejecutar_comando_move_automatico (a + linea + offset1) (b + offset2) st dir3
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf
			offset1 = foldr (+) 0 off1
			offset2 = foldr (+) 0 off2
	ejecutar_comando_move_con_dos_dir (Just (CMoveT (Direc Ultima off1) (Direc (Rel b) off2) dir3)) st = 
		ejecutar_comando_move_automatico (maximo + offset1) (linea + b + offset2) st dir3
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf
			offset1 = foldr (+) 0 off1
			offset2 = foldr (+) 0 off2
	ejecutar_comando_move_con_dos_dir (Just (CMoveT (Direc (Rel a) off1) (Direc Ultima off2) dir3)) st = 
		ejecutar_comando_move_automatico (a + linea + offset1) (maximo + offset2) st dir3
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf
			offset1 = foldr (+) 0 off1
			offset2 = foldr (+) 0 off2
	ejecutar_comando_move_con_dos_dir (Just (CMoveT (Direc Ultima off1) (Direc (Abs b) off2) dir3)) st = 
		ejecutar_comando_move_automatico (maximo + offset1) (b + offset2) st dir3
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf
			offset1 = foldr (+) 0 off1
			offset2 = foldr (+) 0 off2
	ejecutar_comando_move_con_dos_dir (Just (CMoveT (Direc (Abs a) off1) (Direc Ultima off2) dir3)) st = 
		ejecutar_comando_move_automatico (a + offset1) (maximo + offset2) st dir3
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf
			offset1 = foldr (+) 0 off1
			offset2 = foldr (+) 0 off2
	ejecutar_comando_move_con_dos_dir (Just (CMoveT (Direc Corriente off1) (Direc (Rel b) off2) dir3)) st = 
		ejecutar_comando_move_automatico (linea + offset1) (linea + b + offset2) st dir3
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf
			offset1 = foldr (+) 0 off1
			offset2 = foldr (+) 0 off2
	ejecutar_comando_move_con_dos_dir (Just (CMoveT (Direc (Rel a) off1) (Direc Corriente off2) dir3)) st = 
		ejecutar_comando_move_automatico (a + linea + offset1) (linea + offset2) st dir3
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf
			offset1 = foldr (+) 0 off1
			offset2 = foldr (+) 0 off2
	ejecutar_comando_move_con_dos_dir (Just (CMoveT (Direc Corriente off1) (Direc (Abs b) off2) dir3)) st = 
		ejecutar_comando_move_automatico (linea + offset1) (b + offset2) st dir3
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf
			offset1 = foldr (+) 0 off1
			offset2 = foldr (+) 0 off2
	ejecutar_comando_move_con_dos_dir (Just (CMoveT (Direc (Abs a) off1) (Direc Corriente off2) dir3)) st = 
		ejecutar_comando_move_automatico (a + offset1) (linea + offset2) st dir3
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf
			offset1 = foldr (+) 0 off1
			offset2 = foldr (+) 0 off2


	-- DEFAULT
	ejecutar_comando_move_con_dos_dir com st = 
		ejecutar_comando_move_automatico (maximo + 2) maximo st (Direc Ultima [])
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf
	

	ejecutar_comando_move_automatico :: Int -> Int -> State -> Direc -> (String, State)
	ejecutar_comando_move_automatico indice1 indice2 st dir3
		| indice1 > maximo				= ("?\n", (linea, buf, modo, esta_modificado, 'm', nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2))	
		| indice2 > maximo				= ("?\n", (linea, buf, modo, esta_modificado, 'm', nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2))	
		| indice1 <= 0						= ("?\n", (linea, buf, modo, esta_modificado, 'm', nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2))	
		|	indice2 <= 0						= ("?\n", (linea, buf, modo, esta_modificado, 'm', nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2))	
		| indice1 > indice2 			= ("?\n", (linea, buf, modo, esta_modificado, 'm', nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2))	
		| otherwise 							= mover_auxiliar indice1 indice2 st dir3
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf

	mover_auxiliar :: Int -> Int -> State -> Direc -> (String, State)
	mover_auxiliar indice1 indice2 st (Direc Ultima off) = 
		mover_lineas_auxiliar indice1 indice2 st (maximo + offset)
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf
			offset = foldr (+) 0 off
	mover_auxiliar indice1 indice2 st (Direc Corriente off) = 
		mover_lineas_auxiliar indice1 indice2 st (linea + offset)
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf
			offset = foldr (+) 0 off
	mover_auxiliar indice1 indice2 st (Direc (Abs a) off) = 
		mover_lineas_auxiliar indice1 indice2 st (a + offset)
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf
			offset = foldr (+) 0 off
	mover_auxiliar indice1 indice2 st (Direc (Rel a) off) = 
		mover_lineas_auxiliar indice1 indice2 st (a + linea + offset)
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf
			offset = foldr (+) 0 off

	mover_lineas_auxiliar :: Int -> Int -> State -> Int -> (String, State)
	mover_lineas_auxiliar indice1 indice2 st indice3
		| indice3 > maximo 			= ("?\n", (linea, buf, modo, esta_modificado, 'm', nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2))	
		| indice3 < 0						= ("?\n", (linea, buf, modo, esta_modificado, 'm', nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2))
		| indice3 >= indice1 && indice3 < indice2 = ("?\n", (linea, buf, modo, esta_modificado, 'm', nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2))
		| otherwise							= ("", (nueva_linea indice1 indice2 indice3, mover_en_buffer indice1 indice2 buf indice3, modo, True, 'm', nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2))
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf

	mover_en_buffer :: Int -> Int -> [String] -> Int -> [String]
	mover_en_buffer indice1 indice2 buf indice3 
		| indice3 < indice1  	= (take indice3 sin_elementos) ++ a_mover ++ (drop indice3 sin_elementos)
		| indice3 >= indice2 	= take (indice1 - 1) asd ++ (drop indice2 asd) 
		where 
			sin_elementos = (take (indice1 - 1) buf) ++ (drop indice2 buf)
			a_mover = (drop (indice1 - 1) (take indice2 buf))
			asd = (take indice3 buf) ++ a_mover ++ (drop indice3 buf)
			nuevo_indice = indice3 - (length a_mover)

	nueva_linea :: Int -> Int -> Int -> Int
	nueva_linea i1 i2 i3 
		| i3 < i1 	= i3 + i2 - i1 + 1
		| i3 >= i2 	= i3 


	-- *********************************************************************************************** --
	ejecutar_comando_tra_una_dir :: Maybe Comando -> State -> (String, State)
	ejecutar_comando_tra_una_dir (Just (CTransferUn (Direc Todo []))) st = 
		ejecutar_comando_transfer_con_dos_dir (Just (CTransferT (Direc (Abs 1) []) (Direc Ultima []) (Direc Corriente []))) st
	ejecutar_comando_tra_una_dir (Just (CTransferUn (Direc TdoRelativo []))) st = 
		ejecutar_comando_transfer_con_dos_dir (Just (CTransferT (Direc Corriente []) (Direc Ultima []) (Direc Corriente []))) st
	ejecutar_comando_tra_una_dir (Just (CTransferUn direc)) st = 
		ejecutar_comando_transfer_con_dos_dir (Just (CTransferT direc direc (Direc Corriente []))) st

	ejecutar_comando_tra_dos_dir :: Maybe Comando -> State -> (String, State)
	ejecutar_comando_tra_dos_dir (Just (CTransferDos direc1 direc2)) st = 
		ejecutar_comando_transfer_con_dos_dir (Just (CTransferT direc1 direc2 (Direc Corriente []))) st

	ejecutar_comando_tra_current (Just (CTransferCurren direc)) st = 
		ejecutar_comando_transfer_con_dos_dir (Just (CTransferT (Direc Corriente []) (Direc Corriente []) direc)) st

	ejecutar_comando_transfer_con_dos_dir :: Maybe Comando -> State -> (String, State)
	ejecutar_comando_transfer_con_dos_dir (Just (CTransferT (Direc Ultima off1) (Direc Ultima off2) dir3)) st = 
		ejecutar_comando_transfer_automatico (maximo + offset1) (maximo + offset2) st dir3
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf
			offset1 = foldr (+) 0 off1
			offset2 = foldr (+) 0 off2
	ejecutar_comando_transfer_con_dos_dir (Just (CTransferT (Direc Corriente off1) (Direc Corriente off2) dir3)) st = 
		ejecutar_comando_transfer_automatico (linea + offset1) (linea + offset2) st dir3
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf
			offset1 = foldr (+) 0 off1
			offset2 = foldr (+) 0 off2
	ejecutar_comando_transfer_con_dos_dir (Just (CTransferT (Direc (Abs a) off1) (Direc (Abs b) off2) dir3)) st = 
		ejecutar_comando_transfer_automatico (a + offset1) (b + offset2) st dir3
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf
			offset1 = foldr (+) 0 off1
			offset2 = foldr (+) 0 off2
	ejecutar_comando_transfer_con_dos_dir (Just (CTransferT (Direc (Rel a) off1) (Direc (Rel b) off2) dir3)) st = 
		ejecutar_comando_transfer_automatico (a + linea + offset1) (linea + b + offset2) st dir3
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf
			offset1 = foldr (+) 0 off1
			offset2 = foldr (+) 0 off2
	ejecutar_comando_transfer_con_dos_dir (Just (CTransferT (Direc Corriente off1) (Direc Ultima off2) dir3)) st = 
		ejecutar_comando_transfer_automatico (linea + offset1) (maximo + offset2) st dir3
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf
			offset1 = foldr (+) 0 off1
			offset2 = foldr (+) 0 off2
	ejecutar_comando_transfer_con_dos_dir (Just (CTransferT (Direc Ultima off1) (Direc Corriente off2) dir3)) st = 
		ejecutar_comando_transfer_automatico (maximo + offset1) (linea + offset2) st dir3
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf
			offset1 = foldr (+) 0 off1
			offset2 = foldr (+) 0 off2
	ejecutar_comando_transfer_con_dos_dir (Just (CTransferT (Direc (Abs a) off1) (Direc (Rel b) off2) dir3)) st = 
		ejecutar_comando_transfer_automatico (a + offset1) (linea + b + offset2) st dir3
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf
			offset1 = foldr (+) 0 off1
			offset2 = foldr (+) 0 off2
	ejecutar_comando_transfer_con_dos_dir (Just (CTransferT (Direc (Rel a) off1) (Direc (Abs b) off2) dir3)) st = 
		ejecutar_comando_transfer_automatico (a + linea + offset1) (b + offset2) st dir3
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf
			offset1 = foldr (+) 0 off1
			offset2 = foldr (+) 0 off2
	ejecutar_comando_transfer_con_dos_dir (Just (CTransferT (Direc Ultima off1) (Direc (Rel b) off2) dir3)) st = 
		ejecutar_comando_transfer_automatico (maximo + offset1) (linea + b + offset2) st dir3
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf
			offset1 = foldr (+) 0 off1
			offset2 = foldr (+) 0 off2
	ejecutar_comando_transfer_con_dos_dir (Just (CTransferT (Direc (Rel a) off1) (Direc Ultima off2) dir3)) st = 
		ejecutar_comando_transfer_automatico (a + linea + offset1) (maximo + offset2) st dir3
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf
			offset1 = foldr (+) 0 off1
			offset2 = foldr (+) 0 off2
	ejecutar_comando_transfer_con_dos_dir (Just (CTransferT (Direc Ultima off1) (Direc (Abs b) off2) dir3)) st = 
		ejecutar_comando_transfer_automatico (maximo + offset1) (b + offset2) st dir3
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf
			offset1 = foldr (+) 0 off1
			offset2 = foldr (+) 0 off2
	ejecutar_comando_transfer_con_dos_dir (Just (CTransferT (Direc (Abs a) off1) (Direc Ultima off2) dir3)) st = 
		ejecutar_comando_transfer_automatico (a + offset1) (maximo + offset2) st dir3
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf
			offset1 = foldr (+) 0 off1
			offset2 = foldr (+) 0 off2
	ejecutar_comando_transfer_con_dos_dir (Just (CTransferT (Direc Corriente off1) (Direc (Rel b) off2) dir3)) st = 
		ejecutar_comando_transfer_automatico (linea + offset1) (linea + b + offset2) st dir3
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf
			offset1 = foldr (+) 0 off1
			offset2 = foldr (+) 0 off2
	ejecutar_comando_transfer_con_dos_dir (Just (CTransferT (Direc (Rel a) off1) (Direc Corriente off2) dir3)) st = 
		ejecutar_comando_transfer_automatico (a + linea + offset1) (linea + offset2) st dir3
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf
			offset1 = foldr (+) 0 off1
			offset2 = foldr (+) 0 off2
	ejecutar_comando_transfer_con_dos_dir (Just (CTransferT (Direc Corriente off1) (Direc (Abs b) off2) dir3)) st = 
		ejecutar_comando_transfer_automatico (linea + offset1) (b + offset2) st dir3
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf
			offset1 = foldr (+) 0 off1
			offset2 = foldr (+) 0 off2
	ejecutar_comando_transfer_con_dos_dir (Just (CTransferT (Direc (Abs a) off1) (Direc Corriente off2) dir3)) st = 
		ejecutar_comando_transfer_automatico (a + offset1) (linea + offset2) st dir3
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf
			offset1 = foldr (+) 0 off1
			offset2 = foldr (+) 0 off2


	-- DEFAULT
	ejecutar_comando_transfer_con_dos_dir com st = 
		ejecutar_comando_transfer_automatico (maximo + 2) maximo st (Direc Ultima [])
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf
	

	ejecutar_comando_transfer_automatico :: Int -> Int -> State -> Direc -> (String, State)
	ejecutar_comando_transfer_automatico indice1 indice2 st dir3
		| indice1 > maximo				= ("?\n", (linea, buf, modo, esta_modificado, 't', nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2))	
		| indice2 > maximo				= ("?\n", (linea, buf, modo, esta_modificado, 't', nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2))	
		| indice1 <= 0						= ("?\n", (linea, buf, modo, esta_modificado, 't', nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2))	
		|	indice2 <= 0						= ("?\n", (linea, buf, modo, esta_modificado, 't', nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2))	
		| indice1 > indice2 			= ("?\n", (linea, buf, modo, esta_modificado, 't', nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2))	
		| otherwise 							= transfer_auxiliar indice1 indice2 st dir3
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf

	transfer_auxiliar :: Int -> Int -> State -> Direc -> (String, State)
	transfer_auxiliar indice1 indice2 st (Direc Ultima off) = 
		transfer_lineas_auxiliar indice1 indice2 st (maximo + offset)
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf
			offset = foldr (+) 0 off
	transfer_auxiliar indice1 indice2 st (Direc Corriente off) = 
		transfer_lineas_auxiliar indice1 indice2 st (linea + offset)
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf
			offset = foldr (+) 0 off
	transfer_auxiliar indice1 indice2 st (Direc (Abs a) off) = 
		transfer_lineas_auxiliar indice1 indice2 st (a + offset)
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf
			offset = foldr (+) 0 off
	transfer_auxiliar indice1 indice2 st (Direc (Rel a) off) = 
		transfer_lineas_auxiliar indice1 indice2 st (a + linea + offset)
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf
			offset = foldr (+) 0 off

	transfer_lineas_auxiliar :: Int -> Int -> State -> Int -> (String, State)
	transfer_lineas_auxiliar indice1 indice2 st indice3
		| indice3 > maximo 			= ("?\n", (linea, buf, modo, esta_modificado, 't', nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2))	
		| indice3 < 0						= ("?\n", (linea, buf, modo, esta_modificado, 't', nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2))
		| otherwise							= ("", (indice3 + indice2 - indice1 + 1, transfer_en_buffer indice1 indice2 buf indice3, modo, True, 't', nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2))
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux, buffer_insert, auxiliar1, auxiliar2) = st
			maximo = length buf

	transfer_en_buffer :: Int -> Int -> [String] -> Int -> [String]
	transfer_en_buffer indice1 indice2 buf indice3 = (take indice3 buf) ++ a_transfer ++ (drop indice3 buf)
		where 
			a_transfer = (drop (indice1 - 1) (take indice2 buf)) 


	-- *********************************************************************************************** --
	-- *** *** *** *** *** *** --
	-- Funciones auxiliares
	-- *** *** *** *** *** *** --

	insertar :: Int -> Buffer -> Buffer -> Buffer
	insertar n papelera buffer 			= (take (n-1) buffer) ++ papelera ++ (drop (n-1) buffer)

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

	obtener_linea_sin_salto :: Int -> [String] -> String
	obtener_linea_sin_salto linea buf = (buf !! (linea - 1))

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

	paste :: [String] -> Int -> [String] -> [String]
	paste papelera linea buf = (take linea buf) ++ papelera ++ (drop linea buf)
