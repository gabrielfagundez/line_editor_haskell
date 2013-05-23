module Comandos where

	import ActionParser
	import ArgsParser
	import DirectionsParser
	import ParseLib

	-- Definir Data Comando = ...
	data Comando = 	CExitIncond | CExit | CInsertCurr | CWrite | CWriteArg Arg | CPrintCurr | Int |
									CPrint Direc | CInsert Direc | CAppend Direc
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
						`alt` comando_append_con_direccion


	-- *** *** *** *** *** *** --
	-- Funciones aplicadas a un unico tipo de comandos
	-- *** *** *** *** *** *** --

	comando_salir :: Parse Char Comando
	comando_salir = (action_parser_cond 'q') `build` const CExit

	comando_salir_incondicional :: Parse Char Comando
	comando_salir_incondicional = (action_parser_cond 'Q') `build` const CExitIncond

	comando_insertar_linea_actual :: Parse Char Comando
	comando_insertar_linea_actual = (action_parser_cond 'i') `build` const CInsertCurr

	comando_escribir :: Parse Char Comando
	comando_escribir = (action_parser_cond 'w') `build` const CWrite

	comando_escribir_con_argumento :: Parse Char Comando
	comando_escribir_con_argumento = ((action_parser_cond 'w') >*> (token ' ') >*> (args_parser)) `build` \(_, (_, arg)) -> CWriteArg arg 
										
	comando_imprimir_actual :: Parse Char Comando
	comando_imprimir_actual = (action_parser_cond 'p') `build` const CPrintCurr

	comando_imprimir_con_direccion :: Parse Char Comando
	comando_imprimir_con_direccion = (directions_parser >*> (action_parser_cond 'p')) `build` \(dir, _) -> CPrint dir

	comando_insertar_con_direccion :: Parse Char Comando
	comando_insertar_con_direccion = (directions_parser >*> (action_parser_cond 'i')) `build` \(dir, _) -> CInsert dir

	comando_append_con_direccion :: Parse Char Comando
	comando_append_con_direccion = (directions_parser >*> (action_parser_cond 'a')) `build` \(dir, _) -> CAppend dir


	-- *** *** *** *** *** *** --
	-- Ejecucion de los comandos
	-- *** *** *** *** *** *** --

	-- Ejecucion en modo insertar
	ejecutar_comando_modo_insertar :: String -> State -> (String, State)
	ejecutar_comando_modo_insertar "." st = ("", (linea, buf, ModoComando, esta_modificado, ult_com, nom_arch))
		where (linea, buf, _, esta_modificado, ult_com, nom_arch) = st
	ejecutar_comando_modo_insertar string st = ("", (nueva_linea, nuevo_buffer, modo, True, ult_com, nom_arch))
		where 
			(linea, buf, modo, esta_modificado, ult_com, nom_arch) = st
			nueva_linea = (linea + 1)
			nuevo_buffer = insert linea string buf
	
	-- Ejecucion en modo comando
	ejecutar_comando_modo_comando :: Maybe Comando -> State -> (String, State)
	ejecutar_comando_modo_comando comando st 
		| (comando == Just CExit)																=	ejecutar_salir comando st
		| (comando == Just CExitIncond)													= ejecutar_salir_incond comando st
		| (comando == Just CInsertCurr) 												= ejecutar_insertar_actual comando st
		| (comando == Just CWrite)															=	ejecutar_comando_write comando st 
		| (comando == Just CPrintCurr)													=	ejecutar_comando_print_current comando st 
	ejecutar_comando_modo_comando (Just (CWriteArg arg)) st 	= ejecutar_comando_write_con_ruta (Just (CWriteArg arg)) st
	ejecutar_comando_modo_comando (Just (CPrint direc)) st 		= ejecutar_comando_print_con_dir (Just (CPrint direc)) st
	ejecutar_comando_modo_comando (Just (CInsert direc)) st 	= ejecutar_comando_insert_con_dir (Just (CInsert direc)) st
	ejecutar_comando_modo_comando (Just (CAppend direc)) st 	= ejecutar_comando_append_con_dir (Just (CAppend direc)) st
 
	-- *** *** *** *** *** *** --
	-- Ejecucion de los comandos especificos
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

	ejecutar_comando_print_con_dir :: Maybe Comando -> State -> (String, State)
	ejecutar_comando_print_con_dir (Just (CPrint (Direc Ultima []))) st = (obtener_linea (length buf - 1) buf ,(length buf - 1, buf, modo, esta_modificado, 'p', nom_arch))
		where 
			(_, buf, modo, esta_modificado, _, nom_arch) = st
	ejecutar_comando_print_con_dir (Just (CPrint (Direc Corriente []))) st = ((obtener_linea linea buf), (linea, buf, modo, esta_modificado, 'p', nom_arch))
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch) = st
	ejecutar_comando_print_con_dir (Just (CPrint (Direc (Abs a) []))) st 
		| a == 0 										= ("?\n", (linea, buf, modo, esta_modificado, 'p', nom_arch))
		| a > maximo								= ("?\n", (linea, buf, modo, esta_modificado, 'p', nom_arch))
		| otherwise 								= ((obtener_linea (a - 1) buf), (a - 1, buf, modo, esta_modificado, 'p', nom_arch))
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch) = st
			maximo = length buf
	ejecutar_comando_print_con_dir (Just (CPrint (Direc (Rel a) []))) st 
		| absoluta < 0 							= ("?\n", (linea, buf, modo, esta_modificado, 'p', nom_arch))
		| absoluta > (maximo	- 1)	= ("?\n", (linea, buf, modo, esta_modificado, 'p', nom_arch))
		| otherwise			 						= ((obtener_linea (absoluta) buf), (absoluta, buf, modo, esta_modificado, 'p', nom_arch))
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch) = st
			maximo = length buf
			absoluta = linea + a
	ejecutar_comando_print_con_dir (Just (CPrint (Direc Todo []))) st = (compactar buf ,(length buf - 1, buf, modo, esta_modificado, 'p', nom_arch))
		where 
			(_, buf, modo, esta_modificado, _, nom_arch) = st

	ejecutar_comando_insert_con_dir :: Maybe Comando -> State -> (String, State)
	ejecutar_comando_insert_con_dir (Just (CInsert (Direc Ultima []))) st = ("" ,(length buf - 1, buf, ModoInsertar, esta_modificado, 'i', nom_arch))
		where 
			(_, buf, _, esta_modificado, _, nom_arch) = st
	ejecutar_comando_insert_con_dir (Just (CInsert (Direc Corriente []))) st = ("", (linea, buf, ModoInsertar, esta_modificado, 'i', nom_arch))
		where 
			(linea, buf, _, esta_modificado, _, nom_arch) = st
	ejecutar_comando_insert_con_dir (Just (CInsert (Direc (Abs a) []))) st 
		| a == 0 										= ("", (0, buf, ModoInsertar, esta_modificado, 'i', nom_arch))
		| a > maximo								= ("?\n", (linea, buf, modo, esta_modificado, 'i', nom_arch))
		| otherwise 								= ("", (a - 1, buf, ModoInsertar, esta_modificado, 'i', nom_arch))
		where  	
			(linea, buf, modo, esta_modificado, _, nom_arch) = st
			maximo = length buf
	ejecutar_comando_insert_con_dir (Just (CInsert (Direc (Rel a) []))) st 
		| absoluta < 0 							= ("?\n", (linea, buf, modo, esta_modificado, 'i', nom_arch))
		| absoluta > (maximo)				= ("?\n", (linea, buf, modo, esta_modificado, 'i', nom_arch))
		| absoluta == (maximo)			= ("", (maximo - 1, buf, ModoInsertar, esta_modificado, 'i', nom_arch))
		| otherwise			 						= ("", (absoluta, buf, ModoInsertar, esta_modificado, 'i', nom_arch))
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch) = st
			maximo = length buf
			absoluta = (linea + a) + 1

	ejecutar_comando_append_con_dir :: Maybe Comando -> State -> (String, State)
	ejecutar_comando_append_con_dir (Just (CInsert (Direc Ultima []))) st = ("" ,(length buf, buf, ModoInsertar, esta_modificado, 'i', nom_arch))
		where 
			(_, buf, _, esta_modificado, _, nom_arch) = st
	ejecutar_comando_append_con_dir (Just (CInsert (Direc Corriente []))) st = ("", (linea + 1, buf, ModoInsertar, esta_modificado, 'i', nom_arch))
		where 
			(linea, buf, _, esta_modificado, _, nom_arch) = st
	ejecutar_comando_append_con_dir (Just (CInsert (Direc (Abs a) []))) st 
		| a == 0 										= ("", (1, buf, ModoInsertar, esta_modificado, 'i', nom_arch))
		| a > maximo								= ("?\n", (linea, buf, modo, esta_modificado, 'i', nom_arch))
		| otherwise 								= ("", (a, buf, ModoInsertar, esta_modificado, 'i', nom_arch))
		where  	
			(linea, buf, modo, esta_modificado, _, nom_arch) = st
			maximo = length buf
	ejecutar_comando_append_con_dir (Just (CInsert (Direc (Rel a) []))) st 
		| absoluta < 0 							= ("?\n", (linea, buf, modo, esta_modificado, 'i', nom_arch))
		| absoluta > (maximo)				= ("?\n", (linea, buf, modo, esta_modificado, 'i', nom_arch))
		| absoluta == (maximo)			= ("", (maximo, buf, ModoInsertar, esta_modificado, 'i', nom_arch))
		| otherwise			 						= ("", (absoluta + 1, buf, ModoInsertar, esta_modificado, 'i', nom_arch))
		where 
			(linea, buf, modo, esta_modificado, _, nom_arch) = st
			maximo = length buf
			absoluta = (linea + a) + 1


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
	obtener_linea linea buf = (buf !! linea) ++ ['\n']

	compactar = unlines

