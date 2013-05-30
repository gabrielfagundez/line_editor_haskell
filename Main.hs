module Main(main) where

	import System.IO
	import System.Environment
	import Data.List

	import System.Directory

	import ActionParser
	import ArgsParser
	import ParseLib
	import Comandos

	-- Funcion Principal
	main :: IO ()
	main = do
		args <- getArgs
		-- Verifico que haya al menos 1 argumento
		if (length args == 0)
			then do
				putStr "edi: missing file path\n"
				return ()
		else do
			-- Verifico si el path es un directorio
			existeDir <- doesDirectoryExist (head args)
			if (existeDir) 
				then do 
					putStr "edi: file path is a directory\n"
					return ()
			else do
				-- Verifico que el path del file es valido
				existeDir <- existFileDirectory (head args)
				if (not existeDir) 
					then do 
						putStr "edi: directory file path not exist\n"
						return ()
				else do
					-- Verifico que el archivo exista
					existeFile <- doesFileExist (head args)
					if (existeFile) 
						then do 
							buffer 	<- getLines $ head args
							edi (length buffer, buffer, ModoComando, False, 'I', head args, [], 0) False
					else do
						putStr (head args)
						putStr ": No such file or directory\n"
						edi (0, [], ModoComando, False, 'N', head args, [], 0) False

	-- *** *** *** *** *** *** --
	-- Funciones que componen el main
	-- *** *** *** *** *** *** --

	-- Funcion sobre la cual itera el editor
	edi ::  State -> Bool -> IO ()
	edi estado debo_salir =
		if (not debo_salir) then (
			do
				let (_, buffer, modo_actual, _, ultimo_comando, _, _, _)	 = estado 
				let (a_imprimir, estado_actual) = imprimir_cantidad_palabras ultimo_comando estado
				putStr a_imprimir
				
				linea_cruda <- getLine
				let linea_leida = borrar_espacios linea_cruda modo_actual -- Lee el String de la entrada hasta el fin de linea
				
				let (_, _, modo_actual, buf_mod, _, _, _, _)	 	= estado_actual 
				let (comando_leido, pr) 												= parse_string_entrada linea_leida
				let (a_mostrar, nuevo_estado) 									= actualizar linea_leida comando_leido estado_actual
				let (argumento)																	= segundo_argumento pr nuevo_estado

				putStr a_mostrar
				putStr argumento


				let (_, buf, _, _, comando_ejecutado, nom_arch, _, _)	 		= nuevo_estado
				salvar comando_ejecutado nom_arch buf
				
				-- Llamada recursiva a si mismo con estado modificado
				edi (nuevo_estado) (salir comando_leido buf_mod (modo_actual == ModoInsertar))
		)
		else 
			do return ()

	-- Lee un archivo externo y retorna una lista de lineas
	getLines :: FilePath -> IO [String]
	getLines path = do contents <- readFile path
	                   return (lines contents)

	existFileDirectory:: FilePath -> IO Bool
	existFileDirectory path = existeDir
		where
			splitPath = split path '/'
			pathAux = concatAux (take ((length splitPath) - 1) splitPath) '/'
			existeDir = doesDirectoryExist pathAux

	concatAux :: [String] -> Char -> String
	concatAux [] _ = []
	concatAux (x:xs) char = x ++ [char] ++ concatAux xs char	

	split :: String -> Char -> [String]
	split [] delim = [""]
	split (c:cs) delim
	   | c == delim = "" : rest
	   | otherwise = (c : head rest) : tail rest
	   where
	       rest = split cs delim

	-- *** *** *** *** *** *** --
	-- Funciones auxiliares
	-- *** *** *** *** *** *** --

	-- Determina si se debe salir, se ejecuto el comando Q
	salir :: Maybe Comando -> Bool -> Bool -> Bool
	salir _ _ True = False
	salir comando mod modo_insertar = 
		case comando of
			Nothing 	-> False
			Just c 		-> case c of
				CExitIncond 				-> True
				CExit 							-> not(mod)
				_ 									-> False


	-- Funcion que ejecuta el comando sobre el estado y retorna nuevos estado y String a mostrar
	actualizar :: String -> Maybe Comando -> State -> (String, State)
	actualizar string com prev_state
		| (mod == ModoInsertar)				=	actualizar_insertar string prev_state
		| (mod == ModoComando) 				= actualizar_comando com prev_state
		where (_, _, mod, _, _, _, _, _) = prev_state

	-- Funcion que ejecuta el comando sobre el estado y retorna nuevos estado y String a mostrar
	actualizar_comando :: Maybe Comando -> State -> (String, State)
	actualizar_comando Nothing prev_state = ("?\n", (linea, buf, modo, esta_modificado, '?', nom_arch, papelera, aux))
		where (linea, buf, modo, esta_modificado, _, nom_arch, papelera, aux) = prev_state
	actualizar_comando comando prev_state = ejecutar_comando_modo_comando comando prev_state

	-- Funcion que ejecuta el comando sobre el estado y retorna nuevos estado y String a mostrar
	actualizar_insertar :: String -> State -> (String, State)
	actualizar_insertar string prev_state = ejecutar_comando_modo_insertar string prev_state


	segundo_argumento :: Char -> State -> String
	segundo_argumento 'n' nuevo_estado 	= (show $ linea) ++ "\t" ++ (obtener_linea linea buf)
		where 
			(linea, buf, _, _, _, _, _, _) = nuevo_estado
	segundo_argumento 'p' nuevo_estado	= (obtener_linea linea buf)
		where 
			(linea, buf, _, _, _, _, _, _) = nuevo_estado
	segundo_argumento '-' nuevo_estado 	= ""
	segundo_argumento pr nuevo_estado 	= ""



	-- Funcion que chequea si debe imprimir conteo de palabras y lo retorna
	imprimir_cantidad_palabras :: Char -> State -> (String, State)
	imprimir_cantidad_palabras ultimo_comando estado
		| ultimo_comando == 'I'			= (cantidad ++ "\n", (linea, buf, modo, esta_modificado, 'G', nom_arch, papelera, aux))
		| ultimo_comando == 'w'			= (cantidad ++ "\n", (linea, buf, modo, esta_modificado, 'G', nom_arch, papelera, aux))
		| otherwise									= ("", estado)
		where 
			(linea, buf, modo, esta_modificado, i, nom_arch, papelera, aux) = estado 
			cantidad = show $ cantidad_palabras buf

	cantidad_palabras :: [String] -> Int
	cantidad_palabras [] 			= 0
	--cantidad_palabras (x:xs)	= (length $ filter (not . null) (splitOn " " x)) + cantidad_palabras_resto
		--where cantidad_palabras_resto = cantidad_palabras xs
	cantidad_palabras (x:xs) = 1 + length x + cantidad_palabras xs

	salvar :: Char -> FilePath -> [[Char]] -> IO ()
	salvar comando_ejecutado nom_arch buf = 
		if (comando_ejecutado == 'w') then (
			do
			crear_archivo_externo nom_arch
			escribir_arreglo_archivo_externo nom_arch buf
		)
		else 
			do return ()

	borrar_espacios :: String -> ConsoleState -> String 
	borrar_espacios [] _ = []
	borrar_espacios linea_cruda ModoInsertar = linea_cruda
	borrar_espacios (x:xs) _
		| x == ' '			= borrar_espacios xs ModoComando
		| otherwise 		= x : borrar_espacios xs ModoComando
