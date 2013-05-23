module DirectionsParser where

	import ParseLib
	import Data.Char

	-- El tipo de datos de Directions
	data Direc = Direc Base [Int]
		deriving (Eq, Show)

	data Base = Abs Int
						| Ultima
						| Corriente
						| Rel Int
						| Todo
		deriving (Eq, Show)

	-- *** *** *** *** *** *** --
	-- Parser de direcciones
	-- *** *** *** *** *** *** --

	directions_parser st
		| list == [] 		= []
		| otherwise			= [last list]
		where list = lista_direcciones st


	lista_direcciones = ((parse_base >*> parser_operadores) `build` uncurry Direc)

	parser_operadores = list parser_operador

	parser_operador = (parse_operador >*> parse_numero) `build` uncurry (*)

	-- *** *** *** *** *** *** --
	-- Parser de base
	-- *** *** *** *** *** *** --

	parse_base = parse_ultima `alt` parse_corriente `alt` parse_absolute `alt` parse_relative `alt` parse_todo

	parse_todo :: Parse Char Base
	parse_todo "" = []
	parse_todo (x:xs) 
		| x == '.' 			= [(Todo, xs)]
		| otherwise 		= []

	parse_ultima :: Parse Char Base
	parse_ultima "" = []
	parse_ultima (x:xs) 
		| x == '$' 			= [(Ultima, xs)]
		| otherwise 		= []

	parse_corriente :: Parse Char Base	
	parse_corriente "" = []
	parse_corriente (x:xs) 
		| x == '\'' 		= [(Corriente, xs)]
		| otherwise 		= []

	parse_absolute :: Parse Char Base
	parse_absolute "" = []
	parse_absolute (x:xs)
		| isNumber x 		= [(Abs (num_to_int num), (resto_string num (x:xs)))]	
		| otherwise			= []
		where num = head $ lexer (x:xs)

	parse_relative :: Parse Char Base
	parse_relative "" = []
	parse_relative (x:xs)
		| x == '+'			= [(Rel (num_to_int num), (resto_string num xs))]
		| x == '-'			= [(Rel neg_num, (resto_string num xs))]
		| otherwise			= []
		where 
			num = head $ lexer (xs)
			neg_num = 0 - num_to_int num


	parse_operador "" = []
	parse_operador (x:xs)
		| x == '+'			=	[(1, xs)]
		| x == '-'			=	[(-1, xs)]
		| otherwise 		= [] 

	parse_numero "" = []
	parse_numero (x:xs)
		| isNumber x 		= [((num_to_int num), (resto_string num (x:xs)))]	
		| otherwise			= []
		where num = head $ lexer (x:xs)


	-- *** *** *** *** *** *** --
	-- Funciones auxiliares
	-- *** *** *** *** *** *** --
	type Token = String

	lexer :: String -> [Token]
	lexer "" = []
	lexer cs@(a:as) 
		| isAlpha a 	= let (us,ws) = span isAlpha cs
	                 	in  us : lexer ws
		| isSpace a  	= lexer as
		| isNumber a  = let (us,ws) = span isNumber cs
	                 	in  us : lexer ws	
	  | otherwise  	= [a] : lexer as	

	num_to_int :: [Char] -> Int
	num_to_int [] = 0
	num_to_int (x:xs) 
		| xs == [] 		= digitToInt x
		| otherwise 	= (num_to_int xs) + (10 ^ (length xs)) * (digitToInt x)

	resto_string [] ys 		= ys 
	resto_string (x:xs) (y:ys) 
		| x == y 						=	resto_string xs ys
		| otherwise 				= ys