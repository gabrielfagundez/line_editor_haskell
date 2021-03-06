module ActionParser where

import Prelude
import ParseLib

-- El tipo de datos de Accion es un enumerado de acciones
data Action = Append | Change | Equal | Delete | Insert | Join | Move | Numerate | Print | Quit | QuitInconditional | Cut | Yank | EqualSymbol | T | Write | Error
	deriving (Eq, Ord, Show, Read, Bounded, Enum)

-- Parser de Acciones
action_parser :: Parse Char Action
action_parser (x:xs) 	
	| char_into_action x == Error 	= []
	| otherwise											= [((char_into_action x), xs)]
action_parser []									= []

-- Parser condicional de Acciones
action_parser_cond :: Char -> Parse Char Action
action_parser_cond a (x:xs) 	
	| x == a 									= [((char_into_action x), xs)]
	| otherwise								= []
action_parser_cond _ []			= []

char_into_action :: Char -> Action
char_into_action x 
	| x == 'a'	= Append -- Realizado y validado
	| x == 'c'	= Change -- Realizado y validado
	| x == 'd'	= Delete -- Realizado y validado
	| x == 'i'	= Insert -- Realizado y validado
	| x == 'j'	= Join
	| x == 'm'	= Move
	| x == 'n'	= Numerate -- Realizado y validado 
	| x == 'p'	= Print -- Realizado y validado
	| x == 'q'	= Quit -- Realizado y validado
	| x == 'Q'	= QuitInconditional -- Realizado y validado
	| x == 't'	= T
	| x == 'x'	= Cut -- Realizado
	| x == 'y'	= Yank --Realizado
	| x == 'w'	= Write -- Realizado y validado
	| x == '=' 	= Equal
	| otherwise	= Error



