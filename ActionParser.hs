module ActionParser where

import Prelude
import ParseLib

-- El tipo de datos de Accion es un enumerado de acciones
data Action = Append | Change | Delete | Insert | Join | Move | Numerate | Print | Quit | QuitInconditional | Cut | Yank | EqualSymbol | T | Write | Error
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
	| x == 'a'	= Append
	| x == 'c'	= Change
	| x == 'd'	= Delete
	| x == 'i'	= Insert
	| x == 'j'	= Join
	| x == 'm'	= Move
	| x == 'n'	= Numerate
	| x == 'p'	= Print
	| x == 'q'	= Quit
	| x == 'Q'	= QuitInconditional
	| x == 't'	= T
	| x == 'x'	= Cut
	| x == 'y'	= Yank
	| x == 'w'	= Write
	| otherwise	= Error



