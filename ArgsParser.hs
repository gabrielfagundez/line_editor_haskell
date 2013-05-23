module ArgsParser where

import Prelude
import ParseLib

-- El tipo de datos de Arg es un sinonimo de String
type Arg = String

-- Parser de Argumentos
args_parser :: Parse Char Arg
args_parser arg = [(arg, "")]
