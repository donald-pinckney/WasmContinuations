module ToyComp

import ToyAST
import WasmAST
import ToyInterp
import WasmInterp
import Data.Vect
import Data.Fin

-- valueToWasmValue : Value -> WasmValue
-- valueToWasmValue (ValueInt x) = WasmValueI64 x
-- valueToWasmValue (ValueFloat x) = WasmValueF64 x

-- compile_function : 
compile_module : Module nmfns -> WasmModule
compile_module mod = ?compile_module_rhs_2
