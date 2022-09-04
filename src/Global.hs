{-|
Module      : Global
Description : Define el estado global del compilador
Copyright   : (c) Mauro Jaskelioff, Guido Martínez, 2020.
License     : GPL-3
Maintainer  : mauro@fceia.unr.edu.ar
Stability   : experimental

-}
module Global where

import Lang

--Toda la informacion que se quiera durante la ejecucion. Por ejemplo en el ejercicio 13 se puede agregar el tiempo de TypeChecker de cada expresion.
data GlEnv = GlEnv {
  inter :: Bool,        --  ^ True, si estamos en modo interactivo.
                        -- Este parámetro puede cambiar durante la ejecución:
                        -- Es falso mientras se cargan archivos, pero luego puede ser verdadero.
  lfile :: String,      -- ^ Último archivo cargado.
  cantDecl :: Int,      -- ^ Cantidad de declaraciones desde la última carga
  glb :: [Decl TTerm],  -- ^ Entorno con declaraciones globales
  typeDefs :: [(Name, Ty)] -- ^ Lista de tipos definidos
}

-- ^ Entorno de tipado de declaraciones globales
tyEnv :: GlEnv ->  [(Name,Ty)]
tyEnv g = map (\(Decl _ n b) -> (n, getTy b))  (glb g)

-- ^ Entorno de nombres de tipo
tyEnvDef :: GlEnv ->  [(Name,Ty)]
tyEnvDef g = (typeDefs g)

{-
 Tipo para representar las banderas disponibles en línea de comando.
-}
data Mode =
    Interactive
  | Typecheck
  -- | InteractiveCEK
  -- | Bytecompile
  -- | RunVM
  -- | CC
  -- | Canon
  -- | Assembler
  -- | Build
data Conf = Conf {
    opt :: Bool,          --  ^ True, si estan habilitadas las optimizaciones.
    modo :: Mode
}

-- | Valor del estado inicial
initialEnv :: GlEnv
initialEnv = GlEnv True "" 0 [] []
