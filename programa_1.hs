import Text.ParserCombinators.Parsec
import qualified Data.Map as Map
import System.IO (isEOF)

-- Definición de tipos de datos para expresiones y declaraciones
data Expr = Val Double          -- Expresión que representa un valor numérico
          | Var String          -- Expresión que representa una variable
          | Add Expr Expr       -- Expresión que representa una suma de dos expresiones
          | Sub Expr Expr       -- Expresión que representa una resta de dos expresiones
          | Mul Expr Expr       -- Expresión que representa una multiplicación de dos expresiones
          | Div Expr Expr       -- Expresión que representa una división de dos expresiones
          deriving (Show)

data Stmt = Assign String Expr  -- Declaración que asigna un valor a una variable
          | ExprStmt Expr       -- Declaración que contiene una expresión para ser evaluada
          deriving (Show)

-- Parser para expresiones
exprParser :: Parser Expr
exprParser = try termParser <|> try factorParser <|> valParser <|> varParser
  where
    termParser = do
      spaces
      x <- factorParser
      spaces
      op <- oneOf "+-"
      spaces
      y <- exprParser
      return $ case op of
        '+' -> Add x y
        '-' -> Sub x y

    factorParser = do
      spaces
      x <- termParser <|> valParser <|> varParser <|> parensParser
      spaces
      op <- oneOf "*/"
      spaces
      y <- exprParser <|> factorParser
      return $ case op of
        '*' -> Mul x y
        '/' -> Div x y

    valParser = fmap Val $ fmap read $ many1 digit

    varParser = fmap Var $ many1 letter

    parensParser = do
      char '('
      spaces
      x <- exprParser
      spaces
      char ')'
      return x

-- Parser para declaraciones
stmtParser :: Parser Stmt
stmtParser = try assignParser <|> exprStmtParser
  where
    assignParser = do
      var <- many1 letter
      spaces
      char '='
      spaces
      expr <- exprParser
      return $ Assign var expr

    exprStmtParser = do
      expr <- exprParser
      return $ ExprStmt expr

-- Evaluar expresiones
evalExpr :: Expr -> Map.Map String Double -> Double
evalExpr (Val x) _ = x
evalExpr (Var x) vars = case Map.lookup x vars of
  Just val -> val
  Nothing -> error $ "Variable " ++ x ++ " no encontrada."
evalExpr (Add x y) vars = evalExpr x vars + evalExpr y vars
evalExpr (Sub x y) vars = evalExpr x vars - evalExpr y vars
evalExpr (Mul x y) vars = evalExpr x vars * evalExpr y vars
evalExpr (Div x y) vars = evalExpr x vars / evalExpr y vars

-- Evaluar declaraciones
evalStmt :: Stmt -> Map.Map String Double -> (Map.Map String Double, Maybe Double)
evalStmt (Assign var expr) vars =
  let val = evalExpr expr vars
  in (Map.insert var val vars, Nothing)
evalStmt (ExprStmt expr) vars = (vars, Just $ evalExpr expr vars)

-- Ciclo Leer-Evaluar-Imprimir
repl :: Map.Map String Double -> IO ()
repl vars = do
  putStr "> "
  input <- getLine
  if input == "exit"
    then putStrLn "Saliendo del REPL."
    else do
      case parse stmtParser "" input of
        Left err -> putStrLn $ "Error: " ++ show err
        Right stmt -> do
          let (newVars, result) = evalStmt stmt vars
          case result of
            Just res -> putStrLn $ "Resultado: " ++ show res
            Nothing -> return ()
          repl newVars

-- Función principal para iniciar el REPL
main :: IO ()
main = do
  putStrLn "Bienvenido al REPL Haskell."
  putStrLn "Ingrese expresiones matemáticas o declaraciones de asignación."
  putStrLn "Presione Enter para evaluar una expresión o declarar una variable."
  repl Map.empty
