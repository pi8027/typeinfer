
module Main where

import Data.Char
import Data.List
import Data.Maybe
import qualified Data.Map as M
import qualified Data.ByteString.Char8 as BS
import Data.Attoparsec.Char8 as AP
import Control.Applicative
import Control.Monad.State
import Control.Arrow
import System.IO

-- data types

data Type
    = TVar Int
    | TFun Type Type
    deriving Show

type TypeScheme = ([Int], Type)

data Term
    = Var String
    | App Term Term
    | Abs String Term
    | Let String Term Term
    deriving Show

type Assump = [(String, TypeScheme)]

data Prop
    = PTypeRel Assump Term Type
    | PIn String TypeScheme Assump
    | PNeq String String
    deriving Show

data Judge
    = Judge String [Judge] Prop
    | TJudge Prop -- Trivial
    deriving Show

type Subst = M.Map Int Type

type TM = StateT Int Maybe

-- type variable set

tvars :: Type -> [Int]
tvars (TVar n) = [n]
tvars (TFun t1 t2) = union (tvars t1) (tvars t2)

tvarsTs :: TypeScheme -> [Int]
tvarsTs (tvs, t) = union tvs $ tvars t

tvarsProp :: Prop -> [Int]
tvarsProp (PTypeRel assump _ ty) = nub $ (assump >>= tvarsTs . snd) ++ tvars ty
tvarsProp (PIn _ ty _) = tvarsTs ty
tvarsProp _ = []

tvarsJudge :: Judge -> [Int]
tvarsJudge (Judge _ subjs prop) = nub $ (subjs >>= tvarsJudge) ++ tvarsProp prop

-- type inference

freevarsTs :: TypeScheme -> [Int]
freevarsTs (tvs, t) = tvars t \\ tvs

substitute :: Subst -> Type -> Type
substitute s (TVar n) = fromMaybe (TVar n) (M.lookup n s)
substitute s (TFun t1 t2) = TFun (substitute s t1) (substitute s t2)

substitute_assump :: Subst -> Assump -> Assump
substitute_assump s a = map (second (second (substitute s))) a

substitute_prop :: Subst -> Prop -> Prop
substitute_prop s (PTypeRel assump term ty) =
    PTypeRel (substitute_assump s assump) term (substitute s ty)
substitute_prop s (PIn name ts assump) =
    PIn name (second (substitute s) ts) (substitute_assump s assump)
substitute_prop s p = p

substitute_judge :: Subst -> Judge -> Judge
substitute_judge s (Judge rulename subjs prop) =
    Judge rulename (map (substitute_judge s) subjs) (substitute_prop s prop)
substitute_judge s (TJudge prop) = TJudge (substitute_prop s prop)

occurscheck :: Int -> Type -> Bool
occurscheck n (TVar n') = n == n'
occurscheck n (TFun t1 t2) = occurscheck n t1 && occurscheck n t2

unify :: Subst -> [(Type, Type)] -> Maybe Subst
unify s [] = Just s
unify s ((TVar n, TVar n') : cs) | n == n' = unify s cs
unify s ((TVar n, t) : cs) | occurscheck n t = Nothing
unify s ((TVar n, t) : cs) = unify (M.insert n t s') cs' where
    substitute' = substitute (M.singleton n t)
    s' = M.map substitute' s
    cs' = map (\(l, r) -> (substitute' l, substitute' r)) cs
unify s ((t, TVar n) : cs) | occurscheck n t = Nothing
unify s ((t, TVar n) : cs) = unify (M.insert n t s') cs' where
    substitute' = substitute (M.singleton n t)
    s' = M.map substitute' s
    cs' = map (\(l, r) -> (substitute' l, substitute' r)) cs
unify s ((TFun t1l t1r, TFun t2l t2r) : cs) =
    unify s ((t1l, t2l) : (t1r, t2r) : cs)

generalize :: Assump -> Type -> TypeScheme
generalize env t = (tvs, t) where
    tvs = tvars t \\ foldl ((. freevarsTs . snd) . union) [] env

instantiate' :: Int -> TypeScheme -> (Int, Type)
instantiate' n (tvs, t) = (n', substitute s t) where
    phi (n, s) v = (succ n, M.insert v (TVar n) s)
    (n', s) = foldl phi (n, M.empty) tvs

instantiate :: TypeScheme -> TM Type
instantiate ts = do
    n <- get
    let (n', t) = instantiate' n ts
    put n'
    return t

newtvar :: TM Type
newtvar = do
    t <- TVar <$> get
    modify succ
    return t

lookupAssump :: String -> Assump -> Maybe (TypeScheme, Judge)
lookupAssump _ [] = Nothing
lookupAssump x assump@((y, t) : _) | x == y =
    return (t, Judge "T-Env1" [] (PIn x t assump))
lookupAssump x assump@((y, _) : assump') = do
    (t, j) <- lookupAssump x assump'
    return (t, Judge "T-Env2" [j, TJudge (PNeq x y)] (PIn x t assump))

constraints :: Assump -> Term -> TM ([(Type, Type)], Type, Judge)
constraints env e@(Var x) = do
    (ts, j) <- lift $ lookupAssump x env
    t <- instantiate ts
    return ([], t, Judge "T-Var" [j] (PTypeRel env e t))
constraints env e@(App e1 e2) = do
    (c1, t1, j1) <- constraints env e1
    (c2, t2, j2) <- constraints env e2
    tn <- newtvar
    let c3 = (t1, TFun t2 tn) : c1 ++ c2
    return (c3, tn, Judge "T-App" [j1, j2] (PTypeRel env e tn))
constraints env e@(Abs x e1) = do
    tn <- newtvar
    (c1, t1, j1) <- constraints ((x, ([], tn)) : env) e1
    return (c1, TFun tn t1, Judge "T-Abs" [j1] (PTypeRel env e (TFun tn t1)))
constraints env e@(Let x e1 e2) = do
    (c1, t1, j1) <- constraints env e1
    s1 <- lift $ unify M.empty c1
    let t1' = substitute s1 t1
    (c2, t2, j2) <- constraints ((x, generalize env t1') : env) e2
    return (c1 ++ c2, t2, Judge "T-Let" [j1, j2] (PTypeRel env e t2))

-- parser

getResToken :: String -> Parser ()
getResToken str = string (BS.pack str) >> skipSpace

getLabel :: Parser String
getLabel = (BS.unpack <$> AP.takeWhile1 isLower) <* skipSpace

termParser :: Parser Term
termParser = skipSpace *> absParser <* endOfInput where
    absParser =
        liftM3 Let
            (getResToken "[" >> getLabel)
            (getResToken "=" >> absParser)
            (getResToken "]" >> absParser) <|>
        liftM2 (flip (foldr Abs))
            (getResToken "\\" >> many1 getLabel)
            (getResToken "." >> absParser) <|>
        appParser
    appParser =
        liftM2 (foldl App) varParser (many varParser)
    varParser =
        (Var <$> getLabel) <|> (getResToken "(" *> absParser <* getResToken ")")

-- pretty printer

termPpr :: Term -> String
termPpr = absPpr where
    unfoldAbs (Abs x e1) =
        let (xs, e2) = unfoldAbs e1 in (x : xs, e2)
    unfoldAbs e = ([], e)
    absPpr (Let x e1 e2) =
        "\\letterm{" ++ x ++ "}{" ++ absPpr e1 ++ "}{" ++ absPpr e2 ++ "}"
    absPpr e1 = case unfoldAbs e1 of
        ([], e2) -> appPpr e2
        (xs, e2) -> "\\lambda " ++ intercalate " \\, " xs ++ ". " ++ absPpr e2
    appPpr (App e1 e2) = appPpr e1 ++ " \\, " ++ varPpr e2
    appPpr e = varPpr e
    varPpr (Var x) = x
    varPpr e = "(" ++ absPpr e ++ ")"

tvarPpr :: M.Map Int String -> Int -> String
tvarPpr env n = fromMaybe ("?_" ++ show n) (M.lookup n env)

typePpr :: M.Map Int String -> Type -> String
typePpr env = funPpr where
    funPpr (TFun t1 t2) = varPpr t1 ++ " \\to " ++ funPpr t2
    funPpr t = varPpr t
    varPpr (TVar n) = tvarPpr env n
    varPpr t = "(" ++ funPpr t ++ ")"

tsPpr :: M.Map Int String -> TypeScheme -> String
tsPpr env ([], t) = typePpr env t
tsPpr env (l, t) =
    "\\forall " ++ intercalate " \\, " (map (tvarPpr env) l) ++ ". " ++ typePpr env t

assumpPpr :: M.Map Int String -> Assump -> String
assumpPpr env assump =
    "\\{" ++
    intercalate ", "
        (map (\(name, ts) -> name ++ " : " ++ tsPpr env ts) assump) ++
    "\\}"

propPpr :: M.Map Int String -> Prop -> String
propPpr env (PTypeRel assump term ty) =
    assumpPpr env assump ++ " \\vdash " ++ termPpr term ++ " : " ++ typePpr env ty
propPpr env (PIn name ts assump) =
    name ++ " : " ++ tsPpr env ts ++ " \\in " ++ assumpPpr env assump
propPpr env (PNeq name1 name2) = name1 ++ " \\neq " ++ name2

judgePpr :: [Int] -> Judge -> String
judgePpr tvs judge = judgePpr' tvsMap judge where
    idents = map ('\\' :)
        ["alpha", "beta", "gamma", "delta", "epsilon", "zeta", "eta", "theta",
        "iota", "kappa", "mu", "nu", "xi", "omicron", "pi", "rho",
        "sigma", "tau", "upsilon", "phi", "chi", "psi", "omega"]
    tvsMap = M.fromList $ zipWith (\n s -> (n, s)) tvs idents
    judgePpr' env (Judge rulename subjs prop) =
        "\\infere{" ++ rulename ++ "}{\n  " ++
        propPpr env prop ++ "\n}{\n" ++
        intercalate "  &\n"
            (map (unlines . map ("  " ++) . lines . judgePpr' env) subjs) ++
        "}\n"
    judgePpr' env (TJudge prop) = "(" ++ propPpr env prop ++ ")\n"

-- main

main :: IO ()
main = do
    input <- BS.getContents
    case parseOnly termParser input of
        Right term ->
            let result = do
                (cr, ty, judge) <- evalStateT (constraints [] term) 0
                s <- unify M.empty cr
                return (substitute s ty, substitute_judge s judge) in
            case result of
                Just (ty, judge) -> do
                    putStr (judgePpr (tvars ty) judge)
                    hFlush stdout
                Nothing -> putStrLn "Error: type inference failed.\n"
        Left err -> putStrLn err

