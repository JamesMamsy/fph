{- 
Implement the following functions:
 
    ppStencilDef, 
    ppArgDecl,
    ppFSig, 
    ppLHSExpr, 
    ppRHSExpr 
    
Leave everything else as it is.
-}
module ASTEmitter (
    ppProgram ,
    ppBindings ,
    ppAST ,
    ppExprTup ,
    ppFSig ,
    ppArgDecl ,
    ppStencilDef ,
    ppMainTypeDecl ,
    ppMainArgDef ,
    ppMainReturnDef     
) where

import AST

import Data.List (intercalate)


ppProgram :: ASTInstance -> IO ()
ppProgram astInstance= let
        (instanceName,ast,functionSignaturesList,stencilDefinitionsList,mainArgDeclsList) = astInstance
        (mainArgDeclsInArgs,mainArgDeclsOutArgs) = mainArgDeclsList
        stencilDefs = map ppStencilDef stencilDefinitionsList
        inArgDecls = map ppArgDecl mainArgDeclsInArgs
        outArgDecls = map ppArgDecl mainArgDeclsOutArgs
        -- inArgDeclTypes = ppArgs ppArgDeclType mainArgDeclsInArgs
        -- outArgDeclTypes = ppArgs ppArgDeclType mainArgDeclsOutArgs
        functionDecls = map ppFSig functionSignaturesList
        mainTypeDecl = ppMainTypeDecl mainArgDeclsList
        mainArgDef = ppMainArgDef mainArgDeclsList
        mainReturnDef = ppMainReturnDef mainArgDeclsList
        mainExprs = map ("    "++) (ppAST ast)
        mainDef = [mainArgDef]++mainExprs++[mainReturnDef]
    in
        mapM_ putStrLn (
            ["-- "++instanceName++"\n"]++
            ["-- Stencil definitions"]++
            stencilDefs++
            ["\n-- Argument type declarations"]++
            ["---- Input arguments"]++
            inArgDecls++
            ["---- Output arguments"]++
            outArgDecls++
            ["\n-- Function type declarations"]++
            functionDecls++
            ["\n-- Main function type declaration"]++
            [mainTypeDecl]++
            ["\n-- Main function definition"]++
            mainDef
            )

ppBindings :: AST -> String
ppBindings = unlines . ppAST

--Takes in the AST variable and provides a printed format using the Expression printer and expressions in the form of tuples
ppAST :: AST -> [String]
ppAST = map ppExprTup 

--Pretty printer for given expression Tuples
ppExprTup :: (Expr, Expr) -> String
ppExprTup (lhs,rhs) = ppLHSExpr lhs ++ " = " ++ ppRHSExpr rhs

--TODO Pretty print for Left hand expressions -Partially Implemented
ppLHSExpr  :: Expr -> String
ppLHSExpr Vec VE Expr = ppLHSExpr Expr
ppLHSExpr Scalar VE DType Name = show Name
ppLHSExpr SVec Size Expr = ppLHSExpr Expr
ppLHSExpr = show
--TODO Pretty print for Right hand expressions -Partially Implemented
ppRHSExpr :: Expr -> String
ppRHSExpr Vec VE Expr = ppRHSExpr Expr
ppRHSExpr Scalar VE DType Name = show Name
ppRHSExpr = show

-- TODO Pretty-printer for the function signatures
ppFSig :: FunctionSignature -> String
ppFSig = show 

-- Pretty-printer for the argument data types
ppDType :: DType -> String
ppDType DInteger = "Int"
ppDType DInt = "Int"
ppDType DReal = "Float"
ppDType DFloat = "Float"
ppDType (DSVec sz dt) = "SVec "++ show sz ++" "++ ppDType dt
ppDType (DVec sz dt) = "Vec "++ show sz ++" "++ ppDType dt
ppDType (DFVec dims dt) = "FVec "++ show dims ++" "++ ppDType dt
ppDType (DTuple dts) = "("++  intercalate ", " (map ppDType dts) ++")"
ppDType DDC = show DDC

--TODO? Pretty Printer for Argument Declaration
ppArgDecl :: (String, DType) -> String
ppArgDecl (name , this_type ) = name ++ " :: " ++ ppDType this_type

ppArgDeclType :: (String, DType) -> String
ppArgDeclType (_,argType) = ppDType argType

ppArgName  :: (String, DType) -> String
ppArgName (argName,_) = argName

ppArgs pp argDecls
    | length argDecls == 1 = pp (head argDecls)
    | otherwise = "("++ intercalate "," (map pp argDecls) ++")"

--TODO? Pretty-printer for stencil definitions 
ppStencilDef :: StencilDefinition -> String
ppStencilDef (sname,sdef) = sname ++ " = "++ show sdef

ppMainTypeDecl :: ([(String,DType)],[(String,DType)]) -> String
ppMainTypeDecl mainArgDeclsList_ = let
        (mainArgDeclsInArgs,mainArgDeclsOutArgs) = mainArgDeclsList_
        inArgDeclTypes = ppArgs ppArgDeclType mainArgDeclsInArgs
        outArgDeclTypes = ppArgs ppArgDeclType mainArgDeclsOutArgs
    in
        "main :: " ++ inArgDeclTypes ++ " -> " ++ outArgDeclTypes

ppMainArgDef :: ([(String,DType)],[(String,DType)]) -> String
ppMainArgDef (mainArgDeclsInArgs,mainArgDeclsOutArgs) = "main " ++ ppArgs ppArgName mainArgDeclsInArgs++" = let "

ppMainReturnDef :: ([(String,DType)],[(String,DType)]) -> String
ppMainReturnDef (mainArgDeclsInArgs,mainArgDeclsOutArgs) = "  in\n      " ++ ppArgs ppArgName mainArgDeclsOutArgs