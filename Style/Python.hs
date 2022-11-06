{-
   Copyright 2022 J. Walker Orr

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
-}

module Style.Python (checkStyle) where

import qualified Language.Python.Version3.Lexer as PL
import qualified Language.Python.Version3.Parser as PP
import qualified Language.Python.Common.SrcLocation as LS

import Language.Python.Common.Token
import Language.Python.Common.AST
import Text.Printf (printf)
import Data.List
import Data.Maybe
import Data.Char

import Style.Duplicate (findSelectDuplicates)
import Style.Generic (linesTooLong, stripIndents)

mainName = "main"
similarThreshold = 0.9
maxStatements = 20
maxIndents = 5
tooShort = 2
varNameWhiteList = ["i", "j", "k"]

--a record of an individual mistake
data Mistake = TokenMistake String Int 
             | StructureMistake String 
             | DuplicationMistake Int Int
             | LineMistake String Int
             | SourceMistake String
                       

{- Checks the python program for various common mistakes based on its
parse tree and tokenziation -}
checkStyle :: String -> String -> String
checkStyle src name = 
   let
      tokens = PL.lex src name
      ast    = PP.parseModule src name
   in
      case runChecks src <$> tokens <*> ast of
         Left error     -> show error
         Right mistakes -> unlines $ map show mistakes
      
{- Run all the checks for style and design mistakes -}
runChecks :: String -> [Token] -> (ModuleSpan, [Token]) -> [Mistake]
runChecks source tokens (tree, _) = concat [tokenErrors tokens,
                                            structureErrors tree,
                                            sourceErrors tree source]

{-pretty print the mistake-}
instance Show Mistake where

   show (TokenMistake name line)   = printf "%s on line %d" name line
   show (StructureMistake name)    = name
   show (DuplicationMistake line other) = printf "Line %d is possible duplicate of to %d" line other
   show (LineMistake text line)    = printf "Line %d%s" line text
   show (SourceMistake text)       = text

{----------------------------Token Checks -----------------------------------}

{- Finds all the errors that are individual tokens -}
tokenErrors :: [Token] -> [Mistake]
tokenErrors = mapMaybe checkToken 

{- Checks if the token is a mistake -}
checkToken :: Token -> Maybe Mistake
checkToken token = do
    errorText <- badToken token
    return $ TokenMistake errorText (LS.startRow $ LS.getSpan token)

{- Returns the name of the mistake associated with the bad token -}
badToken :: Token -> Maybe String
badToken (GlobalToken t)   = Just "Global Variable"
badToken (ContinueToken t) = Just "Continue Statement"
badToken (BreakToken t)    = Just "Break Statement"
badToken (PassToken t)     = Just "Pass Statement"
badToken _                 = Nothing

{----------------------------Raw Source code checks--------------------------}

{- checks the raw source code for various error -}
sourceErrors :: ModuleSpan -> String -> [Mistake]
sourceErrors (Module stmts) source = 
   let
      checks = [duplicateLines, commentsWithOutSpace, checkLineLength, 
                checkSpaceIndent, checkMaxIndents, enoughComments stmts,
                checkDocStrs]
      code = lines source
   in
      concat $ checks <*> [code]

{- Finds duplicated lines of code -}
duplicateLines :: [String] -> [Mistake]
duplicateLines = 
   let
      isBody line = isCode line && not (isFunDef line)
   in
      map (uncurry DuplicationMistake) 
      . findSelectDuplicates isBody similarThreshold

{- Finds comments without proper spacing -}
commentsWithOutSpace :: [String] -> [Mistake]
commentsWithOutSpace = map (LineMistake ", comment needs a space after the #") 
                     . findIndices (\l -> isComment l && improperComment l) 

{- Determines if enough comments are used -}
enoughComments :: [Statement a] -> [String] -> [Mistake]
enoughComments stmts source =
   let
      --check if a statement is a function, loop, or conditional
      important s  = or [f s | f <- [isFun, isLoop, isIfStmt]] 
      needComments = length . filter important $ allDescendants stmts
      numComments  = length $ filter isComment source
      msg = printf "Need at least %d in-line comments but found %d" needComments numComments
   in
      [SourceMistake msg | needComments > numComments]

{- Finds the lines that are too long -}
checkLineLength :: [String] -> [Mistake]
checkLineLength = map (LineMistake " too long") . linesTooLong

{- Determines if the lines are idented with spaces -}
checkSpaceIndent :: [String] -> [Mistake]
checkSpaceIndent source = 
   let
      badLines = findIndices (not . null . takeWhile isSpaceChar) source
      strLines = intercalate ", " $ map show badLines
      msg = printf "Lines %s indented with spaces, use tabs" strLines
   in 
      [SourceMistake msg | not $ null badLines]

{- Checks if any lines are beyond maximum identation level -}
checkMaxIndents :: [String] -> [Mistake]
checkMaxIndents = map (LineMistake " too many indents, simplify logic") 
                . findIndices (\l -> length (takeWhile isTabChar l) > maxIndents)

{- Checks that the line following a function declaration starts a docstring -}
checkDocStrs :: [String] -> [Mistake]
checkDocStrs [] = []
checkDocStrs lines =
   let
      noDocStr (line, next) = (isFunDef line) && (not $ isDocStrStart next)
   in
      map (\l -> LineMistake " function definition needs docstring comment" (l + 1)) $
      findIndices noDocStr (zip lines (tail lines))

{- Determines if a line is source code or comment -}
isCode :: String -> Bool
isCode = not . isComment

{- Determines if the line is a comment line -}
isComment :: String -> Bool
isComment = isPrefixOf "#" . stripIndents

{- Determines if the comment is not proper -}
improperComment :: String -> Bool
improperComment = not . isPrefixOf "# " . stripIndents

{- Determines if the line starts a docstring -}
isDocStrStart :: String -> Bool
isDocStrStart = isPrefixOf "\"\"\"" . stripIndents

{- Determine if the line has a function declaraction -}
isFunDef :: String -> Bool
isFunDef = isPrefixOf "def " . stripIndents

{----------------------------Structure Checks--------------------------------}

{- Checks for structural errors -}
structureErrors :: ModuleSpan -> [Mistake]
structureErrors (Module stmts) = 
   let
      --all the structural checks
      --checks all the function definitions e.g. position of functions
      allFunChecks = [hasMain, mainFirst, others]

      --checks individual functions
      singleFunChecks = [nestedFun, nestedRet, multiRet, magicNumbers, 
                         recMain, hasRec, mainHasArgs, incCheck, funTooLong,
                         checkStrConv, checkNumConv, checkWhileTrue,
                         checkFunName, checkVarName]

      --checks the top-level statments of the program
      stmtChecks = [callMain, callQuit, importFirst]

      --functions w/ bodies of code
      funStmts   = filter isFun (allDescendants stmts)

      --look for mistakes
      allFun     = allFunChecks <*> [funStmts]
      singleFun  = singleFunChecks <*> funStmts 
      allStmts   = stmtChecks <*> [stmts]
   in
      catMaybes $ concat [allFun, singleFun, allStmts]
       

{- Makes a mistake based the function name -}
funMistake :: Bool -> Ident a -> String -> Maybe Mistake
funMistake hasMistake name text 
   | hasMistake = Just $ StructureMistake (ident_string name ++ text)
   | otherwise  = Nothing

{-----------------------------Main/Top Level Checks--------------------------}
{- Check for a "Main" function -}
hasMain :: [Statement a] -> Maybe Mistake
hasMain stmts = case find isMain stmts of
                Just _ -> Nothing
                _      -> Just $ StructureMistake "Missing main function"


{- Check that the last statement is a call to main -}
callMain :: [Statement a] -> Maybe Mistake
callMain []    = Nothing
callMain stmts 
   | mainIsLast = Nothing
   | otherwise  = Just $ StructureMistake "Last line of program should call main function"
   where lastStmt   = last stmts
         mainIsLast = isMainCallStmt lastStmt


{- Check that the main functions is first -}
mainFirst :: [Statement a] -> Maybe Mistake
mainFirst stmts = case filter isFun stmts of
                  f:_ -> if not $ isMain f then msg else Nothing
                  _    -> Nothing
      
   where msg = Just $ StructureMistake "Main function should be first"


{- Checks that main has no arguments -}
mainHasArgs :: Statement a -> Maybe Mistake
mainHasArgs (Fun name params _ _ _) =
   let
      isMainFun = ident_string name == mainName
      hasArgs   = isMainFun && not (null params)
   in
      funMistake hasArgs name " should have no parameters"

mainHasArgs _ = Nothing

{- Checks that imports come first for all other statements-}
importFirst :: [Statement a] -> Maybe Mistake
importFirst stmts
   | lateImport = Just $ StructureMistake "Imports should come before all other lines of code"
   | otherwise = Nothing
   where
      rest = dropWhile isImport stmts
      lateImport = any isImport (allDescendants rest)


{- Check that there is another function besides main -}
others :: [Statement a] -> Maybe Mistake
others stmts 
   | num > 0   = Nothing
   | otherwise = Just $ StructureMistake "Need another function besides Main"
   where 
      otherFun = filter (\f -> isFun f && not (isMain f)) stmts
      num = length otherFun


{-----------------------------Function Checks-------------------------------}


{- determines if the function has nested function -}
nestedFun :: Statement a -> Maybe Mistake
nestedFun (Fun name _ _ children _) =
   let
      --count the functions that are nested under this one
      nestedStmts = allDescendants children
      count       = length $ filter isFun nestedStmts
      msg         = printf " has %d nested functions" count
   in
      funMistake (count > 0) name msg
      
nestedFun _ = Nothing


{- determines if a function is too long -}
funTooLong :: Statement a -> Maybe Mistake
funTooLong (Fun name _ _ children _) =
   let
      count = length $ allDescendants children
      msg = printf " has %d statements, limit per function is %d" count maxStatements
   in
      funMistake (count > maxStatements) name msg

funTooLong _ = Nothing


{- Check for returns inside of loops and ifs -}
nestedRet :: Statement a -> Maybe Mistake
nestedRet (Fun name _ _ children _) =
   let
      --get the children that are not returns, look through them for returns
      childNotRet = filter (not . isFunOrRet) children
      nestedStmts = concatMap descendants childNotRet
      hasNested   = any isRet nestedStmts
   in
      funMistake hasNested name " has nested return statements"
      
nestedRet _ = Nothing


{- Checks if a function has multiple returns -}
multiRet :: Statement a -> Maybe Mistake
multiRet (Fun name _ _ children _) =
   let
      nonFunChildren = filter (not . isFun) children
      allDesc        = allDescendants nonFunChildren
      returnCount    = length $ filter isRet allDesc
      msg            = printf " has multiple return statements (%d)" returnCount
   in
      funMistake (returnCount > 1) name msg
      
multiRet _ = Nothing


{- Checks if the main function is called inside another function (recursive program)-}
recMain :: Statement a -> Maybe Mistake
recMain (Fun name _ _ children _) =
   let
      calledMain = any isMainCall (allExpressions children)
   in
      funMistake calledMain name " is calling main recursively"

recMain _ = Nothing


{- Checks if the function recursively calls itself -}
hasRec :: Statement a -> Maybe Mistake
hasRec (Fun name _ _ children _) =
   let
      funName = ident_string name
      exprs   = allExpressions children
      recCall = any (isCallWithName funName) exprs
   in
      funMistake recCall name " recursively calls itself"

hasRec _ = Nothing

{- Checks a function for a for-loop with manually incremented variables -}
incCheck :: Statement a -> Maybe Mistake
incCheck (Fun name _ _ children _) =
   let
      incremented = concatMap loopInc children
      vars        = intercalate ", " incremented
      msg         = " has needlessly incremented for-loop variable(s): " ++ vars
      hasInc      = not . null $ incremented
   in
      funMistake hasInc name msg
      
{- Returns all the variables names that were incremented inside a for-loop -}
loopInc :: Statement a -> [String]
loopInc (For vars _ body _ _) =
   let
      loopVars     = mapMaybe varName vars
      assignedVars = (concatMap assignedTo . allDescendants) body
   in
      filter (`elem` loopVars) assignedVars
      
loopInc _ = []

{- Checks a function for unnecessary string conversions -}
checkStrConv :: Statement a -> Maybe Mistake
checkStrConv (Fun name _ _ children _) =
   let
      badStr = any checkStrCall (allExpressions children)
      msg = " has unnecessary string conversion e.g. str(input()) just use input()"
   in
      funMistake badStr name msg

checkStrConv _ = Nothing


{- checks if a function call is like "str(input())" -}
checkStrCall :: Expr a -> Bool
checkStrCall (Call name args _) =
   let
      argExp = map expandArg args
   in
      case varName name of
         Just callName -> callName == "str" && any (isCallWithName "input") argExp
         _             -> False

checkStrCall _ = False


{- Checks a function for uncessesary numeric conversions -}
checkNumConv :: Statement a -> Maybe Mistake
checkNumConv (Fun name _ _ children _) = 
   let
      badLit = any checkNumCall (allExpressions children)
      msg    = " has unnecesssary int() or float() conversion e.g. int(10)"
   in
      funMistake badLit name msg

{- checks the use of int() and float() to make sure that they are necessary -}
checkNumCall :: Expr a -> Bool
checkNumCall (Call name args _) =
   let
      argExp = map expandArg args
   in
      case varName name of
         Just callName -> (callName == "int" || callName == "float")
                          && (any isNumLit argExp)

         _             -> False

checkNumCall _ = False

{- checks for "while true" while loops -}
checkWhileTrue :: Statement a -> Maybe Mistake
checkWhileTrue (Fun name _ _ children _) =
   let
      hasWhileTrue = any isWhileTrue (allDescendants children)
      msg = " has bad loop \"While True\""
   in
      funMistake hasWhileTrue name msg  
   
{- determines if the statement is "while true/false" -}
isWhileTrue :: Statement a -> Bool
isWhileTrue (While (Bool {}) _ _ _) = True
isWhileTrue _                       = False

{- checks if a function name violates snake case -}
checkFunName :: Statement a -> Maybe Mistake
checkFunName (Fun name _ _ _ _) = 
   let
      err = hasCapitals (ident_string name)
      msg = ", function name not in snake case"
   in
      funMistake err name msg

{- checks if variables/arguments violate snake case -}
checkVarName :: Statement a -> Maybe Mistake
checkVarName (Fun name _ _ children _) =
   
   let 
      varNames  = mapMaybe varName $ allExpressions children
      camelCase = filter (not . isShoutCase) $ filter hasCapitals varNames
      short     = filter (\n -> length n <= tooShort) $ filter (`notElem` varNameWhiteList) varNames
      badNames  = nub (camelCase ++ short)

      msg = " has variables that do not conform to snake case or are too short: " ++ intercalate ", " badNames
   in
     funMistake (not $ null badNames) name msg

{- determines if the string (var or function name) has capital letters -}
hasCapitals :: String -> Bool
hasCapitals = any isUpper

{- determines if the string is in shout case -}
isShoutCase :: String -> Bool
isShoutCase = all (\c -> isUpper c || c == '_' || isDigit c)

{----------------------------------Expression Checks-------------------------}

{- Looks for calls to quit or exit -}
callQuit :: [Statement a] -> Maybe Mistake
callQuit stmts 
   | hasQuit    = Just $ StructureMistake "Do not use quit() or exit()"
   | otherwise  = Nothing
   where
      exprs   = allExpressions stmts
      hasQuit = any (\e -> isCallWithName "quit" e
                || isCallWithName "exit" e) exprs


{----------------------------------Magic numbers!----------------------------}

{- Check for magic numbers in statements and builds -}
magicNumbers :: Statement a -> Maybe Mistake
magicNumbers (Fun name _ _ children _) =
   let
      exprs = allExpressions children
      count = sum $ map magicExpr exprs
   in
      funMistake (count > 0) name (printf " has %d magic numbers" count)

{- Checks an expression for magic numbers -}
magicExpr :: Expr a -> Int
magicExpr (Int _ lit _)     = magicLit lit
magicExpr (Float _ lit _)   = magicLit lit
magicExpr (Strings strs _)  = strMagicLit $ concat strs
magicExpr _                 = 0


{- Check if the int/float literal is magic -}
magicLit :: String -> Int
magicLit lit 
   | notMagic  = 0
   | otherwise = 1
   where notMagic = lit `elem` ["-1", "0", "1", "100", "0.0", "-1.0", "1.0", "100.0"]

{- Check if the string literal is just digits i.e. "magic" -}
strMagicLit :: String -> Int
strMagicLit lit 
   | allDigits = 1
   | otherwise = 0
   where 
      noQuotes  = filter (`notElem` ['\'', '"']) lit
      allDigits = all isDigit noQuotes && not (null noQuotes)

{---------------------------Utility Functions------------------------------}
{- Returns all the descendants of statements given -}
allDescendants :: [Statement a] -> [Statement a]
allDescendants stmts = stmts ++ concatMap descendants stmts

{- Returns the descendants of the statement -}
descendants :: Statement a -> [Statement a]
descendants stmt = 
   let
      children = getChildren stmt
   in
      children ++ concatMap descendants children

{- Returns the children statements of the statement -}
getChildren :: Statement a -> [Statement a]
getChildren (While _ b e _)     = b ++ e
getChildren (For _ _ b e _)     = b ++ e
getChildren (Fun _ _ _ b _)     = b
getChildren (Class _ _ b _)     = b
getChildren (Conditional g e _) = concatMap snd g ++ e
getChildren (Try b _ e f _)     = concat [b, e, f]
getChildren (With _ b _)        = b
getChildren _                   = []

{- Returns all the expressions in given statements and their descendants -}
allExpressions :: [Statement a] -> [Expr a]
allExpressions stmts = 
   let
      allDesc = allDescendants stmts
      exprs   = concatMap stmtExprs allDesc
   in
      concatMap expandExpr exprs

{- Expands the expressions that are nested within other expressions -}
expandExpr :: Expr a -> [Expr a]
expandExpr expr = expr : concatMap expandExpr (childExpr expr)

--TODO left off some more "exotic" expressions like comprehensions
{- Looks up the child expression for the given expresion -}
childExpr :: Expr a -> [Expr a]
childExpr (Call expr args _)           = expr : map expandArg args
childExpr (Subscript var expr _)       = [var, expr]
childExpr (CondExpr test left right _) = [test, left, right]
childExpr (BinaryOp _ left right _)    = [left, right]
childExpr (UnaryOp _ expr _)           = [expr]
childExpr (Dot expr _ _)               = [expr]
childExpr (Tuple exprs _)              = exprs
childExpr (List exprs _)               = exprs
childExpr (Set exprs _)                = exprs
childExpr (Paren expr _)               = [expr]
childExpr _ = []

expandArg :: Argument a -> Expr a
expandArg (ArgExpr expr _)           = expr
expandArg (ArgVarArgsPos expr _ )    = expr
expandArg (ArgVarArgsKeyword expr _) = expr
expandArg (ArgKeyword _ expr _)      = expr

{- returns all the expressions in the statement -}
stmtExprs :: Statement a -> [Expr a]
stmtExprs (While cond _ _ _)            = [cond]
stmtExprs (For vars gen _ _ _)          = gen : vars 
stmtExprs (Fun _ _ opt _ _)             = maybeToList opt
stmtExprs (Conditional guards _ _)      = map fst guards
stmtExprs (Assign lhs rhs _)            = rhs : lhs 
stmtExprs (AugmentedAssign lhs _ rhs _) = [lhs, rhs]
stmtExprs (Return expr _)               = maybeToList expr
stmtExprs (With ctx _ _)                = concatMap (\c -> fst c : maybeToList (snd c)) ctx
stmtExprs (StmtExpr expr _)             = [expr]
stmtExprs _                             = []

{- Determines if the statement is the declaration of the main function -}
isMain :: Statement a -> Bool
isMain (Fun name _ _ _ _) = ident_string name == mainName
isMain _                  = False

{- Determines if the statment is a function declaration -}
isFun :: Statement a -> Bool
isFun Fun {} = True
isFun _      = False

{- Determines if the statement is a loop -}
isLoop :: Statement a -> Bool
isLoop While {} = True
isLoop For {}   = True
isLoop _        = False

{- Determines if the statement is a conditional i.e. "if" -}
isIfStmt :: Statement a -> Bool
isIfStmt Conditional {} = True
isIfStmt _              = False

{- Determines if the statement is an import -}
isImport :: Statement a -> Bool
isImport Import {}     = True
isImport FromImport {} = True
isImport _             = False

{- Determines if the statement is a return statement -}
isRet :: Statement a -> Bool
isRet (Return _ _) = True
isRet _            = False

isFunOrRet :: Statement a -> Bool
isFunOrRet s = isFun s || isRet s

{- Determines if the expression is a numeric literal -}
isNumLit ::  Expr a -> Bool
isNumLit Int {}   = True
isNumLit Float {} = True
isNumLit _        = False

{- Determines if the expression is a boolean literal -}
isBoolLit :: Expr a -> Bool
isBoolLit Bool {} = True
isBoolLit _       = False

{- Checks if the statement is a function call has the given name-}
isCallWithName :: String -> Expr a -> Bool
isCallWithName name (Call funName _ _) = case varName funName of
                                             Just var -> var == name
                                             Nothing  -> False

isCallWithName _ _                     = False

{- Checks if the statement is a call to main -}
isMainCallStmt :: Statement a -> Bool
isMainCallStmt (StmtExpr expr _) = isMainCall expr
isMainCallStmt _                 = False

{- determines if a function call to "main" -}
isMainCall :: Expr a -> Bool
isMainCall = isCallWithName mainName

{- Gets the name of the function call -}
varName :: Expr a -> Maybe String
varName (Var name _) = Just $ ident_string name
varName _            = Nothing

{- determines if the statement is an assignment or augmented assignment e.g. += -}
isAssignment :: Statement a -> Bool
isAssignment Assign {}          = True
isAssignment AugmentedAssign {} = True
isAssignment _                  = False

{- returns a list of variables being assigned to in the statement -}
assignedTo :: Statement a -> [String]
assignedTo (Assign vars _ _)             = mapMaybe varName vars
assignedTo (AugmentedAssign var _ _ _)   = mapMaybe varName [var]
assignedTo _                             = []

{- Determines if the character is a space -}
isSpaceChar :: Char -> Bool
isSpaceChar ' ' = True
isSpaceChar _   = False

{- Determines if the character is a tab -}
isTabChar :: Char -> Bool
isTabChar '\t' = True
isTabChar _    = False
