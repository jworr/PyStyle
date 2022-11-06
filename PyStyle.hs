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

import System.Environment (getArgs, getProgName)

import Style.Python (checkStyle)

{-Runs a tree-based ruleset on a python program to look for common design flaws -}
main :: IO ()
main = do 
       -- get the commandline args
       args <- getArgs
       name <- getProgName

       --if there is a file argument, treat it as a file and apply the rules
       --to it
       case args of
         fileName:[] -> readFile fileName 
                        >>= (\src -> putStrLn ("All Errors:\n" 
                                               ++ checkStyle src fileName))
         _    -> putStrLn ("Usage: " ++ name ++ " <python file>")

