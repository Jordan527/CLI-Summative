{-
REQUIREMENTS
• Your program will be a CLI application
• Your program should have the ability to read a file format of your choice, which holds the initial modules and enrolled students   ********* DONE *********
• Your program should have the ability to search for student details					********* DONE *********
• Your program should have the ability to add new students and modules                  
• Your program should be able to output reports to a file (e.g., Students enrolled)
-}

{- 
DO NEXT
• load modules from file
-} 


import System.IO
import System.Directory
import qualified Data.Map as M
import Data.Char
import Data.List.Split
import Data.List
import Data.Maybe
import Test.HUnit

data Student = Student {
               sID :: String,
               forename :: String, 
               surname :: String, 
               course :: String, 
               year :: String} deriving (Eq, Show)

data Module = Module {
              mID :: String,
              name :: String,
              studentIDs :: [String]} deriving (Eq, Show)


data Students = Students {sDict::(M.Map String Student)} deriving (Show)
studentDict :: M.Map String Student
studentDict = M.empty

data Modules = Modules {mDict::(M.Map String Module)} deriving (Show)
moduleDict :: M.Map String Module
moduleDict = M.empty


main :: IO ()
main = do deleteFiles'
          readFiles'
          studentDict <- loadStudents
          moduleDict <- loadModules
          let students = Students studentDict
          let modules = Modules moduleDict
          putStrLn ""
          putStrLn "\nWhat do you want to do?"
          putStrLn "1. Search for student details"
          putStrLn "2. Search for module details"
          putStrLn "3. Quit"
          choice <- getLine
          menuPath choice students modules

invalidMain :: Students -> Modules -> IO ()
invalidMain studentDict moduleDict = do putStrLn "\nThat is not a valid option!"
                                        putStrLn "1. Search for student details"
                                        putStrLn "2. Search for module details"
                                        putStrLn "3. Quit"
                                        choice <- getLine
                                        menuPath choice studentDict moduleDict

menuPath :: String -> Students -> Modules -> IO()
menuPath a studentDict moduleDict
         | a == "1" = searchStudent studentDict
         | a == "2" = searchModule moduleDict
         | a == "3" = return ()
         | otherwise = invalidMain studentDict moduleDict




{- IMPORT DATA -}
deleteFiles' :: IO ()
deleteFiles' = do studentFile <- doesFileExist "Students.txt"
                  if studentFile then removeFile "Students.txt" else print "Students.txt does not exist"
                  moduleFile <- doesFileExist "Modules.txt"
                  if moduleFile then removeFile "Modules.txt" else print "Modules.txt does not exist"

readFiles' :: IO ()
readFiles' = do inputHandle <- openFile "Data.txt" ReadMode
                seperateFiles inputHandle
                hClose inputHandle
                return ()
                                     
seperateFiles :: Handle -> IO ()
seperateFiles inputHandle = do ineof <- hIsEOF inputHandle
                               if ineof
                               then return () 
                               else do outputString <- hGetLine inputHandle
                                       let line = splitOn "," outputString
                                           path = line !! 0
                                       if path == "Students.txt" 
                                       then do seperateFiles inputHandle
                                               let id = line !! 1
                                                   forename = line !! 2
                                                   surname= line !! 3
                                                   course = line !! 4
                                                   year = line !! 5
                                                   output = id ++ "," ++ forename ++ "," ++ surname ++ "," ++ course ++ "," ++ year ++ "," ++ "\n"
                                               appendFile "Students.txt" output
                                               return () 
                                       else do seperateFiles inputHandle
                                               let id = line !! 1
                                                   name = line !! 2
                                                   students = drop 3 line
                                                   output = id ++ "," ++ name ++ "," ++ (intercalate "," students) ++ "," ++ "\n"
                                               appendFile "Modules.txt" output
                                               return () 
loadStudents :: IO (M.Map [Char] Student)
loadStudents = do inputHandle <- openFile "Students.txt" ReadMode
                  do dict <- readStudents inputHandle 
                     hClose inputHandle
                     return dict

readStudents :: Handle -> IO (M.Map [Char] Student)
readStudents inputHandle = do ineof <- hIsEOF inputHandle
                              let dict = studentDict
                              if ineof
                              then return dict 
                              else do outputString <- hGetLine inputHandle
                                      let line = splitOn "," outputString
                                          id = line !! 0
                                          forename = line !! 1
                                          surname= line !! 2
                                          course = line !! 3
                                          year = line !! 4
                                          student = Student id forename surname course year
                                          emptyDict = studentDict
                                          dict = M.insert id student emptyDict 
                                      do newDict <- readStudents inputHandle
                                         let dict = M.insert id student newDict 
                                         return dict 

loadModules :: IO (M.Map [Char] Module)
loadModules = do inputHandle <- openFile "Modules.txt" ReadMode
                 do dict <- readModules inputHandle 
                    hClose inputHandle
                    return dict
                     
readModules :: Handle -> IO (M.Map [Char] Module)
readModules inputHandle = do ineof <- hIsEOF inputHandle
                             let dict = moduleDict
                             if ineof
                             then return dict 
                             else do outputString <- hGetLine inputHandle
                                     let line = splitOn "," outputString
                                         id = line !! 0
                                         name = line !! 1
                                         students = pop (drop 2 line)
                                         module' = Module id name students
                                         emptyDict = moduleDict
                                         dict = M.insert id module' emptyDict 
                                     do newDict <- readModules inputHandle
                                        let dict = M.insert id module' newDict 
                                        return dict 
                                     

                               


{- STUDENT SEARCHING -}
searchStudent:: Students -> IO ()
searchStudent studentDict = do putStrLn ""
                               putStrLn "How would you like to search for the student?"
                               putStrLn "1. ID" 
                               putStrLn "2. Full name" 
                               putStrLn "Else: Menu" 
                               input <- getLine
                               let name = input
                               studentSearchPath input studentDict

studentSearchPath :: String -> Students -> IO ()
studentSearchPath a studentDict
           | a == "1" = studentSearchID studentDict
           | a == "2" = studentSearchName studentDict
           | otherwise = main

studentSearchID :: Students -> IO ()
studentSearchID studentDict = do putStrLn ""
                                 putStrLn "Enter a student ID"
                                 studentID <- getLine
                                 let student = M.lookup studentID (sDict studentDict)
                                 putStrLn ""
                                 let student' = removeMaybe student
                                 if student' == []
                                 then putStrLn "Student not found"
                                 else printStudent (head student')
                                 main

studentSearchName :: Students -> IO ()
studentSearchName studentDict = do putStrLn ""
                                   putStrLn "Enter a student name"
                                   studentName <- getLine
                                   let studentLists = M.toList (sDict studentDict)
                                   do studentID <- studentNameIteration studentName studentLists
                                      let student = M.lookup studentID (sDict studentDict)
                                      putStrLn ""
                                      let student' = removeMaybe student
                                      if student' == []
                                      then putStrLn "Student not found"
                                      else printStudent (head student')
                                      main

studentNameIteration :: String -> [(String, Student)] -> IO String
studentNameIteration _ [] = return " "
studentNameIteration name (x:xs) = do let student = snd x
                                      do studentName <- createName (forename student, surname student)
                                         if lower studentName == lower name
                                         then return (fst x)
                                         else studentNameIteration name xs


{- MODULE SEARCHING -}
searchModule :: Modules -> IO ()
searchModule moduleDict = do putStrLn ""
                             putStrLn "How would you like to search for the module?"
                             putStrLn "1. ID" 
                             putStrLn "2. course" 
                             putStrLn "Else: Menu" 
                             input <- getLine
                             let name = input
                             moduleSearchPath input moduleDict

moduleSearchPath :: String -> Modules -> IO ()
moduleSearchPath a moduleDict
           | a == "1" = moduleSearchID moduleDict
           | a == "2" = moduleSearchName moduleDict
           | otherwise = main
           
moduleSearchID :: Modules -> IO ()
moduleSearchID moduleDict = do putStrLn ""
                               putStrLn "Enter a module ID"
                               moduleID <- getLine
                               let module' = M.lookup moduleID (mDict moduleDict)
                               putStrLn ""
                               let module'' = removeMaybe module'
                               if module'' == []
                               then putStrLn "Module not found"
                               else printModule (head module'')
                               main
                               
moduleSearchName :: Modules -> IO ()
moduleSearchName moduleDict = do putStrLn ""
                                 putStrLn "Enter a module name"
                                 moduleName <- getLine
                                 let moduleLists = M.toList (mDict moduleDict)
                                 do moduleID <- moduleNameIteration moduleName moduleLists
                                    let module' = M.lookup moduleID (mDict moduleDict)
                                    putStrLn ""
                                    let module'' = removeMaybe module'
                                    if module'' == []
                                    then putStrLn "Module not found"
                                    else printModule (head module'')
                                    main

moduleNameIteration :: String -> [(String, Module)] -> IO String
moduleNameIteration _ [] = return " "
moduleNameIteration course (x:xs) = do let module' = snd x
                                       if lower course == lower (name module')
                                       then return (fst x)
                                       else moduleNameIteration course xs                                      


           
{- FORMAT FUNCTIONS -}
printStudent :: Student -> IO ()
printStudent student = do putStrLn ("ID: " ++ (sID student))
                          putStrLn ("Forename: " ++ (forename student))
                          putStrLn ("Surname: " ++ (surname student))
                          putStrLn ("Course: " ++ (course student))
                          putStrLn ("Year: " ++ (year student))
                          return ()
                         
printModule :: Module -> IO ()
printModule module' = do putStrLn ("ID: " ++ (mID module'))
                         putStrLn ("Course: " ++ (name module'))
                         putStrLn ("Students ID's: " ++ (intercalate "," (studentIDs module')))
                         return ()

pop :: [a] -> [a]
pop list = take ((length list) - 1) list

createName :: (String, String) -> IO String
createName (f, s) = return (f ++ " " ++ s)

lower :: String -> String
lower xs = [toLower x | x <- xs]

removeMaybe :: Maybe a -> [a]
removeMaybe Nothing = []
removeMaybe (Just a) = [a]



{- 
***** TESTS ***** 
runTestTT tests
-}

stringList :: [Int] -> [String]
stringList x = map show x

test1 = TestCase (assertEqual "pop [1..5] Int" [1..5] (pop [1..6]))
test2 = TestCase (assertEqual "pop [1..1000] Int" [1..999] (pop [1..1000]))
test3 = TestCase (assertEqual "pop [] Int" [] (pop [1]))
{-
test4 = TestCase (do list <- stringList [1..5]
                     assertEqual "pop [1..5] String" 5 (pop list))
-}                     

tests = TestList [TestLabel "Small Int pop test" test1, 
                  TestLabel "Large Int pop test" test2, 
                  TestLabel "Empty pop test" test3]