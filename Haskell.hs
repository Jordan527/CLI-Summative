{-
REQUIREMENTS
• Your program will be a CLI application
• Your program should have the ability to read a file format of your choice, which holds the initial modules and enrolled students   ********* DONE *********
• Your program should have the ability to search for student details					********* DONE *********
• Your program should have the ability to add new students and modules                  ********* PARTIALLY DONE *********   NO CHECKS         
• Your program should be able to output reports to a file (e.g., Students enrolled)     ********* DONE *********
-}

{- 
DO NEXT
• option do display all student details in module rather than just id's
• do checks before allowing students and modules to be added (existing entries)
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
               sID :: Integer,
               forename :: String, 
               surname :: String, 
               course :: String, 
               year :: String} deriving (Eq, Show)

data Module = Module {
              mID :: Integer,
              name :: String,
              studentIDs :: [Integer]} deriving (Eq, Show)


data Students = Students {sDict::(M.Map Integer Student)} deriving (Show)
studentDict :: M.Map Integer Student
studentDict = M.empty

data Modules = Modules {mDict::(M.Map Integer Module)} deriving (Show)
moduleDict :: M.Map Integer Module
moduleDict = M.empty

main :: IO ()
main = do deleteFiles'
          readFiles'
          studentDict <- loadStudents
          moduleDict <- loadModules
          let students = Students studentDict
          let modules = Modules moduleDict
          refreshSource
          menu students modules


menu :: Students -> Modules -> IO ()
menu students modules = do putStrLn ""
                           putStrLn "\nWhat do you want to do?"
                           putStrLn "1. Search details"
                           putStrLn "2. Add details"
                           putStrLn "3. Create report"
                           putStrLn "4. Quit"
                           choice <- getLine
                           menuPath choice students modules

menuPath :: String -> Students -> Modules -> IO()
menuPath a students modules
         | a == "1" = search students modules
         | a == "2" = add students modules
         | a == "3" = report students modules
         | a == "4" = return ()
         

search :: Students -> Modules -> IO ()
search students modules = do putStrLn ""
                             putStrLn "1. Search for student details"
                             putStrLn "2. Search for module details"
                             choice <- getLine
                             searchPath choice students modules

searchPath :: String -> Students -> Modules -> IO ()
searchPath a students modules
           | a == "1" = searchStudent students
           | a == "2" = searchModule modules

add :: Students -> Modules -> IO ()
add students modules = do putStrLn ""
                          putStrLn "1. Add student"
                          putStrLn "2. Add module"
                          choice <- getLine
                          addPath choice students modules

addPath :: String -> Students -> Modules -> IO ()
addPath a students modules
        | a == "1" = addStudent students
        | a == "2" = addModule modules students

report :: Students -> Modules -> IO ()
report students modules = do putStrLn ""
                             putStrLn "1. Create student report"
                             putStrLn "2. Create Module report"
                             choice <- getLine
                             reportPath choice students modules

reportPath :: String -> Students -> Modules -> IO ()
reportPath a students modules
           | a == "1" = studentReport students
           | a == "2" = moduleReport modules students


{- REPORTS -}
studentReport :: Students -> IO ()
studentReport students = do putStrLn "What is the student ID?"
                            inID <- getLine
                            let id = read inID :: Integer
                                dictStudent = M.lookup id (sDict students)
                                student = removeMaybe dictStudent
                            if student == []
                            then putStrLn "Student not found"
                            else createStudentReport (head student)

createStudentReport :: Student -> IO ()
createStudentReport student = do let id = "Student ID: " ++ (show (sID student))
                                     fn = "\nForename: " ++ (forename student)
                                     sn = "\nSurname: " ++ (surname student)
                                     crs = "\nCourse: " ++ (course student)
                                     yr = "\nYear: " ++ (year student)
                                     output = id ++ fn ++ sn ++ crs ++ yr
                                     name = createStringName (forename student, surname student)
                                     filename = "Reports/Students/" ++ name ++ ".txt"
                                 writeFile filename output

moduleReport :: Modules -> Students -> IO ()
moduleReport modules students = do putStrLn "What is the module ID?"
                                   inID <- getLine
                                   let id = read inID :: Integer
                                       dictModule = M.lookup id (mDict modules)
                                       module' = removeMaybe dictModule
                                   if module' == []
                                   then putStrLn "Module not found"
                                   else createModuleReport (head module') students

createModuleReport :: Module -> Students -> IO ()
createModuleReport module' students = do let id = "Module ID: " ++ (show (mID module'))
                                             title = "\nTitle: " ++ (name module')
                                             sIDs = "\nStudents:"
                                             output = id ++ title ++ sIDs
                                             core = name module'
                                             filename = "Reports/Modules/" ++ core ++ ".txt"
                                         writeFile filename output
                                         moduleReportStudents filename (studentIDs module') students
                                         

moduleReportStudents :: String -> [Integer] -> Students -> IO ()
moduleReportStudents _ [] _ = return ()
moduleReportStudents path (x:xs) students = do let maybeStudent = M.lookup x (sDict students)
                                                   justStudent = removeMaybe maybeStudent
                                                   student = head justStudent
                                                   id = "\nStudent ID: " ++ (show (sID student))
                                                   fn = "\nForename: " ++ (forename student)
                                                   sn = "\nSurname: " ++ (surname student)
                                                   crs = "\nCourse: " ++ (course student)
                                                   yr = "\nYear: " ++ (year student)
                                                   output = "\n" ++ id ++ fn ++ sn ++ crs ++ yr
                                               appendFile path output
                                               moduleReportStudents path xs students
                                          

{- IMPORT DATA -}
refreshSource :: IO ()
refreshSource = do studentsDict <- loadStudents
                   modulesDict <- loadModules
                   let studentList = M.elems studentsDict
                       moduleList = M.elems modulesDict
                   removeFile "Sources/Data.txt"
                   appendStudents studentList
                   appendModules moduleList
                   
                   return ()

appendStudents :: [Student] -> IO ()
appendStudents [] = return ()
appendStudents (x:xs) = do let id = sID x
                               fore = forename x
                               sur = surname x
                               crs = course x
                               yr = year x
                               output = "Students.txt," ++ (show id) ++ "," ++ fore ++ "," ++ sur ++ "," ++ crs ++ "," ++ yr ++ "\n" 
                           appendFile "Sources/Data.txt" output
                           appendStudents xs

appendModules :: [Module] -> IO ()
appendModules [] = return ()
appendModules (x:xs) = do let id = mID x
                              title = name x
                              students = studentIDsOutput (studentIDs x)
                              output = "Modules.txt," ++ (show id) ++ "," ++ title ++ "," ++ students ++ "\n" 
                          appendFile "Sources/Data.txt" output
                          appendModules xs

deleteFiles' :: IO ()
deleteFiles' = do studentFile <- doesFileExist "Sources/Students.txt"
                  if studentFile then removeFile "Sources/Students.txt" else print "Sources/Students.txt does not exist"
                  moduleFile <- doesFileExist "Sources/Modules.txt"
                  if moduleFile then removeFile "Sources/Modules.txt" else print "Sources/Modules.txt does not exist"

readFiles' :: IO ()
readFiles' = do inputHandle <- openFile "Sources/Data.txt" ReadMode
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
                                               appendFile "Sources/Students.txt" output
                                               return () 
                                       else do seperateFiles inputHandle
                                               let id = line !! 1
                                                   name = line !! 2
                                                   students = drop 3 line
                                                   output = id ++ "," ++ name ++ "," ++ (intercalate "," students) ++ "," ++ "\n"
                                               appendFile "Sources/Modules.txt" output
                                               return () 
loadStudents :: IO (M.Map Integer Student)
loadStudents = do inputHandle <- openFile "Sources/Students.txt" ReadMode
                  do dict <- readStudents inputHandle 
                     hClose inputHandle
                     return dict

readStudents :: Handle -> IO (M.Map Integer Student)
readStudents inputHandle = do ineof <- hIsEOF inputHandle
                              let dict = studentDict
                              if ineof
                              then return dict 
                              else do outputString <- hGetLine inputHandle
                                      let line = splitOn "," outputString
                                          id = read (line !! 0) :: Integer
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

loadModules :: IO (M.Map Integer Module)
loadModules = do inputHandle <- openFile "Sources/Modules.txt" ReadMode
                 do dict <- readModules inputHandle 
                    hClose inputHandle
                    return dict
                     
readModules :: Handle -> IO (M.Map Integer Module)
readModules inputHandle = do ineof <- hIsEOF inputHandle
                             let dict = moduleDict
                             if ineof
                             then return dict 
                             else do outputString <- hGetLine inputHandle
                                     let line = splitOn "," outputString
                                         id = read (line !! 0) :: Integer
                                         name = line !! 1
                                         stringStudents = init (drop 2 line)
                                         students = stringsToInts stringStudents
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
                               putStrLn "3. Menu" 
                               input <- getLine
                               let name = input
                               studentSearchPath input studentDict

studentSearchPath :: String -> Students -> IO ()
studentSearchPath a studentDict
           | a == "1" = studentSearchID studentDict
           | a == "2" = studentSearchName studentDict
           | a == "3" = main

studentSearchID :: Students -> IO ()
studentSearchID studentDict = do putStrLn ""
                                 putStrLn "Enter a student ID"
                                 studentID <- getLine
                                 let studentIDint = read studentID::Integer
                                 let student = M.lookup studentIDint (sDict studentDict)
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

studentNameIteration :: String -> [(Integer, Student)] -> IO Integer
studentNameIteration _ [] = return 0
studentNameIteration name (x:xs) = do let student = snd x
                                      do studentName <- createIOName (forename student, surname student)
                                         if lower studentName == lower name
                                         then return (fst x)
                                         else studentNameIteration name xs


{- MODULE SEARCHING -}
searchModule :: Modules -> IO ()
searchModule moduleDict = do putStrLn ""
                             putStrLn "How would you like to search for the module?"
                             putStrLn "1. ID" 
                             putStrLn "2. course" 
                             putStrLn "3. Menu" 
                             input <- getLine
                             let name = input
                             moduleSearchPath input moduleDict

moduleSearchPath :: String -> Modules -> IO ()
moduleSearchPath a moduleDict
           | a == "1" = moduleSearchID moduleDict
           | a == "2" = moduleSearchName moduleDict
           | a == "3" = main
           
moduleSearchID :: Modules -> IO ()
moduleSearchID moduleDict = do putStrLn ""
                               putStrLn "Enter a module ID"
                               moduleID <- getLine
                               let moduleIDint = read moduleID :: Integer
                               let module' = M.lookup moduleIDint (mDict moduleDict)
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

moduleNameIteration :: String -> [(Integer, Module)] -> IO Integer
moduleNameIteration _ [] = return 0
moduleNameIteration course (x:xs) = do let module' = snd x
                                       if lower course == lower (name module')
                                       then return (fst x)
                                       else moduleNameIteration course xs                                      

{- ADD -}
addStudent :: Students -> IO ()
addStudent dict = do putStr "Forename: "
                     forename <- getLine
                     putStr "Surname: "
                     surname <- getLine
                     putStr "Course: "
                     course <- getLine
                     putStr "Year: "
                     year <- getLine
                     let lastID = last (M.keys (sDict dict))
                         id = lastID + 1
                         output = "" ++ (show id) ++ "," ++ forename ++ "," ++ surname ++ "," ++ course ++ "," ++ year ++ ","
                     appendFile "Sources/Students.txt" output
                     refreshSource
                     return ()
                     
addModule :: Modules -> students -> IO ()
addModule modules students = do putStr "Title: "
                                title <- getLine
                                putStr "Student IDs (seperate by commas, no spaces): "
                                inIDs <- getLine
                                let lastID = last (M.keys (mDict modules))
                                    id = lastID + 1
                                    output = "" ++ (show id) ++ "," ++ title ++ "," ++ inIDs ++ ","
                                appendFile "Sources/Modules.txt" output
                                refreshSource
                                return ()



{- FORMAT FUNCTIONS -}
printStudent :: Student -> IO ()
printStudent student = do putStrLn ("ID: " ++ (show (sID student)))
                          putStrLn ("Forename: " ++ (forename student))
                          putStrLn ("Surname: " ++ (surname student))
                          putStrLn ("Course: " ++ (course student))
                          putStrLn ("Year: " ++ (year student))
                          return ()
                         
printModule :: Module -> IO ()
printModule module' = do putStrLn ("ID: " ++ (show (mID module')))
                         putStrLn ("Course: " ++ (name module'))
                         putStrLn ("Students ID's: " ++ (studentIDsOutput (studentIDs module')))
                         return ()

pop :: [a] -> [a]
pop list = take ((length list) - 1) list

createIOName :: (String, String) -> IO String
createIOName (f, s) = return (f ++ " " ++ s)

createStringName :: (String, String) -> String
createStringName (f, s) = f ++ " " ++ s

lower :: String -> String
lower xs = [toLower x | x <- xs]

removeMaybe :: Maybe a -> [a]
removeMaybe Nothing = []
removeMaybe (Just a) = [a]

stringsToInts :: [String] -> [Integer]
stringsToInts list = [read x :: Integer | x <- list]

studentIDsOutput :: [Integer] -> String
studentIDsOutput list = intercalate "," [show x | x <- list]



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