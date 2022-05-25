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
          menu students modules


{- MENUS -}
menu :: Students -> Modules -> IO ()
menu students modules = do putStrLn ""
                           putStrLn "\nWhat do you want to do?"
                           putStrLn "1. Search details"
                           putStrLn "2. Add details"
                           putStrLn "3. Delete details"
                           putStrLn "4. Create report"
                           putStrLn "5. Quit"
                           choice <- getLine
                           menuPath choice students modules

menuPath :: String -> Students -> Modules -> IO()
menuPath a students modules
         | a == "1" = search students modules
         | a == "2" = add students modules
         | a == "3" = delete' students modules
         | a == "4" = report students modules
         | a == "5" = return ()
         

search :: Students -> Modules -> IO ()
search students modules = do putStrLn ""
                             putStrLn "1. Search for student details"
                             putStrLn "2. Search for module details"
                             putStrLn "3. Menu" 
                             choice <- getLine
                             searchPath choice students modules

searchPath :: String -> Students -> Modules -> IO ()
searchPath a students modules
           | a == "1" = searchStudent students
           | a == "2" = searchModule modules
           | a == "3" = main
           
           
searchStudent:: Students -> IO ()
searchStudent studentDict = do putStrLn ""
                               putStrLn "How would you like to search for the student?"
                               putStrLn "1. ID" 
                               putStrLn "2. Full name" 
                               putStrLn "3. Surname"
                               putStrLn "4. Menu" 
                               input <- getLine
                               let name = input
                               studentSearchPath input studentDict

studentSearchPath :: String -> Students -> IO ()
studentSearchPath a students
           | a == "1" = studentSearchID students
           | a == "2" = studentSearchFullName students
           | a == "3" = studentSearchSurname students
           | a == "4" = main
           
searchModule :: Modules -> IO ()
searchModule modules = do putStrLn ""
                          putStrLn "How would you like to search for the module?"
                          putStrLn "1. ID" 
                          putStrLn "2. course" 
                          putStrLn "3. Menu" 
                          input <- getLine
                          let name = input
                          moduleSearchPath input modules

moduleSearchPath :: String -> Modules -> IO ()
moduleSearchPath a modules
           | a == "1" = moduleSearchID modules
           | a == "2" = moduleSearchName modules
           | a == "3" = main

add :: Students -> Modules -> IO ()
add students modules = do putStrLn ""
                          putStrLn "1. Add student"
                          putStrLn "2. Add module"
                          putStrLn "3. Menu" 
                          choice <- getLine
                          addPath choice students modules

addPath :: String -> Students -> Modules -> IO ()
addPath a students modules
        | a == "1" = addStudent students
        | a == "2" = addModule modules students
        | a == "3" = main

delete' :: Students -> Modules -> IO ()
delete' students modules = do putStrLn ""
                              putStrLn "1. Delete student"
                              putStrLn "2. Delete module"
                              putStrLn "3. Menu" 
                              choice <- getLine
                              deletePath choice students modules
                             
deletePath :: String -> Students -> Modules -> IO ()
deletePath a students modules 
           | a == "1" = deleteStudent students
           | a == "2" = deleteModule modules
           | a == "3" = main
                          
report :: Students -> Modules -> IO ()
report students modules = do putStrLn ""
                             putStrLn "1. Create student report"
                             putStrLn "2. Create Module report"
                             putStrLn "3. Menu" 
                             choice <- getLine
                             reportPath choice students modules

reportPath :: String -> Students -> Modules -> IO ()
reportPath a students modules
           | a == "1" = studentReportMenu students
           | a == "2" = moduleReportMenu modules students
           | a == "3" = main
           
studentReportMenu :: Students -> IO ()
studentReportMenu students = do putStrLn ""
                                putStrLn "How would you like to create the report?"
                                putStrLn "1. Individual student report"
                                putStrLn "2. All students report"
                                putStrLn "3. Menu" 
                                choice <- getLine
                                studentReportPath choice students
                                
studentReportPath :: String -> Students -> IO ()
studentReportPath a students
                  | a == "1" = studentIndividualReport students
                  | a == "2" = studentOverallReport students
                  | a == "3" = main

moduleReportMenu :: Modules -> Students -> IO ()
moduleReportMenu modules students = do putStrLn ""
                                       putStrLn "How would you like to create the report?"
                                       putStrLn "1. Individual module report"
                                       putStrLn "2. All modules report"
                                       putStrLn "3. Menu" 
                                       choice <- getLine
                                       moduleReportPath choice modules students
                                
moduleReportPath :: String -> Modules -> Students -> IO ()
moduleReportPath a modules students
                 | a == "1" = moduleIndividualReport modules students
                 | a == "2" = moduleOverallReport modules
                 | a == "3" = main


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
                               output = "Students.txt," ++ (show id) ++ "," ++ fore ++ "," ++ sur ++ "," ++ crs ++ "," ++ yr ++ ",\n" 
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

writeStudents :: Students -> IO ()
writeStudents students = do removeFile "Sources/Students.txt"
                            let list = M.elems (sDict students)
                            writeStudentsCore list
                            refreshSource
                            

writeStudentsCore :: [Student] -> IO ()
writeStudentsCore [] = return ()
writeStudentsCore (x:xs) = do let id = sID x
                                  fore = forename x
                                  sur = surname x
                                  crs = course x
                                  yr = year x
                                  output = "" ++ (show id) ++ "," ++ fore ++ "," ++ sur ++ "," ++ crs ++ "," ++ yr ++ ",\n" 
                              appendFile "Sources/Students.txt" output
                              writeStudentsCore xs   

writeModules :: Modules -> IO ()
writeModules modules = do removeFile "Sources/Modules.txt"
                          let list = M.elems (mDict modules)
                          writeModulesCore list
                          refreshSource
                    
writeModulesCore :: [Module] -> IO ()
writeModulesCore [] = return ()
writeModulesCore (x:xs) = do let id = mID x
                                 title = name x
                                 intIDs = studentIDs x
                                 ids = studentIDsOutput intIDs
                                 output = (show id) ++ "," ++ title ++ "," ++ ids ++ ",\n"
                             appendFile "Sources/Modules.txt" output
                             writeModulesCore xs
                                 

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
                                                   output = id ++ "," ++ forename ++ "," ++ surname ++ "," ++ course ++ "," ++ year ++ ",\n"
                                               appendFile "Sources/Students.txt" output
                                               return () 
                                       else do seperateFiles inputHandle
                                               let id = line !! 1
                                                   name = line !! 2
                                                   students = drop 3 line
                                                   output = id ++ "," ++ name ++ "," ++ (intercalate "," students) ++ ",\n"
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
studentSearchID :: Students -> IO ()
studentSearchID students = do putStrLn ""
                              putStrLn "Enter a student ID"
                              studentID <- getLine
                              let studentIDint = read studentID::Integer
                              let student = M.lookup studentIDint (sDict students)
                              putStrLn ""
                              let student' = removeMaybe student
                              if student' == []
                              then putStrLn "Student not found"
                              else printStudent (head student')
                              main

studentSearchFullName :: Students -> IO ()
studentSearchFullName students = do putStrLn ""
                                    putStrLn "Enter a student name"
                                    studentName <- getLine
                                    let studentLists = M.toList (sDict students)
                                    do studentID <- studentFullNameIteration studentName studentLists
                                       let student = M.lookup studentID (sDict students)
                                       putStrLn ""
                                       let student' = removeMaybe student
                                       if student' == []
                                       then putStrLn "Student not found"
                                       else printStudent (head student')
                                       main

studentFullNameIteration :: String -> [(Integer, Student)] -> IO Integer
studentFullNameIteration _ [] = return 0
studentFullNameIteration name (x:xs) = do let student = snd x
                                          do studentName <- createIOName (forename student, surname student)
                                             if lower studentName == lower name
                                             then return (fst x)
                                             else studentFullNameIteration name xs

studentSearchSurname :: Students -> IO ()
studentSearchSurname students = do putStrLn ""
                                   putStrLn "Enter a student surname"
                                   studentName <- getLine
                                   let studentLists = M.toList (sDict students)
                                   do studentIDs <- studentSurnameIteration studentName studentLists []
                                      putStrLn ""
                                      if null studentIDs
                                      then putStrLn "Student not found"
                                      else studentPrinting (sort studentIDs) students
                                      main

studentSurnameIteration :: String -> [(Integer, Student)] -> [Integer] -> IO [Integer]
studentSurnameIteration _ [] _ = return []
studentSurnameIteration name (x:xs) ids = do let student = snd x
                                                 id = fst x
                                                 studentName = surname student
                                             if lower studentName == lower name
                                             then do ids <- studentSurnameIteration name xs ids
                                                     let newIDs = ids ++ [id]
                                                     return newIDs
                                             else do ids <- studentSurnameIteration name xs ids
                                                     return ids

studentPrinting :: [Integer] -> Students -> IO ()
studentPrinting [] _ = return ()
studentPrinting (x:xs) students = do let maybeStudent = M.lookup x (sDict students)
                                         justStudent = removeMaybe maybeStudent
                                         student = head justStudent
                                     printStudent student
                                     putStrLn "\n"
                                     studentPrinting xs students 

{- MODULE SEARCHING -}         
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
addStudent students = do putStr "Forename: "
                         forename <- getLine
                         putStr "Surname: "
                         surname <- getLine
                         putStr "Course: "
                         course <- getLine
                         putStr "Year: "
                         year <- getLine
                         let lastID = last (M.keys (sDict students))
                             id = lastID + 1
                             output = "" ++ (show id) ++ "," ++ forename ++ "," ++ surname ++ "," ++ course ++ "," ++ year ++ ","
                         let studentLists = M.toList (sDict students)
                             fullName = createStringName (forename, surname)
                         do studentID <- studentFullNameIteration fullName studentLists
                            let student = M.lookup studentID (sDict students)
                            putStrLn ""
                            let student' = removeMaybe student
                            if null student'
                            then do appendFile "Sources/Students.txt" output
                                    refreshSource
                            else putStrLn "A student with this name already exists"
                         main
                     
addModule :: Modules -> Students -> IO ()
addModule modules students = do putStr "Title: "
                                title <- getLine
                                putStr "Student IDs (seperate by commas, no spaces): "
                                inIDs <- getLine
                                let lastID = last (M.keys (mDict modules))
                                    id = lastID + 1
                                    stringIDs = splitOn "," inIDs
                                    ids = stringsToInts stringIDs
                                    output = "" ++ (show id) ++ "," ++ title ++ "," ++ inIDs ++ ","
                                do invalid <- invalidID ids students
                                   if invalid
                                   then putStrLn "A student ID was invalid"
                                   else do appendFile "Sources/Modules.txt" output
                                           refreshSource
                                   main
                                
invalidID :: [Integer] -> Students -> IO Bool
invalidID [] _ = return False
invalidID (x:xs) students = do let dict = sDict students
                                   maybeStudent = M.lookup x dict
                                   justStudent = removeMaybe maybeStudent
                               if null justStudent
                               then return True
                               else do valid <- invalidID xs students
                                       return valid

{- DELETE -} 
deleteStudent :: Students -> IO ()
deleteStudent students = do putStrLn "What is the student ID?"
                            inID <- getLine
                            let dict = sDict students
                                id = read inID :: Integer
                                exists = M.member id dict
                            if not exists 
                            then putStrLn "This ID does not exist"
                            else do let newDict = M.delete id dict
                                        newStudents = Students newDict
                                    writeStudents newStudents
                            main

deleteModule :: Modules -> IO ()
deleteModule modules = do putStrLn "What is the module ID?"
                          inID <- getLine
                          let dict = mDict modules
                              id = read inID :: Integer
                              exists = M.member id dict
                          if not exists 
                          then putStrLn "This ID does not exist"
                          else do let newDict = M.delete id dict
                                      newModules = Modules newDict
                                  writeModules newModules
                          main

{- REPORTS -}
studentIndividualReport :: Students -> IO ()
studentIndividualReport students = do putStrLn "What is the student ID?"
                                      inID <- getLine
                                      let id = read inID :: Integer
                                          dictStudent = M.lookup id (sDict students)
                                          student = removeMaybe dictStudent
                                      if student == []
                                      then putStrLn "Student not found"
                                      else createIndividualStudentReport (head student)
                                      main

createIndividualStudentReport :: Student -> IO ()
createIndividualStudentReport student = do let id = "Student ID: " ++ (show (sID student))
                                               fn = "\nForename: " ++ (forename student)
                                               sn = "\nSurname: " ++ (surname student)
                                               crs = "\nCourse: " ++ (course student)
                                               yr = "\nYear: " ++ (year student)
                                               output = id ++ fn ++ sn ++ crs ++ yr
                                               name = createStringName (forename student, surname student)
                                               filename = "Reports/Students/" ++ name ++ ".txt"
                                           writeFile filename output

studentOverallReport :: Students -> IO () 
studentOverallReport students = do exists <- doesFileExist "Reports/Students/Students.txt"
                                   let studentList = M.elems (sDict students)
                                   if exists 
                                   then do removeFile "Reports/Students/Students.txt" 
                                           createOverallStudentReport studentList
                                   else createOverallStudentReport studentList
                                   main


createOverallStudentReport :: [Student] -> IO ()
createOverallStudentReport [] = return ()
createOverallStudentReport (x:xs) = do let id = "Student ID: " ++ (show (sID x))
                                           fn = "\nForename: " ++ (forename x)
                                           sn = "\nSurname: " ++ (surname x)
                                           crs = "\nCourse: " ++ (course x)
                                           yr = "\nYear: " ++ (year x)
                                           output = id ++ fn ++ sn ++ crs ++ yr ++ "\n\n"
                                           filename = "Reports/Students/Students.txt"
                                       appendFile filename output
                                       createOverallStudentReport xs

moduleIndividualReport :: Modules -> Students -> IO ()
moduleIndividualReport modules students = do putStrLn "What is the module ID?"
                                             inID <- getLine
                                             let id = read inID :: Integer
                                                 dictModule = M.lookup id (mDict modules)
                                                 module' = removeMaybe dictModule
                                             if module' == []
                                             then putStrLn "Module not found"
                                             else createIndividualModuleReport (head module') students
                                             main

createIndividualModuleReport :: Module -> Students -> IO ()
createIndividualModuleReport module' students = do let id = "Module ID: " ++ (show (mID module'))
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
                                               
                                               
moduleOverallReport :: Modules -> IO ()
moduleOverallReport modules = do exists <- doesFileExist "Reports/Modules/Modules.txt"
                                 let moduleList = M.elems (mDict modules)
                                 if exists 
                                 then do removeFile "Reports/Modules/Modules.txt" 
                                         createOverallModuleReport moduleList
                                 else createOverallModuleReport moduleList
                                 main
                                 
createOverallModuleReport :: [Module] -> IO ()
createOverallModuleReport [] = return ()
createOverallModuleReport (x:xs) = do let id = "Module ID: " ++ (show (mID x))
                                          title = "\nTitle: " ++ (name x)
                                          sIDs = "\nStudents: " ++ (studentIDsOutput (studentIDs x))
                                          output = id ++ title ++ sIDs ++ "\n\n"
                                      appendFile "Reports/Modules/Modules.txt"  output
                                      createOverallModuleReport xs

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