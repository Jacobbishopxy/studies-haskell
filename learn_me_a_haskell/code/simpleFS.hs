-- file: simpleFS.hs
-- author: Jacob Xie
-- date: 2023/08/20 13:35:07 Sunday
-- brief:

type Name = String

type Data = String

data FSItem = File Name Data | Folder Name [FSItem] deriving (Show)

myDisk :: FSItem
myDisk =
  Folder
    "root"
    [ File "goat_yelling_like_man.wmv" "baaaaaa",
      File "pope_time.avi" "god bless",
      Folder
        "pics"
        [ File "ape_throwing_up.jpg" "bleargh",
          File "watermelon_smash.gif" "smash!!",
          File "skull_man(scary).bmp" "Yikes!"
        ],
      File "dijon_poupon.doc" "best mustard",
      Folder
        "programs"
        [ File "fartwizard.exe" "10gotofart",
          File "owl_bandit.dmg" "mov eax, h00t",
          File "not_a_virus.exe" "really not a virus",
          Folder
            "source code"
            [ File "best_hs_prog.hs" "main = print (fix error)",
              File "random.hs" "main = print 4"
            ]
        ]
    ]

data FSCrumb = FSCrumb Name [FSItem] [FSItem] deriving (Show)

type FSZipper = (FSItem, [FSCrumb])

x -: f = f x

fsUp :: FSZipper -> FSZipper
fsUp (item, FSCrumb name ls rs : bs) = (Folder name (ls ++ [item] ++ rs), bs)

fsTo :: Name -> FSZipper -> FSZipper
fsTo name (Folder folderName items, bs) =
  let (ls, item : rs) = break (nameIs name) items
   in (item, FSCrumb folderName ls rs : bs)

nameIs :: Name -> FSItem -> Bool
nameIs name (Folder folderName _) = name == folderName
nameIs name (File fileName _) = name == fileName

-- Manipulating our file system
fsRename :: Name -> FSZipper -> FSZipper
fsRename newName (Folder name items, bs) = (Folder newName items, bs)
fsRename newName (File name dat, bs) = (File newName dat, bs)

fsNewFile :: FSItem -> FSZipper -> FSZipper
fsNewFile item (Folder folderName items, bs) = (Folder folderName (item : items), bs)

main :: IO ()
main = do
  let newFocus = (myDisk, []) -: fsTo "pics" -: fsTo "skull_man(scary).bmp"
  putStrLn $ "newFocus: " ++ show (fst newFocus)

  let newFocus2 = newFocus -: fsUp -: fsTo "watermelon_smash.gif"

  putStrLn $ "newFocus2: " ++ show (fst newFocus2)

  let newFocus3 = (myDisk, []) -: fsTo "pics" -: fsRename "cspi" -: fsUp

  putStrLn $ "newFocus3: " ++ show (fst newFocus3)

  let newFocus4 = (myDisk, []) -: fsTo "pics" -: fsNewFile (File "heh.jpg" "lol") -: fsUp

  putStrLn $ "newFocus4: " ++ show (fst newFocus4)
