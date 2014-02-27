>module Main where

Импорты.

Наши биндинги.
Низкоуровневые.

>import Bindings.Verba.C
>import Bindings.Verba.ErrorCodes

Высокоуровневые.

>import Bindings.Verba.HL

Импортируемые функции перечислены исключительно с точки зрения дидактити.
Пожалуйста, *НЕ* используйте такой стиль в своих проектах.

>import Control.Applicative
>import Control.Monad

>import Control.Monad.Error

>import Control.Exception

>import Foreign (Word16(..))

>import Data.List (sort)

>import System.Environment (getArgs)

Пакет directory. 

>import System.Directory (getCurrentDirectory, copyFile)

Пакет filepath.

>import System.FilePath ((</>), replaceExtension, splitFileName)

Вспомогательная функция подготовки путей.
Принимает список от 0 до n элементов (берутся первые 4).
В зависимости от числа элементов списка формирует 4-х элементный кортеж
(<путь к закрытым ключам шифрования>,<путь к открытым ключам шифрования>,<путь к закрытым ключам подписи>,<путь к закрытым ключам подписи>).
Если список пустой, то кортеж заполняется значением текущего каталога программы.

>initPaths :: [FilePath] -> IO (FilePath, FilePath, FilePath, FilePath)
>initPaths (a:[])       = return (a, a, a, a)
>initPaths (a:b:[])     = return (a, b, a, b)
>initPaths (a:b:c:[])   = return (a, b, c, b)
>initPaths (a:b:c:d:_)  = return (a, b, c, d)
>initPaths _ = do
>                  w <- getCurrentDirectory
>                  return (w, w, w, w)

О путях стоит упомянуть отдельно. Русские имена в путях не поддерживаются. Совершенно.
Вариантов решения проблемы аж 4:
1. Не использовать русские пути. Самый простой вариант.
   Использование русских путей вообще-то моветон.
2. Делать GUIшную программу (ключ -optl-mwindows для ghc, передающий -mwindows в ld).
   *Внезапно*, все заработает. Только консоли не будет.
3. Реализовать свою версию FFI-шных withCString с помощью аналогов из GHC.FFI.
4. Явно выставить кодировку с помощью setForeignEncoding из GHC.IO.Encoding.
   Этот способ, хоть и самый простой, и может быть самый правильный, но он устанавливает кодировку для всей FFI-подсистемы 
   GHCшного рантайма. Т.е. русские имена в путях мы однозначно починим, а вот что-то другое *может* и сломаться.

Проблема эта не GHC- или хаскельспецифичная. Она присутствует в реализации любого языка, рантайм которого использует юникод для представления строк.

Инициализирует подсистемы подписи и шифрования Вербы.
Пути к справочникам передаются по логике функции initPaths.
Пути не проверяются на валидность до первого вызова процедур шифрования/подписи.
При повторном вызове первая процедура выкинет эксцепшен с кодом ошибки 8 (E_INIT).

>initVerba :: [FilePath] -> IO ()
>initVerba p = do
>    (cc, co, sc, so) <- initPaths p
>    cryptoInit cc co
>    signInit   sc so

Деинициализирует подсистемы подписи и шифрования Вербы.
Никогда не кидает эксцепшенов.

>doneVerba :: IO ()
>doneVerba = do
>    cryptoDone
>    signDone
>    return ()

Шифрует и подписывает файл "простыми" файловыми сервисами Вербы.
p1 — исходный файл
p2 — подписанный файл
p3 — подписанный и зашифрованный файл
sign — номер секретного ключа подписи 12 символов, можно передать пустой список, тогда ключ подписи будет взят из 0-го слота.
Семантика передачи ключа подписи сложна. Процедуре signInit передается указатель на блок размером 13 байт (12 номер ключа плюс \0). Если номер ключа, серии или порядкового номера ключа подписи состоит из \0, то они берутся из 0-го слота. После чего в буфер записывается полный номер ключа подписи.
self — номер секретного ключа шифрования (0 - 65535)
rcvrs — список номеров получателей (0 - 65535)
series — серия ключей шифрования
На справочники должны быть выработаны имитовставски на ключах в слоте №0.
Например:
в 0й слот загружены ключи 1234941099 (шифрование) 1234994109901 (подпись)
в 1й слот загружены ключи 4321942009 (шифрование)
получатели 8020942009 и 8030942009
тогда,
signAndEncrypt "inp/a" "tmp/a" "out/a" "123494100901" 4321 [8020, 8030]  "942009"

>signAndEncrypt :: FilePath -> FilePath -> FilePath -> String -> Word16 -> [Word16] -> String -> IO ()
>signAndEncrypt p1 p2 p3 sign self rcvrs series = do
>    signFile p1 p2 sign
>    enCryptFile p2 p3 self rcvrs series

Понятно, что предыдущая функция крайне неудобна в использовании:
Во-первых, передается слишком много весьма и весьма неочевидных параметров.

Для целей отладки приведем список часто возникающих проблем

Код ошибки 6   — неправильно указан собственный ключ шифрования, подписи и/или соответствующий CKD/CKDI не загружен в драйвер.
Код ошибки 8   — не выполнена процедура инициализации Вербы
Код ошибки 103 — неправильно указана ключевая серия открытого ключа шифрования, либо неправильно указаны пути для ctyptoInit и/или signInit. 
Код ошибки 117 — неправильно указан номер открытого ключа шифрования получателя (одного из получателей)
TODO: дописать как функция себя ведет при неправильно сформированных имитовставках на справочники
Код ошибки 114 — неправильная имитовставка на справочник открытых ключей подписи
возможно, еще и 107 и 109.

Во-вторых, при возникновении ошибки в enCryptFile, файл останется подписанным, но незашифрованным.
Именно ради этого и приходится городить огород с тремя каталогами.

>data CryptoEnvironment = CryptoEnvironment { _signKey :: String
>                                           , _encryptionKey :: (Word16, String)
>                                           } deriving (Eq, Show)

Получает номера ключей для схемы шифрования ФНС/ФТС путем анализа загруженных ключей.
Пытается ругаться на тупость пользователя.

Проводится слишком мало проверок, например не проводится проверка на длину ключа, не анализируется поле keyStatus, и вообще номера ключей должны быть объявлены отдельными типами данных...

>getCryptoEnvironment :: ErrorT String IO CryptoEnvironment
>getCryptoEnvironment = do
>    slots <- liftIO $ sort <$> getDrvInfo
>    case slots of
>         (s0:s1:_) -> do
>             when (_nump s0 == "")
>                  (throwError "В 0-й слот не загружен ключ подписи")
>             when (_num  s0 == "")
>                  (throwError "В 0-й слот не загружен ключ шифрования")
>             when (_num  s1 == "")
>                  (throwError "В 1-й слот не загружен ключ шифрования")
>             return $ CryptoEnvironment (_nump s0)
>                                        ( (read . take 4 $ _num s1)
>                                        , (take 6 . drop 4 $ _num s1))
>         _         -> throwError $ "В драйвер должно быть загружено по крайней мере два ключа"

Более простая версия, предназначенная для шифрования по схеме ФНС.
Нужные номера ключей берутся из CryptoEnvironment.
Пути рассчитываются от каталога baseDir.
Расширение зашифрованного файла заменяется на ".vrb".
env - "криптографическое окружение"
baseDir - базовый каталог шифрования и подписи. Должен включать 3 подкаталога "inp", "tmp", "out"
filename - имя файла для зашифрования
rcvrs - список номеров ключей получателей

>signAndEncryptEnv :: CryptoEnvironment -> FilePath -> String -> [Word16] -> IO ()
>signAndEncryptEnv env baseDir filename rcvrs = do
>    let p1   = baseDir </> "inp" </> filename
>        p2   = baseDir </> "tmp" </> filename
>        p3   = baseDir </> "out" </> (replaceExtension filename "vrb")
>        sign = _signKey env
>        (self, series) = _encryptionKey env
>    signFile p1 p2 sign
>    enCryptFile p2 p3 self rcvrs series
>

Расшифровывает и проверяет подпись. Возвращает результат проверки.
Работает по тому же принципу что и signAndEncryptEnv

>decryptAndCheckEnv :: CryptoEnvironment -> FilePath -> String -> IO Bool
>decryptAndCheckEnv env baseDir filename = do
>    let p1       = baseDir </> "inp" </> filename
>        p2       = baseDir </> "tmp" </> filename
>        p3       = baseDir </> "out" </> filename
>        (key, _) = _encryptionKey env
>    getFileSenderId p1                              -- Выдает более понятную ошибку, если файл был не зашифрован
>    deCryptFile p1 p2 key
>    res <- check_file_sign p2
>    if all ((==) SIGN_STATUS_CORRECT . _status) res
>    then do  delSign p2 (-1)
>             copyFile p2 p3
>             return True
>    else return False

Спецверсия liftIO. Поднимает VerbaException в IO до ErrorT.

>liftV :: IO a -> ErrorT String IO a
>liftV = ErrorT . liftM (either (\(VerbaException errc) ->
>                                    Left . showVerbaError . verr $ errc) (Right)) . try

>showVerbaError (VerbaError code mnemonic descr) =
>    "Ошибка Вербы: " ++ (show code) ++ " " ++ mnemonic ++ " "++ descr

Верба позволяет расшифровывать файлы, зашифрованные на собственном закрытом ключе, даже без указания самого себя в качестве получателя. Это наиболее простая версия функции, никогда не генерирующая эксцепшенов.

>rollback :: FilePath -> IO ()
>rollback path = do
>   handle (\(VerbaException _) -> return ())
>          (do sender <- getFileSenderId path
>              deCryptFile path path (read . take 4 $ sender)
>              delSign path (-1))

Запускает произвольное IO-вычисление внутри контекста вербы. Ловит VerbaException.

>withVerba :: [FilePath] -> IO a -> IO ()
>withVerba path io =
>    handle (\(VerbaException e) -> putStrLn . showVerbaError . verr $ e)
>           (bracket_ (initVerba path)
>                     (doneVerba)
>                     (io >> return ()))

Сводим все воедино.
Парсер аргементов командной строки самый примитивный.

>execArgs ("signencrypt":baseDir:filename:rcvrs) = do
>    liftIO . putStrLn $ "Подписываю и шифрую файл " ++ filename ++ "\nна абонентов " ++ (show rcvrs)
>    env <- getCryptoEnvironment
>    liftV $ signAndEncryptEnv env baseDir filename (map read rcvrs)
>execArgs ("decryptcheck":baseDir:filename:_) = do
>    liftIO . putStrLn $ "Расшифровываю и проверяю подпись файла " ++ filename
>    env <- getCryptoEnvironment
>    res <- liftV $ decryptAndCheckEnv env baseDir filename
>    unless res (throwError "Подпись под файлом неверна.")
>execArgs ("rollback":path:_) = do
>    getCryptoEnvironment  -- Само окружение нам не нужно... А вот ругань...
>    liftIO . putStrLn $ "Отменяю криптографическое преобразование " ++ path
>    liftV $ rollback path
>execArgs _ = do liftIO $ usage
>                throwError "Неверные параметры командной строки"

Пишет результат операции в консоль

>reportResult (Left e) = putStrLn $ "Ошибка: " ++ e
>reportResult (Right _) = putStrLn "Операция завершена успешно."

Выводит небольшой хелп.

>usage :: IO ()
>usage = do
>    putStrLn "Usage:"
>    putStrLn "\t<keydir> signencrypt <baseDir> <fileName> <rcvr1> <rcvr2> ... <rcvrN>"
>    putStrLn "\t<keydir> decryptcheck <baseDir> <fileName>"
>    putStrLn "\t<keydir> rollback <filePath>"
>    putStrLn ""

Главная функция

>main :: IO ()
>main = do
>    args <- getArgs
>    case args of
>         keysDir:rest -> withVerba [keysDir] (reportResult =<< (runErrorT $ execArgs rest))
>         _            -> usage

Как этим пользоваться.

Для разработчика.
ghci Simple.lhs wbotho.dll

Чтобы скомпилировать 
ghc --make Simple.lhs -lwbotho -L%WINDIR%\System32

или через cabal
