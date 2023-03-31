module Main where

-- некторая теореия о работе СЕД
-- 1. это потоковый редактор, который выполняет обработку всего входа в 
-- один проход.
-- 2. результат обработки выводится в стандртнрый выход, если иное не
-- регулируется ключом -i. Этот ключ редактировует файл "по месту".
-- 3. как работает
--    1. чиатет строку из входного потока, удаляет завершающий \n и 
--    кладет строку в pattern space
--    2. теперь исполняется сценарий (заданная последовательность команд)
--    , и любая команда может содержать адрес 
--    обрабатываемой строки. Этот адрес что-то типа условия, команда
--    выполняется если в pattern space сейчас находится строка с данным
--    номером. В противном случае команда не выполняется.
--    СЭД копирует строку из input в pattern space  и к этой строке применяет
--    все команды, адреса которых попадают в pattern space (то есть строка
--    с данным номером сейчас содержится в pattern space). Затем patter space
--    копируется в output.
--    Copy the input line into the pattern space.
-- Apply the first sed command on the pattern space, if the address restriction
-- is true.
-- Repeat with the next sed expression, again operating on the pattern space.
-- When the last operation is performed, write out the pattern space and 
-- read in the next line from the input file.
--    3. Если не указан ключик -n то содержимое pattern space печатается в
--    выходной канал. При этом к строке добавляется \n ранее удаленный.
--    4. переходим к следующей строке во входном потоке и возвращаемся к шагу 1.
--    Таким образом весь сценарий команд выполняется раз за разом для каждой
--    строки входного потока.
--    5. Содердимое pattern space очищается между циклами, если не задана
--    специальная команда D.
--    6. А вот hold space  наоборот, сохраняет свои данные между циклами.
--    




-- 31/03 вводим понятие zipper, таким образом можно модернизировать
-- строить непосредстенно в input, вместо перекачивания строк из input 
-- в output с промежуточной обработкой.
-- Zipper это способ представления данных, курсор указывает на текущий
-- элемент. С этим классом мы можем например ходить по списку вперед-назад.
-- В нашем случае назад ходить ненужно, но нужно переходить к следующему 
-- элементу сразу после завершения работы команды. И для этого мы будем
-- использовать метод Zipper.push
-- 

import Control.Monad.State
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.List.Zipper as Z


data SedState = SedState {
                      line         :: Int
                    , input        :: Z.Zipper T.Text
                    , patternSpace :: T.Text
                    , holdSpace    :: T.Text
                    --, output       :: T.Text
                }

data Command = Print
             | Delete
             | Next
             | Substitute String String [Char]

sed :: [Command] -> T.Text -> T.Text
-- 31.03 рефакторинг: использование Zipper изменяет определение defaultState
-- В определении z входной поток разделяется на строки, так что теперь
-- элементами Зиппера становятся строки и ходим мы по строкам.
sed cs t = evalState (runCommands cs) defaultState
    where defaultState = SedState 1 z (Z.cursor z) (T.singleton '\n') -- T.empty
          z = Z.fromList (T.lines t)

-- команды последовательно вносят изменения в SedState
-- модифицируют output,  а потом его значение возвращается
-- в interact и печатается
runCommands :: [Command] -> State SedState T.Text
-- 31/03 рефакторинг: тепеь исподхзуем Зиппер
runCommands cs = do
    -- мы вызываем mapM_ и он проходит по всему списку команд
    -- и таким образом SedState приводится в целевое состояние
    -- а output содержит итоговый результат обработки
    mapM_ runCommand cs
    ss <- get -- а теперь мы считываем это итоговое состояние
    if Z.endp (input ss) -- и если мы обработали весь входной поток
    -- 31.03 рефакторинг выполняем действия чтобы извлечь значение из Зиппера
    -- и вернуть T.Text
    then return . T.unlines . Z.toList $ input ss
    else runCommand Next >> runCommands cs -- а если нет, то делаем переход к следующей строке и продолжаем обработку


 
runCommand :: Command -> State SedState ()
-- 30/03 рефакторинг, для повышения производительности, вызов замен на <+>
-- 31/03 рефакторринг: теперь используем Zipper. Вызов push двигает курсор на
-- следующий элемент после вставки. То есть курср стоял на каком-то input,
-- push выполнил вставку и перешел с следующему элементу в input (тому который вставили).
runCommand Print = modify $ \ss -> ss { input = Z.push (patternSpace ss) (input ss)}
--runCommand Print = modify $ \ss -> 
--    let newOutput = (T.lines $ output ss) ++ (T.lines $ patternSpace ss)
--    in ss {output = T.unlines newOutput }
runCommand Delete = modify $ \ss -> ss { patternSpace  = T.empty }
-- переходим к обработке следующей строки и заодно помещаем ее в patternSpace,
-- где она собственно и обрабатывается
-- 31/03 рефакторинг: изпольхуем Зиппер, и еще при переходе к следующей строке
-- нужно принудительно двинуть Зиппер вперед
runCommand Next = modify $ \ss -> ss { line = line ss + 1, 
                                       input = Z.right (input ss),
                                       -- после сдвига Зиппера берем текущее значение
                                       -- курсора и помещаем его в буфера для обработки
                                       patternSpace = Z.cursor (input ss) }


(<+>) :: T.Text -> T.Text -> T.Text
a <+> b = a `T.append` T.cons '\n' b

main :: IO ()
-- получает на вход список команд, но их еще надо извлечь
-- из командной строки
main = TIO.interact $ sed [Print] --, Next, Delete]

