module Sed where

import Control.Monad.State
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

data SedState = SedState {
                    line :: Int,
                    input :: T.Text,
                    patternSpace :: T.Text,
                    holdSpace :: T.Text,
                    output :: T.Text
                }

data Command = Print
             | Delete
             | Next
             | Substitute String String [Char]

sed :: [Command] -> T.Text -> T.Text
sed cs t = evalState (runCommands cs) defaultState
    where defaultState = SedState 1 t (head $ T.lines t) T.empty T.empty

-- команды последовательно вносят изменения в SedState
-- модифицируют output,  а потом его значение возвращается
-- в interact и печатается
runCommands :: [Command] -> State SedState T.Text
runCommands cs = do
    -- мы вызываем mapM_ и он проходит по всему списку команд
    -- и таким образом SedState приводится в целевое состояние
    -- а output содержит итоговый результат обработки
    mapM_ runCommand cs
    ss <- get -- а теперь мы считываем это итоговое состояние
    if line ss == length (T.lines $ input ss) -- и если мы обработали весь входной поток
    then return $  output ss -- то возвращаем результат из output
    else runCommand Next >> runCommands cs -- а если нет, то делаем переход к следующей строке и продолжаем обработку

runCommand :: Command -> State SedState ()
runCommand Print = modify $ \ss -> 
    let newOutput = (T.lines $ output ss) ++ (T.lines $ patternSpace ss)
    in ss {output = T.unlines newOutput }
runCommand Delete = modify $ \ss -> ss { patternSpace  = T.empty }
-- переходим к обработке следующей строки и заодно помещаем ее в patternSpace,
-- где она собственно и обрабатывается
runCommand Next = modify $ \ss -> ss { line = line ss + 1, 
                                       patternSpace = (T.lines $ input ss) !! line ss }

main :: IO ()
-- получает на вход список команд, но их еще надо извлечь
-- из командной строки
main = TIO.interact $ sed [Print, Next, Delete]

