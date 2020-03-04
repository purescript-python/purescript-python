import Serialize.TaglessPacker
import Data.Text (unpack)
main :: IO ()
main = do
  putStrLn . unpack . runPacker $
      let res :: Serial Lines
          res =
            tfCons "cons2"
              [
                tfCons "cons1" [tfFloat 1.0, tfInt 1]
              , tfCons "cons3" []
              ]
      in res
