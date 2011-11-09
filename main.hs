import Yesod.Default.Config (fromArgs)
import Yesod.Default.Main   (defaultMain)
import Application          (withTodoHS)

main :: IO ()
main = defaultMain fromArgs withTodoHS