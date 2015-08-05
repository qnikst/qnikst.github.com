В посте [http://ndtimofeev.github.io/ru/posts/2015-08-02-scope.html](http://ndtimofeev.github.io/ru/posts/2015-08-02-scope.html)
ставится вопрос как решить задачу с выделением многих объектов в "Ресурсо-подобном" окружениии.

Попробуем решить данную задачу. Естественно такую задачу можно решить генераций
кода и TH, но это путь настоящего лиспера. В то время как Haskell язык с продвинутой
системой типов, настолько, что типы могут писать за нас. Вот это и попробуем продемострировать.

Сначала включим немного интересных расширений.

> {-# LANGUAGE TypeOperators #-}
> {-# LANGUAGE GeneralizedNewtypeDeriving #-}
> {-# LANGUAGE DataKinds #-}
> {-# LANGUAGE KindSignatures #-}
> {-# LANGUAGE GADTs #-}
> {-# LANGUAGE RankNTypes #-}

Для решения нашей задачи мы пострим список на уровне типов, тут он будет
напоминать тип из пакета [vinyl](https://hackage.haskell.org/package/vinyl),
так же гетерогенные списки и похожие стуктуры можно найти в пакетах
[HList](https://hackage.haskell.org/package/HList), [fixed-vector-hetero](https://hackage.haskell.org/package/fixed-vector-hetero)
и других.

Тут мы пишем тип сами, потому, что это просто, и для данной задачи не требуются
какие либо дополнительные свойства. Для этого мы создаем GADT структуру данных,
тут можно было бы воспользоваться и type family, но в этом случае возникли бы
сложности связанные с их неинъективностью.

> data H (p :: * -> *) (c::[*]) where
>   Nil   :: H p '[]
>   (:.)  :: p a -> H p as -> H p (a ': as)

Здесь важно, что у мы создаем не 1 параметр, в котором типы элементов, определяются
списком на уровне типов (второй параметр), но и дополнительным типов, который
определяет тип элемента, так что элементы списка являются тегами. Это сделано
для упрощения написания необходимой функциональность, но может быть легко расширено,
если требуется.

Выпишем тип для регионов, которые тут невложенные, что важно, т.к. иначе
можно было бы сделать приведение ресурсов к внутреннему региону, как сделано
в [regions](https://hackage.haskell.org/package/regions-0.11/docs/Control-Monad-Trans-Region.html#g:4).

> newtype Scope s c = Scope { unScope :: IO c } -- destructor should be private
>    deriving (Functor, Applicative, Monad)

Конструктор экспортировать не следует, т.к. пользователю не нужно давать возможность
доставать IO действие. Фантомная переменная `s` нужна для того, чтобы переменные
аллоцированные внутри scope не могли из него выйти

Теперь запишем типы используемые в задаче:

> data DeviceHnd a   = DeviceHnd String deriving (Show, Eq)
> data StaticHnd s a = StaticHnd String deriving (Show, Eq)

Для аллокации и деаллокации ресурсов предлагается использовать следующие функции:

> alloc0 :: DeviceHnd a -> Scope s (StaticHnd s a)
> alloc0 (DeviceHnd s)= Scope $ const (StaticHnd s) <$> putStrLn ("allocating " ++ s)

> dealloc0 :: StaticHnd s a -> Scope s ()
> dealloc0 (StaticHnd s) = Scope $ putStrLn ("deallocating " ++ s)

В задаче ставился, вопрос, как сразу выделить много ресурсов, и благодаря
информации о типе объектов в гетероненном списке, мы может легко это сделать.

> alloc1 :: H DeviceHnd ls -> Scope s (H (StaticHnd s) ls)
> alloc1 Nil = return Nil
> alloc1 (a :. as) = (:.) <$> alloc0 a <*> alloc1 as

> dealloc1 :: H (StaticHnd s) ls -> Scope s ()
> dealloc1 Nil = return ()
> dealloc1 (a :. as) = dealloc0 a >> dealloc1 as

Тут важную роль играет первый параметр типа гетерогенного списка `p`, благодаря
этой информации мы можем применять функции `*0` при проходе списка. Если бы мы
это не сделали, то пришлось бы ещё реализовавывать ad-hoc полиморфизм добавляя
классы-типов и код был бы чуть-чуть более громоздким.

Теперь мы можем реализовать искомый тип:

> withResources :: H DeviceHnd ls -> (forall s . H (StaticHnd s) ls -> Scope s b) -> IO b
> withResources ls f = unScope $ do
>    h <- alloc1 ls
>    v <- f h
>    dealloc1 h
>    return v

> test = withResources ((DeviceHnd "1"::DeviceHnd Int) :. ((DeviceHnd "2"::DeviceHnd String):.Nil)) $
>         \(dev1:.(dev2:.Nil)) -> do
>            useInt dev1
>            useString dev2
>  where
>   useInt :: StaticHnd s Int -> Scope s ()
>   useInt _ = Scope $ putStrLn "int"
>   useString :: StaticHnd s String -> Scope s ()
>   useString _ = Scope $ putStrLn "String"

И проверить:

> *Main> test
> allocating 1
> allocating 2
> int
> String
> deallocating 1
> deallocating 2

Все ли это? Нет, во-первых к созданию ресурсов нужно добавить поддержку асинхронных исключений
и выделять ресурсы используя `bracket` или одну из библиотек `Regions` или `resourcet`, в этом
случае необходимость использования масс-выделения может исчезнуть. С другой стороны, в `alloc1`
мы можем перед аллокацией ресурсов запрашивать все устройства через выделенный объект, что может
спасти от возможных deadlock-ов, в случае, если два разных потока одновременно аллоцируют
ресурсы в "плохом" порядке.

