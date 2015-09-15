В целом тем кто читал [TAPL](https://www.cis.upenn.edu/~bcpierce/tapl/) чтение данного поста не рекомендуется,
так же тем кто хочет знать как делать правильно лучше сразу брать TAPL
и читать его. Так же тут не описывается полноценное ООП, для этого
можно посмотреть в сторону [Object Haskell](http://arxiv.org/abs/cs/0509027), где показывается вариант
реализации через гетерогенные списки.

В одном из тредов на [point.im](http://umnik.point.im/vhpti) возник вопрос, каким образом можно
сделать "объект" (состояние и функции связанные с ним) с помощью функций.

Здесь и далее будет использоваться haskell и немного комментариев о
других языках, так же будет использоваться подход идеоматичный для haskell,
другие варианты тоже будут мельком обсуждены.

Итак нам как обычно потребуется немного расширений языка

> {-# LANGUAGE GADTs #-}
> {-# LANGUAGE RankNTypes #-}

> import System.IO

Общая идея заключается в том, чтобы создать автомат (mealy machine), который
на вход получает запросы, а на выходе дает результат (+ новую версию себя).
Рассмотрим сначала простейший вариант:

> newtype M1 a = M1 { runM1 :: forall b . a b -> b }

Тут мы создаем тип обёртку, над замыканием, которое принимает на вход запрос
типа `a b` и отдает результат типа `b`. Таким образом объекты типизируются
видом запросов, которые они могут обрабатывать. Можно записать простой пример
запросов для системы логирования:

> data LogLevel = Debug | Info | Warn | Error deriving (Eq, Show, Ord)

> data LogRequest a where
>   WriteLog :: LogLevel -> String -> LogRequest (IO ())
>   SetLevel :: LogLevel -> LogRequest (M1 LogRequest)
>   GetLevel :: LogRequest LogLevel

> type LogObject = M1 LogRequest

Здесь мы имеем три типа запроса, первый - записал лог, создает действие типа
`IO ()`, т.е. выполняет какой-то эффект, второе - обновляет уровень логирования
возвращая новый обьект из текущего, и третье это чистое действие, получение
уровня логирования из текущего. 

Теперь мы можем создавать различне логгеры, например:

Для того, чтобы не делать копипаст создадим базовый логгер:

> baseLogger :: (LogLevel -> String -> IO ())
>            -> (LogLevel -> LogObject)
>            -> LogLevel
>            -> LogObject
> baseLogger writeLog newLogger lvl = M1 go where
>   go :: LogRequest a -> a -- очень важная строчка делающая компилятор счастливым
>   go (WriteLog l s) 
>      | l >= lvl = writeLog l s
>      | otherwise = return ()
>   go (SetLevel l) = newLogger l
>   go GetLevel     = lvl

И два конкретных

> mkStdErrLogger :: LogLevel -> LogObject
> mkStdErrLogger = baseLogger (\_ s -> hPutStrLn stderr s) mkStdErrLogger

> mkFileLogger :: FilePath -> LogLevel -> LogObject
> mkFileLogger path = baseLogger (\_ s -> appendFile path s) (mkFileLogger path)

Пример использования

```
*Main> let l1 = mkStdErrLogger Warn
*Main> runM1 l1 (WriteLog Info "info")
*Main> runM1 l1 (WriteLog Error "error")
error
*Main> runM1 l1 GetLevel
Warn
*Main> let l2 = runM1 l1 (SetLevel Info)
*Main> runM1 l2 (WriteLog Info "info")
info
```

Что тут неудобно: 

  1. не всегда возвращение нового объекта может быть удобно, есть несколько выходов
     или завернуть в State, или использовать изменяемые поля. Но в последнем случае
     доступы к полям тоже будут в `IO` монаде, заметьте, что сейчас функции, считывающие
     состояние без его изменения чистые.

  2. я не знаю как правильно добавлять наследование в таком подходе, или объекты
     принимающие доп параметры, например, если в FileLogger хочется иметь запрос
     `GetFilePath`, тут надо смотреть статью выше. Наверное в ближайшие дни опишу
     один из вариантов расширения.

Но для многих задач и этого достаточно.

Что можно улучшать, можно, например, разделить все запросы на запросы записи,
чтения и выполнения, например как `data Action = Read | Write | Execute` и пример можно
найти где-то [здесь](https://github.com/YoEight/eventstore/blob/refact/testability/Database/EventStore/Internal/Manager/Subscription/Model.hs#L102-L106).

Абсолютно те же трюки можно провести и в других языках, только вместо pattern-matching
будет диспетчеризация по типу, например куча `if` и проверкой `typeof` и меньше
статических гарантий.

