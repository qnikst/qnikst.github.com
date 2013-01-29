----
author: qnikst
----
Создание сайта на Yesod/Haskell

by qnikst on 2012-05-29

В данном посте описывается создание очень простого сайта на haskell движке yesod на примере цитатника форума и канала gentoo.ru.
Подготовка к соданию

Для создания сайта необходим сам фреймвок yesod, который может быть установлен с помощью утилиты cabal. В случае если Вы являетесь счасливым обладателем дистрибутива gentoo, то можно установить yesod из haskell-overlay.

# layman -a haskell
# emerge -av yesod

Заголовка сайта создаётся с помощью простой утилиты yesod:

qnikst@qwork ~/workspace/myself $ yesod init 
Welcome to the Yesod scaffolder.
...

Будет задано несколько вопросов о названии проета, авторе, лицензии и типе используемого движка базы данных. На основе этой информации будет создана заготовка

Результатом будет созданный шаблон сайта:

  \
  |- Application.hs  -- модуль приложения
  |- devel.hs        -- вспомогательный файл для запуска приложеия в режиме отладки
  |- deploy          -- каталог для деплоймента с помощью heroku
  |- Foundation.hs   -- Основной файл проекта, где задаются типы данных приложения
  |- Import.hs       -- Вспомогательный файл, для того, чтобы не упростить импорт 
  |                     функций в другие файлы
  |- main.hs         -- вспомогательны файл для запуска приложения в нормальном
  |                     режиме
  |- messages        -- файлы переводов I18n
  |- Model.hs        -- файл где будут создавать модели БД
  |- Model           -- каталог для хранения доп моделей (не автосгенерированных 
  |                     и дополнительных функций
  |- Settings.hs     -- файл настроек (и обработки настроек сайта)
  |- Settings        -- доп. настройки
  |- test            -- каталог с тестами системы
  |- templates       -- каталог шаблонов
  |- ygruq.cabal     -- кабал файл описывающий сборку
  |- config          -- конфигурационные файлы
      |- models      -- файл моделей
      |- routes      -- файл путе
      |- setting.yml -- yaml файл насторек проекта
      |- sqlite.yml  -- настройки БД

Естественно для маленького проекта это очень сильное переусложнение структуры и можно сделать гораздо более простое yesod приложение состоящее из одного файла, но поскольку хочется разобраться в том, как проект должен выглядеть в сложном варианте, то мы рассматриваем всю структуру.

Далее (если используется gentoo) удалим лишние зависимости в cabal файле это yesod-platform, которая лишь для того, чтобы спасать от dependency-hell, но поскольку в gentoo он уже решён, то этой проблемы нет. Так же я временно убрал зависимость от yesod-test поскольку в этом проекте тесты пока не используются.
Настраиваем пути

Официальный мануал: http://www.yesodweb.com/book/routing-and-handlers

Следующем шагом будет создание путей. Yesod позиционируется как REST фреймворк поэтому он спроектирован для использования соотвествующей раскладки, но для чистого html сайта это не очень удобно, поэтому мы сделаем более простою структуру: модель-действие-параметры

Для того, чтобы определиться со структурой выделим основной фунционал сайта:

1. создание цитаты
2. вывод списка цитат
3. вывод конкретной цитаты
4. вывод бездны 
5. вывод rss

И отобразим этот фукционал на структуру сайта

/quote/create         QuoteCreateR          GET POST
/quote/list           QuoteListR            GET
/quote/show/#QuoteId  QuoteShowR            GET
/quote/list/#Int      QuoteListPageR        GET
/quote/abyss/         QuoteAbyssListR       GET
/quote/abyss/process  QuoteAbyssProcessR    POST
/quote/rss        QuoteFeedR                GET

Для каждого пути нужно создать свой метод вида httpMethodTypeName. В путях можно использовать стандартные типы haskell и свои типы если в пути используется такая переменная, то тако аргумент должен быть и у функции обработчика (см. ниже)

Каждый из типов можно использовать для автоматического создания безопасных url, так вызов QuoteCreateR или QuoteListPageR 3 выдаст ссылки на содание цитаты и 3-ю страницу соотвественно.

Далее создадим обработчик Handler/Quote.hs и добавим туда заглушки для методов:

{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}
module Handler.Quote
    where

import Import

postQuoteCreateR :: Handler RepHtml
postQuoteCreateR = undefined

getQuoteCreateR  :: Handler RepHtml
getQuoteCreateR  = undefined

getQuoteListPageR:: Int -> Handler RepHtml
getQuoteListPageR page = undefined

Теперь в файл Application.hs добавим импорт созданного обработчика Handler.Quote:

import Handler.Quote

Создание моделей

Официальная документация: http://www.yesodweb.com/book/persistent

Теперь в файле ‘config/model’ создаём модели для сайта. Добавляем туда модель для цитаты, с полями автор цитаты, отправитель, источник, подтверждающая ссылка, текст цитаты, время отправления цитаты, является ли цитата подтвержденной.

  Quote
    author Text
    sender Text
    source Text
    prooflink Text
    text   Text
    timestamp UTCTime default=now()
    is_approved  Bool default=False

Для того, чтобы это дело собралось необходимо в файл моделей добавить импорт модуля Data.Time в файле Model.hs и в cabal файл нужно добавить зависимость от пакета time.
Проверка

Теперь проект можно собирать. .. $ cabal-dev configure && yesod –dev devel

После сборки проект можно посмотреть по адресу http://localhost:3000/.
Непосредственное создание сайта
Вывод списка цитат

Обработаем QuotesListR для вывода всех принятых цитат. Для этого мы должны загрузить все цитаты удовлетворяющие требованию и показать их.

Поскольку цитаты нужно показывать в нескольких местах, то логично для их представления сделать шаблон (showQuote), который на вход получает цитату, которую нужно показать и возвращает виджет, которы может быть включен в вёрстку.

-- file: Handler/Quote.hs

showQuote quote = $(whamletFile "templates/quote-show.hamlet")

getQuoteListR :: Handler RepHtml                                                
getQuoteListR = do                                                              
    quotes <- runDB $ selectList [QuoteApproved ==. True] []                       
    defaultLayout $(widgetFile "quote-list") 

и создадим для вывода цитаты:

$forall Entity quoteId quote <- quotes
  <input type=checkbox name=abyss value=#{show $ unKey quoteId}>

и сам виджет цитаты:

-- file: templates/quote-show.hamlet
<div .quote> 
    <pre>#{unTextarea (quoteText quote)}
      \
      \            -- 
      $maybe author <- quoteAuthor quote
        author
      (#{quoteSource quote})
    <a href=#{quoteProoflink quote}>пруфлинк
    \ цитату прислал 
    $maybe sender <- quoteSender quote
      #{sender} 
    $nothing
      анонимный друг 
    (#{showTime (quoteTimestamp quote)})

Таким же образом сделаем и бездну.
Создание формы добавления

Для добавления цитат используем встроенные в yesod формы, тут удобнее всего будет использовать ApplicativeForm. В данном случае форма представляет собой функцию, которая возвращает результирующий тип данных в случае успеха список ошибок в случае если что-то не так. Ниже приведена используемая форма. Тут aopt используется для необязательного параметра, areq для обязательного, а pure подставляет значение не создавая элемента формы.

quoteAForm :: UTCTime -> Maybe Quote -> AForm App App Quote
quoteAForm time mquote = Quote 
            <$> aopt textField "Автор"       (quoteAuthor <$> mquote)
            <*> aopt textField "Отправитель" (quoteSender <$> mquote)
            <*> areq (selectFieldList sources) "Место" (quoteSource <$> mquote)
            <*> areq urlField "Ссылка"      (quoteProoflink <$> mquote)
            <*> areq textareaField "Текст"   (quoteText   <$> mquote)
            <*> pure time 
            <*> pure False
        where 
            sources :: [(Text,LinkSource)]
            sources = [("gentoo.ru",Gru)
                      ,("gentoo@conference.gentoo.ru",GruConf)
                      ,("gentoo@conference.jabber.ru",JRuConf)
                      ,("gentoo-user-ru@lists.gentoo.org",RuMail)
                      ,("ru.gentoo-wiki.com",RuWiki)
                      ,("Другое",OtherSource)]

Шаблон формы. Важно, что мы создаёт два варинта отправки формы только для просмотра и для добавления и это должно быть обработано.

<form method=post action=@{QuoteCreateR} enctype=#{enctype}>
   <table>
     ^{formWidget}
   <input type=submit name=add value=Добавить>
   <input type=submit name=show value=Посмотреть>

Показ формы на странице создания:

getQuoteCreateR  :: Handler RepHtml
getQuoteCreateR  = do
    let pageType = Create
    time <- liftIO $ zonedTimeToUTC <$> getZonedTime
    (formWidget,enctype) <- generateFormPost $ renderTable $ quoteAForm time Nothing
    defaultLayout $ do
        $(widgetFile "quote-list-wrapper")

Показ формы на другой странице

quoteCreate formWidget enctype = $(widgetFile "quote-create")
anyhanler = do
    time <- liftIO $ zonedTimeToUTC <$> getZonedTime
    (formWidget,enctype) <- generateFormPost $ renderTable $ quoteAForm time Nothing

Обработка формы:

postQuoteCreateR :: Handler RepHtml
postQuoteCreateR = do
    time <- liftIO $ zonedTimeToUTC <$> getZonedTime
    ((result,formWidget),enctype) <- runFormPost $ renderDivs $ quoteAForm time Nothing
    showOnly <- lookupPostParam "show" -- проверям нажато ли только просмотр
    case result of
        FormSuccess quote -> do
            if isJust showOnly 
        then setMessage [shamlet|Просмотр цитаты <strong>цитата не добавлена</strong>|] 
                else do  -- добавляем цитату и редиректим пользователя на её просмотр
                  quoteId  <- runDB (insert quote)
                  setMessage [shamlet|Цитата была успешно добавлена|]
                  createFile
                  toMaster <- getRouteToMaster
                  redirect $ toMaster $ QuoteShowR quoteId
            defaultLayout $ do
                $(widgetFile "quote-show")
                $(widgetFile "quote-create")
        _other -> do -- просто показываем форму со всеми ошибками
            defaultLayout $ do
                $(widgetFile "quote-create")

Логин администратора

В есод встроена система авторизации на основе разных провайтеров таких как

    OpenId
    BrowserId
    Database password

Для простоты в данном случае используется browserid. Поскольку на данном сайте все авторизованные пользователи являются администраторами, то необходимо изменить логику по умолчанию (когда новый пользователь автоматически регистрируется).

-- file: Foundation.hs
    getAuthId creds = runDB $ do
        x <- getBy $ UniqueUser $ credsIdent creds
        case x of
            Just (Entity uid _) -> return $ Just uid
            Nothing -> return Nothing

Теперь в обработчиках мы можем добавить “maid <- maybeAuth” для проверки является ли пользователь администратором и “requireAuth” для того, чтобы потребовать авторизацию

Теперь в шаблон ‘quote-list’ добавим необходимые детали

^{pager}
$maybe _ <- maid
  <form action=@{QuoteAbyssProcessR} method=post>
    $forall Entity quoteId quote <- quotes
        <input type=checkbox name=abyss value=#{show $ unKey quoteId}>             
        ^{showQuote quote}
    <div>
      <input type=submit name=delete value=Удалить>
      <input type=submit name=approve value=Опубликовать>
$nothing
  \
    $forall Entity quoteId quote <- quotes
        ^{showQuote quote}

Обработка бездны

В шаблоне выше была вручную создана форма для принятия и удаления цитат, теперь нужно сделать обрабочик, логично, что использование автоматической формы в yesod не является хорошим решением и проще обработать данные вручную:

postQuoteAbyssProcessR :: Handler RepHtml
postQuoteAbyssProcessR = do
    _ <- requireAuth                -- требуем авторизации
                        -- получаем список параметров из запроса
    toDelete  <- lookupPostParam "delete"
    toApprove <- lookupPostParam "approve"
    qLst      <- lookupPostParams "abyss"   -- тут параметром много (?abyss=1&abyss=2...)
    let qlst' = map (Key . read . unpack) qLst  -- получаем список ключей (можно было проще)
    case (toDelete,toApprove) of        -- получаем выбранное действие
        (Just _, Nothing) -> delete' qlst'
        (Nothing, Just _) -> approve' qlst'
        (_,_)             -> do
            setMessage [shamlet|Invalid command|]
    toMaster <- getRouteToMaster        -- редиректим пользователя
    redirect $ toMaster QuoteAbyssListR
    where
        delete' list = do
            runDB $ mapM delete list
            setMessage [shamlet|Цитаты были удалены|]
        approve' list = do  
            runDB $ mapM (flip update [QuoteApproved =. True]) list
            setMessage [shamlet|Цитаты были опубликованы|]

Дополнительные возможности
RSS

Yesod поддерживает автоматическое создание rss лент в виде RSS и ATOM в зависимости от заголовков посылаемых клиентом.

getQuoteFeedR :: Handler RepAtomRss
getQuoteFeedR = do 
    quotes <- runDB $ selectList [ QuoteApproved==.True]
                                 [ Desc QuoteTimestamp
                                 , LimitTo 100
                                 ]
    newsFeed Feed 
        { feedTitle = "Gentoo.ru Quotes"
        , feedLinkSelf = QuoteFeedR
        , feedLinkHome = HomeR 
        , feedDescription = "Description"
        , feedLanguage = "ru"
        , feedUpdated  = (quoteTimestamp $ entityVal $ head $ quotes)
        , feedEntries  =  (map toFeed quotes)
        }
    where
        toFeed (Entity i q) = 
            FeedEntry 
                { feedEntryLink    = QuoteShowR i
                , feedEntryUpdated = quoteTimestamp q
                , feedEntryTitle   = ("Цитата №" `append` (toPathPiece i))
                , feedEntryContent = (toHtml $ quoteText q)
                }

Создание архива

Для создания архива используется потоковое api получения данных из базы, архивирования и записи в файл, таким образом можно гарантировать, то что программа не начнёт использовать слишком много ресурсов

createFile = do
    (qC, vC) <- runDB $ do
        a <- count [QuoteApproved ==. True]
        b <- selectFirst [] [ Desc TarballTimestamp]
        return (a,maybe 0 (tarballNumquotes.entityVal) b)
    when (qC>vC+32) $ do
         time <- liftIO $ getCurrentTime
         let y = getL year time
             m = getL month time
             d = getL day time
             s = printf "%02d%02d%02d" y m d
             f = "fortune-mod-gentoo-ru-"++s++".gz"
             t = Tarball 
                    { tarballFilename  = S.pack f
                    , tarballNumquotes = qC
                    , tarballTimestamp = time
                    }
         runDB $ do 
            insert t
            runResourceT $ selectSource [QuoteApproved ==. True] []
                $= CL.map toText
                $ CL.map encodeUtf8
                =$ gzip 
                =$ sinkFile ("static/files/"++f)


toText :: Entity Quote -> Text
toText (Entity _ quote) = 
    S.concat [ (unTextarea $ quoteText quote)
             , "\n"
             , "   -- "
             , (maybe "" id $ quoteAuthor quote)
             , " "
             , (S.pack $ show $ quoteSource quote)
             , "\n%\n"
             ]
404
===

**There isn't a GitHub Page here.**

*Are you trying to publish one?* We'll send you an email when your page
has been built. It may take up to ten minutes until your page is
available.

`Read the full documentation <http://pages.github.com/>`_ to learn how
to set up **GitHub Pages**
 for your repository, organization, or user account.

-  `Contact Support <https://github.com/contact>`_
-  `Status Site <http://status.github.com>`_
-  `@github <http://twitter.com/github>`_

.. figure:: data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAG0AAAAwCAYAAAAb6PR/AAAAGXRFWHRTb2Z0d2FyZQBBZG9iZSBJbWFnZVJlYWR5ccllPAAAAyJpVFh0WE1MOmNvbS5hZG9iZS54bXAAAAAAADw/eHBhY2tldCBiZWdpbj0i77u/IiBpZD0iVzVNME1wQ2VoaUh6cmVTek5UY3prYzlkIj8+IDx4OnhtcG1ldGEgeG1sbnM6eD0iYWRvYmU6bnM6bWV0YS8iIHg6eG1wdGs9IkFkb2JlIFhNUCBDb3JlIDUuMC1jMDYwIDYxLjEzNDc3NywgMjAxMC8wMi8xMi0xNzozMjowMCAgICAgICAgIj4gPHJkZjpSREYgeG1sbnM6cmRmPSJodHRwOi8vd3d3LnczLm9yZy8xOTk5LzAyLzIyLXJkZi1zeW50YXgtbnMjIj4gPHJkZjpEZXNjcmlwdGlvbiByZGY6YWJvdXQ9IiIgeG1sbnM6eG1wPSJodHRwOi8vbnMuYWRvYmUuY29tL3hhcC8xLjAvIiB4bWxuczp4bXBNTT0iaHR0cDovL25zLmFkb2JlLmNvbS94YXAvMS4wL21tLyIgeG1sbnM6c3RSZWY9Imh0dHA6Ly9ucy5hZG9iZS5jb20veGFwLzEuMC9zVHlwZS9SZXNvdXJjZVJlZiMiIHhtcDpDcmVhdG9yVG9vbD0iQWRvYmUgUGhvdG9zaG9wIENTNSBNYWNpbnRvc2giIHhtcE1NOkluc3RhbmNlSUQ9InhtcC5paWQ6NjdCQkMxMDhEQjI4MTFFMDk4REM4M0Q4MjE0NzE1RkUiIHhtcE1NOkRvY3VtZW50SUQ9InhtcC5kaWQ6NjdCQkMxMDlEQjI4MTFFMDk4REM4M0Q4MjE0NzE1RkUiPiA8eG1wTU06RGVyaXZlZEZyb20gc3RSZWY6aW5zdGFuY2VJRD0ieG1wLmlpZDoyMjMxMjA2QkRCMjYxMUUwOThEQzgzRDgyMTQ3MTVGRSIgc3RSZWY6ZG9jdW1lbnRJRD0ieG1wLmRpZDoyMjMxMjA2Q0RCMjYxMUUwOThEQzgzRDgyMTQ3MTVGRSIvPiA8L3JkZjpEZXNjcmlwdGlvbj4gPC9yZGY6UkRGPiA8L3g6eG1wbWV0YT4gPD94cGFja2V0IGVuZD0iciI/PhqQs8QAAArlSURBVHja7FwJY9q4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
   :align: center
   :alt: 


