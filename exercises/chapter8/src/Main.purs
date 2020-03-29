module Main where

import Prelude hiding (div)

import Affjax (get) as Affjax
import Affjax.ResponseFormat (json) as ResponseFormat
import Data.Argonaut.Core (Json)
import Data.Argonaut.Core (toArray, toObject, toString) as JSON
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.String.NonEmpty as NonEmpty
import Data.Traversable (traverse)
import Debug.Trace (spy)
import Effect (Effect, foreachE)
import Effect.Aff (Aff, error, launchAff_, throwError, try)
import Effect.Class (liftEffect)
import Effect.Console (error, log) as Console
import Foreign.Object (lookup) as Object
import Math (asin)
import Web.DOM (Document) as DOM
import Web.DOM.Document (createElement, toNonElementParentNode) as DOM1
import Web.DOM.Element (Element, setAttribute, setId, toEventTarget, toNode) as DOM2
import Web.DOM.Node (appendChild, parentNode, removeChild, setTextContent) as DOM3
import Web.DOM.NonElementParentNode (getElementById) as DOM4
import Web.Event.Event (Event) as Event
import Web.Event.EventTarget (addEventListener, eventListener) as Event
import Web.HTML (window) as HTML0
import Web.HTML.Event.EventTypes (click) as Event
import Web.HTML.HTMLDocument (body, toDocument,HTMLDocument) as HTML1
import Web.HTML.HTMLElement (toNode,HTMLElement) as HTML3
import Web.HTML.HTMLInputElement (fromElement, value) as HTMLInput
import Web.HTML.Window (document,Window) as HTML
import Web.DOM.Internal.Types(Node) as Web.DOM.Internal.Types


type RedditPost = { title :: String, selftext :: Maybe String, id :: String }

textInput :: DOM.Document -> Effect DOM2.Element
textInput document = do
  element     <- DOM1.createElement "input" document
  DOM2.setAttribute "type" "text" element
  pure element

label :: String -> DOM.Document -> Effect DOM2.Element
label text document = do
  elem <- DOM1.createElement "label" document
  DOM3.setTextContent text (DOM2.toNode elem)
  pure elem

button :: String -> (Event.Event -> Effect Unit) -> DOM.Document -> Effect DOM2.Element
button text onClick document = do
  elem      <- DOM1.createElement "button" document
  _         <- DOM3.setTextContent text (DOM2.toNode elem)
  let target = DOM2.toEventTarget elem
  listener  <- Event.eventListener onClick
  _         <- Event.addEventListener Event.click listener false target
  pure elem

section :: DOM.Document -> Effect DOM2.Element
section document = DOM1.createElement "section" document

div :: DOM.Document -> Effect DOM2.Element
div = DOM1.createElement "div"

ul :: DOM.Document -> Effect DOM2.Element
ul = DOM1.createElement "ul"

li :: DOM.Document -> Effect DOM2.Element
li = DOM1.createElement "li"

p :: DOM.Document -> Effect DOM2.Element
p = DOM1.createElement "p"

link :: String -> String -> DOM.Document -> Effect DOM2.Element
link href text document = do
  elem <- DOM1.createElement "a" document
  DOM3.setTextContent text (DOM2.toNode elem)
  DOM2.setAttribute "href" href elem
  pure elem

post :: RedditPost -> DOM.Document -> Effect DOM2.Element
post redditPost document = do
  container <- div document
  a         <- link ("https://www.reddit.com/r/purescript/comments/" <> redditPost.id) redditPost.title document
  paragraph <- p document
  let text  = fromMaybe "" redditPost.selftext
  DOM3.setTextContent text (DOM2.toNode paragraph)
  let containerNode = DOM2.toNode container
  DOM3.appendChild (DOM2.toNode a) containerNode # void
  DOM3.appendChild (DOM2.toNode paragraph) containerNode # void
  pure container

posts :: Array RedditPost -> DOM.Document -> Effect DOM2.Element
posts redditPosts document = do
  list <- ul document
  DOM2.setId "posts" list
  let listNode = DOM2.toNode list
  foreachE redditPosts \redditPost -> do
    listItem <- li document
    item <- post redditPost document
    DOM3.appendChild (DOM2.toNode item) (DOM2.toNode listItem) # void
    DOM3.appendChild (DOM2.toNode listItem) listNode # void
  pure list

liftEither :: forall a b. String -> Either a b -> Aff b
liftEither errorMessage (Left err) = throwError (error errorMessage)
liftEither _ (Right val) = pure val

liftMaybe :: forall a. String -> Maybe a -> Aff a
liftMaybe errorMessage = maybe (throwError (error errorMessage)) pure

controls :: DOM.Document -> Effect DOM2.Element
controls document = do
  repoLabel   <- label "Subreddit" document
  input       <- textInput document
  _           <- DOM2.setId "subreddit" input
  goButton    <- button "Go" onClick document
  container   <- section document
  let containerNode = DOM2.toNode container
  DOM3.appendChild (DOM2.toNode repoLabel) containerNode # void
  DOM3.appendChild (DOM2.toNode input) containerNode # void
  DOM3.appendChild (DOM2.toNode goButton) containerNode # void
  pure container

    where

      onClick :: Event.Event -> Effect Unit
      onClick event = launchAff_ do
        either <- try doAjax
        case either of
          Right _ -> Console.log "Ajax complete!" # liftEffect
          Left error -> liftEffect $ Console.error ("Error performing ajax request to reddit: " <> (show error))

        where
          doAjax :: Aff Unit
          doAjax = do
            input         <- DOM4.getElementById "subreddit" (DOM1.toNonElementParentNode document)
                              # liftEffect
                              >>= liftMaybe "Couldn't find subreddit text field"
            value         <- traverse HTMLInput.value (HTMLInput.fromElement input)
                              # liftEffect  >>= liftMaybe "Subreddit Element is not an input field"
            neVal         <- NonEmpty.fromString value # liftMaybe "Subreddit is empty"
            redditPosts   <- fetchPosts $ NonEmpty.toString neVal
            postsSection  <- DOM4.getElementById "posts" (DOM1.toNonElementParentNode document)
                              # liftEffect
                              >>= liftMaybe "Couldn't find the 'posts' section"
            let postsSectionNode = DOM2.toNode postsSection
            parent        <- DOM3.parentNode (DOM2.toNode postsSection)
                              # liftEffect
                              >>= liftMaybe "Couldn't find the parent of the 'posts' section"
            newPostsElem  <- posts redditPosts document # liftEffect
            DOM3.removeChild postsSectionNode parent # liftEffect # void
            DOM3.appendChild (DOM2.toNode newPostsElem) parent # liftEffect # void


      fetchPosts :: String -> Aff (Array RedditPost)
      fetchPosts subreddit = do
        let url = "https://www.reddit.com/r/" <> subreddit <> "/new.json"
        json <- Affjax.get ResponseFormat.json url <#> _.body >>=
                  liftEither "Request to reddit failed to decode"
        liftMaybe "Couldn't properly decode json" (spy "maybeChildren" $  maybeChildren (spy "original" json))

        where

           maybeChildren :: Json -> Maybe (Array RedditPost)
           maybeChildren json =
             JSON.toObject (spy "maybeJson" json) >>=
               Object.lookup "data" >>= JSON.toObject >>=
               Object.lookup "children" >>= JSON.toArray >>=
               traverse childToRecord

           childToRecord :: Json -> Maybe RedditPost
           childToRecord json = do
             obj           <- JSON.toObject (spy "json" json)
             dataObj       <- Object.lookup "data" obj >>= JSON.toObject
             title         <- Object.lookup "title" dataObj >>= JSON.toString
             let selftext  =  Object.lookup "selftext" dataObj >>= JSON.toString
             id            <- Object.lookup "id" dataObj >>= JSON.toString
             pure { title, selftext, id }

data Maybe1 (a ::Type)   = Nothing1 | Just1 a 


main :: Effect Unit
main = do
  window        <- HTML0.window :: Effect HTML.Window
  htmlDocument  <- HTML.document window :: Effect HTML1.HTMLDocument
  let document =  HTML1.toDocument htmlDocument :: DOM.Document 
  maybeBody     <- HTML1.body htmlDocument :: Effect (Maybe HTML3.HTMLElement)
  case maybeBody of
    Nothing   -> Console.error "no body element found!"
    Just (body::  HTML3.HTMLElement) -> do
      ctrls <- controls document :: Effect DOM2.Element
      let bodyNode = (HTML3.toNode :: HTML3.HTMLElement -> Web.DOM.Internal.Types.Node)(body::  HTML3.HTMLElement) 
      DOM3.appendChild (DOM2.toNode (ctrls :: DOM2.Element)) bodyNode # void
      postsList :: DOM2.Element <- posts [] document
      DOM3.appendChild (DOM2.toNode postsList) bodyNode # void
      


