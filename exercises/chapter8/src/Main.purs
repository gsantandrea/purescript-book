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
import Web.DOM.Document (createElement, toNonElementParentNode) as Web.DOM.Document
import Web.DOM.Element (Element, setAttribute, setId, toEventTarget, toNode) as Web.DOM.Element
import Web.DOM.Node (appendChild, parentNode, removeChild, setTextContent) as Web.DOM.Node
import Web.DOM.NonElementParentNode (getElementById) as Web.DOM.NonElementParentNode
import Web.Event.Event (Event) as Event
import Web.Event.EventTarget (addEventListener, eventListener) as Event
import Web.HTML (window) as Web.HTML
import Web.HTML.Event.EventTypes (click) as Event
import Web.HTML.HTMLDocument (body, toDocument,HTMLDocument) as Web.HTML.HTMLDocument
import Web.HTML.HTMLElement (toNode,HTMLElement) as Web.HTML.HTMLElement
import Web.HTML.HTMLInputElement (fromElement, value) as HTMLInput
import Web.HTML.Window (document,Window) as Web.HTML.Window
import Web.DOM.Internal.Types(Node) as Web.DOM.Internal.Types


type RedditPost = { title :: String, selftext :: Maybe String, id :: String }

textInput :: DOM.Document -> Effect Web.DOM.Element.Element
textInput document = do
  element     <- Web.DOM.Document.createElement "input" document
  Web.DOM.Element.setAttribute "type" "text" element
  pure element

label :: String -> DOM.Document -> Effect Web.DOM.Element.Element
label text document = do
  elem <- Web.DOM.Document.createElement "label" document
  Web.DOM.Node.setTextContent text (Web.DOM.Element.toNode elem)
  pure elem

button :: String -> (Event.Event -> Effect Unit) -> DOM.Document -> Effect Web.DOM.Element.Element
button text onClick document = do
  elem      <- Web.DOM.Document.createElement "button" document
  _         <- Web.DOM.Node.setTextContent text (Web.DOM.Element.toNode elem)
  let target = Web.DOM.Element.toEventTarget elem
  listener  <- Event.eventListener onClick
  _         <- Event.addEventListener Event.click listener false target
  pure elem

section :: DOM.Document -> Effect Web.DOM.Element.Element
section document = Web.DOM.Document.createElement "section" document

div :: DOM.Document -> Effect Web.DOM.Element.Element
div = Web.DOM.Document.createElement "div"

ul :: DOM.Document -> Effect Web.DOM.Element.Element
ul = Web.DOM.Document.createElement "ul"

li :: DOM.Document -> Effect Web.DOM.Element.Element
li = Web.DOM.Document.createElement "li"

p :: DOM.Document -> Effect Web.DOM.Element.Element
p = Web.DOM.Document.createElement "p"

link :: String -> String -> DOM.Document -> Effect Web.DOM.Element.Element
link href text document = do
  elem <- Web.DOM.Document.createElement "a" document
  Web.DOM.Node.setTextContent text (Web.DOM.Element.toNode elem)
  Web.DOM.Element.setAttribute "href" href elem
  pure elem

post :: RedditPost -> DOM.Document -> Effect Web.DOM.Element.Element
post redditPost document = do
  container <- div document
  a         <- link ("https://www.reddit.com/r/purescript/comments/" <> redditPost.id) redditPost.title document
  paragraph <- p document
  let text  = fromMaybe "" redditPost.selftext
  Web.DOM.Node.setTextContent text (Web.DOM.Element.toNode paragraph)
  let containerNode = Web.DOM.Element.toNode container
  Web.DOM.Node.appendChild (Web.DOM.Element.toNode a) containerNode # void
  Web.DOM.Node.appendChild (Web.DOM.Element.toNode paragraph) containerNode # void
  pure container

posts :: Array RedditPost -> DOM.Document -> Effect Web.DOM.Element.Element
posts redditPosts document = do
  list <- ul document
  Web.DOM.Element.setId "posts" list
  let listNode = Web.DOM.Element.toNode list
  foreachE redditPosts \redditPost -> do
    listItem <- li document
    item <- post redditPost document
    Web.DOM.Node.appendChild (Web.DOM.Element.toNode item) (Web.DOM.Element.toNode listItem) # void
    Web.DOM.Node.appendChild (Web.DOM.Element.toNode listItem) listNode # void
  pure list

liftEither :: forall a b. String -> Either a b -> Aff b
liftEither errorMessage (Left err) = throwError (error errorMessage)
liftEither _ (Right val) = pure val

liftMaybe :: forall a. String -> Maybe a -> Aff a
liftMaybe errorMessage = maybe (throwError (error errorMessage)) pure

controls :: DOM.Document -> Effect Web.DOM.Element.Element
controls document = do
  repoLabel   <- label "Subreddit" document
  input       <- textInput document
  _           <- Web.DOM.Element.setId "subreddit" input
  goButton    <- button "Go" onClick document
  container   <- section document
  let containerNode = Web.DOM.Element.toNode container
  Web.DOM.Node.appendChild (Web.DOM.Element.toNode repoLabel) containerNode # void
  Web.DOM.Node.appendChild (Web.DOM.Element.toNode input) containerNode # void
  Web.DOM.Node.appendChild (Web.DOM.Element.toNode goButton) containerNode # void
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
            input         <- Web.DOM.NonElementParentNode.getElementById "subreddit" (Web.DOM.Document.toNonElementParentNode document)
                              # liftEffect
                              >>= liftMaybe "Couldn't find subreddit text field"
            value         <- traverse HTMLInput.value (HTMLInput.fromElement input)
                              # liftEffect  >>= liftMaybe "Subreddit Element is not an input field"
            neVal         <- NonEmpty.fromString value # liftMaybe "Subreddit is empty"
            redditPosts   <- fetchPosts $ NonEmpty.toString neVal
            postsSection  <- Web.DOM.NonElementParentNode.getElementById "posts" (Web.DOM.Document.toNonElementParentNode document)
                              # liftEffect
                              >>= liftMaybe "Couldn't find the 'posts' section"
            let postsSectionNode = Web.DOM.Element.toNode postsSection
            parent        <- Web.DOM.Node.parentNode (Web.DOM.Element.toNode postsSection)
                              # liftEffect
                              >>= liftMaybe "Couldn't find the parent of the 'posts' section"
            newPostsElem  <- posts redditPosts document # liftEffect
            Web.DOM.Node.removeChild postsSectionNode parent # liftEffect # void
            Web.DOM.Node.appendChild (Web.DOM.Element.toNode newPostsElem) parent # liftEffect # void


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
  window        <- Web.HTML.window :: Effect Web.HTML.Window.Window
  htmlDocument  <- Web.HTML.Window.document window :: Effect Web.HTML.HTMLDocument.HTMLDocument
  let document =  Web.HTML.HTMLDocument.toDocument htmlDocument :: DOM.Document 
  maybeBody     <- Web.HTML.HTMLDocument.body htmlDocument :: Effect (Maybe Web.HTML.HTMLElement.HTMLElement)
  case maybeBody of
    Nothing   -> Console.error "no body element found!"
    Just (body::  Web.HTML.HTMLElement.HTMLElement) -> do
      ctrls <- controls document :: Effect Web.DOM.Element.Element
      let bodyNode = (Web.HTML.HTMLElement.toNode :: Web.HTML.HTMLElement.HTMLElement -> Web.DOM.Internal.Types.Node)(body::  Web.HTML.HTMLElement.HTMLElement) 
      Web.DOM.Node.appendChild (Web.DOM.Element.toNode (ctrls :: Web.DOM.Element.Element)) bodyNode # void
      postsList :: Web.DOM.Element.Element <- posts [] document
      Web.DOM.Node.appendChild (Web.DOM.Element.toNode postsList) bodyNode # void
      


