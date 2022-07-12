import Data.Void
import ReactiveMarkup.App
import ReactiveMarkup.Context
import ReactiveMarkup.Markup
import ReactiveMarkup.Target.Gtk
import ReactiveMarkup.Widget

main :: IO ()
main = do
  pure ()

--   runGtk app

-- app :: App Gtk Void Void
-- app =
--   App
--     { appRender = \_ -> gui,
--       appHandleEvent = \_ -> pure undefined,
--       appInitialState = undefined
--     }

gui :: Markup Gtk Root Void
-- GUIs are complex and take a lot of effort to create.
-- -> What would be the simplest way to create graphical user interfaces?
--
-- Considering the popularity of HTML, it seems that „just writing the components“ is a good approach.

-- gui = text "Hello World"

-- We can also add more complex components that can hold other components.

-- gui =
--     column [
--         text "Simple column",
--         text "to combine components"
--     ]

-- Now we have the same level of expressiveness as HTML. However, this is not enough for even simple gui applications.
-- We need to somehow scale this approach up to allow for:
--     - Reusable components
--     - Changing GUI based on some state
--     - Responding to GUI events

-- Reusable components can be achieved easily because we can simply use the Haskell language features.

-- We have created the same component twice.
-- gui =
--   column
--     [ text "Hello World",
--       text "Hello World"
--     ]

-- We can simplify this easily by binding text "Hello World" to helloWorldText and then using helloWorldText twice.
-- gui =
--   column
--     [ helloWorldText,
--       helloWorldText
--     ]
--   where
--     helloWorldText = text "Hello World"

-- It is also possible to create reusable components with slight differences with the help of functions

-- gui =
--   column
--     [ helloWorldText 0,
--       helloWorldText 3
--     ]
--   where
--     helloWorldText exclamationStrength = text ("Hello World" <> T.replicate exclamationStrength "!")

-- By modelling our components as functions, we can easily create reusable components!

-- Next up, think about how we want to change our GUI based on some state.
-- Let's first say we have some state and want to create our UI
-- based on that without thinking how we can react to state changes.
-- Once again, we can simply use a function of the form: STATE -> UI

-- gui state =
--   column
--     [ helloWorldText 0,
--       helloWorldText 3
--     ]
--   where
--     helloWorldText exclamationStrength = text ("Hello World" <> T.replicate exclamationStrength "!")

-- gui = statefulGui 5

-- statefulGui state = row [text "Current state is: ", string (show state)]

-- The function approach is useful not only for small reusable components,
-- but also can be used to create the gui based on the application state.

-- How do we respont to changes?
-- Traditionally, the developer needs to manually set the state of ui components. This is tedious and error-prone!
-- Instead, we could reevaluate our entire gui function when the state changes.

-- gui = statefulGui 5 -- Gui with state 5
-- gui = statefulGui 10 -- Next gui with state 10

-- This is obviously inefficient, since simple changes result in recomputation of the entire gui!
-- To use this approach, we need some way to recompute only the changed elements.
-- The idea to reduce this problem is the following:
--     Only if some ui component actually uses the state, should it be recomputed.
--     Gui components that stay the same regardless of the state need not be changed.
--     Therefore, we need to keep track of when exactly a ui component uses the state.

-- I borrowed some semantics from FRP. Let's say that "Dynamic a" is something
-- which holds a value of type "a" which can change over time.
-- "Dynamic a" stays the but it's value does not.
-- Now, we create a function: DYNAMIC STATE -> UI
-- We can now keep track of when the value of Dynamic is accessed.

-- counter component for demonstration purposes. Provides a Dynamic Gtk Int which counts upwards
-- gui = counter statefulGui

-- statefulGui :: Dynamic Gtk Int -> Markup Gtk Root Void
-- statefulGui dynamicState =
--   column
--     [ text "Not changed",
--       dynamicMarkup dynamicState $ \state -> string (show state)
--     ]

-- dynamicMarkup allows us to read the state of a Dynamic.
-- It also makes sure that its content updates when the Dynamic value changes.
-- We do not rerender the whole gui. Components that deal with Dynamic values need to make sure themselves
-- that they update correctly.

-- We can now deal with changes, but how do we respond to ui events?
-- Keep in mind that we still want to keep the syntax of: Just write the component you want
-- -> we are rather restricted since all components can only return our ui type

-- Jetpack Compose and React solution:
-- Pass in callbacks that are triggered when events happen

-- gi-gtk-declarative and my solution: Keep track of the allowed events in the markup type.

-- Let's say that a button emits an event of type ButtonClick,

myButton :: Markup Gtk Block () -- ButtonClick is the event type!
myButton = button [] "Click me"

-- We can now add components that can react to events.
-- For example:
mapEventIO' :: (innerEvent -> IO (Maybe outerEvent)) -> Markup Gtk Block innerEvent -> Markup Gtk Block outerEvent
mapEventIO' = mapEventIO

-- You provide a function which takes the inner event and does some IO
-- and/or produces an outer event and the component will
-- take core of aligning the types of the markup correctly.

-- gui =
--   column
--     [ text "Button clicks are printed to console",
--       mapEventIO' (\ButtonClick -> print "Button Clicked" $> Nothing) myButton
--     ]

-- Another option is localState, which makes it possible for components
-- to have internal state and change that state based on gui event

-- Given an initial state and a function which takes the inner event and
-- produces new state and/or produces an outer event,
-- localState will allow you to use the state when creating your gui.

-- localState' ::
--   (state -> innerEvent -> (Maybe state, Maybe outerEvent)) ->
--   state ->
--   (Dynamic Gtk state -> Markup Gtk Root innerEvent) ->
--   Markup Gtk Root outerEvent
-- localState' = localState

-- gui = localState' update 0 $ \dynState ->
--   column
--     [ row [text "You have clicked the button: ", dynamicMarkup dynState (\i -> string (show i))],
--       button "Add 1"
--     ]
--   where
--     update previousState ButtonClick = (Just (previousState + 1), Nothing)

-- Expressive power is now roughly equivalent with other convential declarative
-- gui frameworks like React or Jetpack Compose.

-- Reactive-Markup components however have the additional property; they do not adhere
-- to a specific implementation;
-- The components are only the specification and there can exist multiple interpretations.
-- Which interpretation is choosen is indicated by the first and second type parameter.

-- The first type parameter of Markup sets the concrete target platform, which is Gtk in this case.
textGtk :: Markup Gtk Block Void
textGtk = text "I am a Gtk label"

-- The type of text however would also allow for other interpretations as long as the choosen implementation
-- can interpret text elements

text' :: Render Words t c => Markup t c Void
text' = text "I am a polymorphic label"

-- This way, you can reuse the same code for different gui frameworks.

-- The second type parameter sets the context. Some gui components you can only use in specific contexts.
bold' :: Markup Gtk Inline Void -> Markup Gtk Inline Void
bold' = bold

-- Only inline markup can be used in conjunction with bold!

-- test :: Markup Gtk Block Void
-- test = bold (row [])
-- Type error. Block markup cannot be made bold!

-- gui = bold (text "Bold text")
-- Ok. Inline markup can be made bold.

-- Another example:
textBlock :: Markup Gtk Block Void
textBlock = text "I am a text in the block context"

textInline :: Markup Gtk Inline Void
textInline = text "I am a text in the inline context! "

-- combinedBlock :: Markup Gtk Block Void
-- combinedBlock = textBlock <> textBlock
-- Type error. You cannot combine block markup.

combinedInline :: Markup Gtk Inline Void
combinedInline = italic textInline <> bold textInline

-- Ok. You can combine inline markup.

gui =
  column
    [ text "You can easily combine inline texts",
      lift $ combinedInline <> combinedInline,
      dropEvents $ button [] "Click me"
    ]
